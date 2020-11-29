Unit wp_native;


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

(*

format changes

1 -> 2:
  Move image data from being text of image directly to being in the data element

*)

Uses
  SysUtils, Vcl.Graphics, Vcl.Imaging.PNGImage, ZLib, Contnrs,
  fsl_utilities, fsl_crypto, fsl_stream, fsl_collections, fsl_xml,
  wp_graphics, wp_types, wp_document, wp_working, wp_format, wp_imaging;

Type
  TWPNativeReaderAllowedItem = (WPNativeReaderAllowedItemStyle, WPNativeReaderAllowedItemAnnotations, WPNativeReaderAllowedItemAttachments, WPNativeReaderAllowedItemDocument,
    WPNativeReaderAllowedItemSection, WPNativeReaderAllowedItemParagraph, WPNativeReaderAllowedItemField,
    WPNativeReaderAllowedItemText, WPNativeReaderAllowedItemTable, WPNativeReaderAllowedItemRow,
    WPNativeReaderAllowedItemCell, WPNativeReaderAllowedItemLineBreak, WPNativeReaderAllowedItemImage, WPNativeReaderAllowedItemBreak);

  TWPNativeReaderAllowedItems = Set Of TWPNativeReaderAllowedItem;


Const
  NAMES_NATIVEREADERALLOWEDITEM : Array [TWPNativeReaderAllowedItem] Of String =
    (TAG_NAME_STYLE, TAG_NAME_ANNOTATIONS, TAG_NAME_ATTACHMENTS, TAG_NAME_DOCUMENT, TAG_NAME_SECTION, TAG_NAME_PARAGRAPH, TAG_NAME_FIELD, TAG_NAME_TEXT,
     TAG_NAME_TABLE, TAG_NAME_TABLE_ROW, TAG_NAME_TABLE_CELL, TAG_NAME_LINEBREAK, TAG_NAME_IMAGE, TAG_NAME_BREAK);


Type
  TWPNativeReader = Class (TWPReader)
    Private
      FAnnotationMap : TFslStringIntegerMatch;
      FLoadImageHolders : Boolean;
      FActiveExtractor : TFslXMLExtractor;
      FVersion : String;

      Procedure ReadAdornment(oAdornments : TWPDocumentImageAdornments);
      Procedure ReadMap(oMap : TWPImageMap);
      Procedure ReadMapArea(oMap : TWPImageMap);
      Procedure ReadMapCoordinate(oCoords : TWPCoordinateList);
      Procedure ReadHotspot(oPiece : TWPWorkingDocumentPiece); Overload;
      Procedure ReadHotspot(oHotspot : TWPHotspot); Overload;

      Function ResolveAnnotation(sAnnotation : String) : Integer;
    Protected
      Function GetDefaultFontSize : Integer;
      Function GetDefaultFontName : String;

      Function ReadEnumeratedAttribute(Const sValue: String; Const aValues: Array Of String; Const aDefault : Byte): Byte;
      Function ReadFontstateAttribute(Const sValue: String): TWPSFontstate;
      Function ReadCapsStateAttribute(Const sValue: String): TWPSCapsState;
      Function ReadTristateAttribute(Const sValue: String): TWPSTristate;

      Procedure ReadBorder(Const sName: String; oBorder : TWPBorder);

      Procedure ReadFontAttributes(oFont: TWPSFontDetails);
      Procedure ReadParagraphAttributes(oParagraph : TWPSParagraphDetails);
      Procedure ReadStyleAttributes(oPiece: TWPWorkingDocumentPiece);

      Procedure ReadTableItem(oItem : TWPWorkingDocumentTableItemPiece);

      Procedure ReadContent(oDocument : TWPWorkingDocument; aItems : TWPNativeReaderAllowedItems);
      Procedure ReadLineBreak(oDocument : TWPWorkingDocument);
      Procedure ReadField(oDocument : TWPWorkingDocument);
      Procedure ReadImage(oDocument : TWPWorkingDocument);
      Procedure ReadBreak(oDocument : TWPWorkingDocument);
      Procedure ReadParagraph(oDocument : TWPWorkingDocument);
      Procedure ReadSection(oDocument : TWPWorkingDocument);
      Procedure ReadTable(oDocument : TWPWorkingDocument);
      Procedure ReadTableCell(oDocument : TWPWorkingDocument);
      Procedure ReadTableRow(oDocument : TWPWorkingDocument; oOwner : TWPWorkingDocumentTableRowStartPiece);
      Procedure ReadText(oDocument : TWPWorkingDocument);
      Procedure ReadStyle;
      Procedure ReadAnnotations(oDocument : TWPWorkingDocument);
      Procedure ReadAttachments(oDocument : TWPWorkingDocument);
      Procedure ReadDocument(oDocument : TWPWorkingDocument);

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;

      Property LoadImageHolders : Boolean Read FLoadImageHolders Write FLoadImageHolders;
  End;



Type
  TWPSaveImageEvent = Procedure (oSender : TObject; oBuffer : TFslBuffer; Const sExtension : String; Var sName : String) Of Object;

  TWPNativeWriter = Class (TWPWriter)
    Private
      FActiveFormatter : TFslXMLFormatter;
      FImagesAsReferences : Boolean;

      FTextOpen : Boolean;
      FText : String;
      FTextPiece : TWPWorkingDocumentPiece;

      Procedure OpenText(oText : TWPWorkingDocumentTextPiece);
      Function MatchesText(oText : TWPWorkingDocumentTextPiece) : Boolean;
      Procedure CloseText;

      Procedure SetFontAttributes(oFont : TWPSFontDetails);
      Procedure SetParagraphAttributes(oFormat : TWPSParagraphDetails);
      Procedure SetStyleAttributes(oPiece : TWPWorkingDocumentPiece);
      Procedure SetHotspotAttributes(oPiece : TWPWorkingDocumentPiece); Overload;
      Procedure SetHotspotAttributes(oHotspot : TWPHotspot); Overload;

      Procedure WriteStyle(oStyle : TWPStyle);
      Procedure WriteStyles;

      Procedure WriteBorder(Const sName : String; oBorder : TWPBorder);

      Procedure WriteImageAdornment(oAdornment : TWPDocumentImageAdornment);
      Procedure WriteImageMap(oMap : TWPImageMap);
      Procedure WriteImageMapArea(oArea : TWPImageMapArea);
      Procedure WriteImageMapCoordinate(oCoordinate : TWPCoordinate);
      Procedure SetImageAttributes(oImage : TWPWorkingDocumentImagePiece);
      Function SaveImageToBuffer(oImage : TWPWorkingDocumentImagePiece; oImg : TFslGraphic; oBuffer : TFslBuffer) : String;
      Procedure WriteImageReference(oImage : TWPWorkingDocumentImagePiece; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
      Procedure WriteImageSource(oImage : TWPWorkingDocumentImagePiece; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);

      Procedure WriteTableItemAttributes(oTableItem : TWPWorkingDocumentTableItemPiece);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Override;
      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Override;
      Procedure WriteLineBreak(oLineBreak : TWPWorkingDocumentLineBreakPiece); Override;
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

      Function SupportsNestedRows : Boolean; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property ImagesAsReferences : Boolean Read FImagesAsReferences Write FImagesAsReferences;
  End;





Type
  TWPNativeDocumentReaderAllowedItem = (WPNativeDocumentReaderAllowedItemStyle, WPNativeDocumentReaderAllowedItemDocument,
    WPNativeDocumentReaderAllowedItemSection, WPNativeDocumentReaderAllowedItemParagraph, WPNativeDocumentReaderAllowedItemField,
    WPNativeDocumentReaderAllowedItemText, WPNativeDocumentReaderAllowedItemTable, WPNativeDocumentReaderAllowedItemRow,
    WPNativeDocumentReaderAllowedItemCell, WPNativeDocumentReaderAllowedItemLineBreak, WPNativeDocumentReaderAllowedItemImage, WPNativeDocumentReaderAllowedItemBreak);

  TWPNativeDocumentReaderAllowedItems = Set Of TWPNativeDocumentReaderAllowedItem;


Const
  NAMES_NativeDocumentREADERALLOWEDITEM : Array [TWPNativeDocumentReaderAllowedItem] Of String =
    (TAG_NAME_STYLE, TAG_NAME_DOCUMENT, TAG_NAME_SECTION, TAG_NAME_PARAGRAPH, TAG_NAME_FIELD, TAG_NAME_TEXT,
     TAG_NAME_TABLE, TAG_NAME_TABLE_ROW, TAG_NAME_TABLE_CELL, TAG_NAME_LINEBREAK, TAG_NAME_IMAGE, TAG_NAME_BREAK);


Type
  TWPNativeDocumentReader = Class (TWPReader)
    Private
      FLoadImageHolders : Boolean;
      FActiveExtractor : TFslXMLExtractor;
      FVersion : String;

      Procedure ReadMap(oMap : TWPImageMap);
      Procedure ReadMapArea(oMap : TWPImageMap);
      Procedure ReadMapCoordinate(oArea : TWPImageMapArea);
      Procedure ReadHotspot(oPiece : TWPWorkingDocumentPiece); Overload;
      Procedure ReadHotspot(oHotspot : TWPHotspot); Overload;

    Protected
      Function GetDefaultFontSize : Integer;
      Function GetDefaultFontName : String;

      Function ReadEnumeratedAttribute(Const sValue: String; Const aValues: Array Of String; Const aDefault : Byte): Byte;
      Function ReadFontstateAttribute(Const sValue: String): TWPSFontstate;
      Function ReadCapsStateAttribute(Const sValue: String): TWPSCapsState;
      Function ReadTristateAttribute(Const sValue: String): TWPSTristate;

      Procedure ReadBorder(Const sName: String; oBorder : TWPBorder);

      Procedure ReadFontAttributes(oFont: TWPSFontDetails);
      Procedure ReadParagraphAttributes(oParagraph : TWPSParagraphDetails);
      Procedure ReadStyleAttributes(oPiece: TWPWorkingDocumentPiece);

      Procedure ReadTableItem(oItem : TWPWorkingDocumentTableItemPiece);

      Procedure ReadContent(oDocument : TWPWorkingDocument; aItems : TWPNativeDocumentReaderAllowedItems);
      Procedure ReadLineBreak(oDocument : TWPWorkingDocument);
      Procedure ReadField(oDocument : TWPWorkingDocument);
      Procedure ReadImage(oDocument : TWPWorkingDocument);
      Procedure ReadBreak(oDocument : TWPWorkingDocument);
      Procedure ReadParagraph(oDocument : TWPWorkingDocument);
      Procedure ReadSection(oDocument : TWPWorkingDocument);
      Procedure ReadTable(oDocument : TWPWorkingDocument);
      Procedure ReadTableCell(oDocument : TWPWorkingDocument);
      Procedure ReadTableRow(oDocument : TWPWorkingDocument; oOwner : TWPWorkingDocumentTableRowStartPiece);
      Procedure ReadText(oDocument : TWPWorkingDocument);
      Procedure ReadStyle;
      Procedure ReadDocument(oDocument : TWPWorkingDocument);

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;

      Property LoadImageHolders : Boolean Read FLoadImageHolders Write FLoadImageHolders;
  End;



Type
  TWPNativeDocumentWriter = Class (TFslObject)
    Private
      FFormatter : TFslXMLFormatter;

      Procedure SetFormatter(Const Value: TFslXMLFormatter);

      Procedure SetFontAttributes(oFont : TWPSFontDetails);
      Procedure SetParagraphAttributes(oFormat : TWPSParagraphDetails);
      Procedure SetStyleAttributes(Const sStyle : String; oFont : TWPSFontDetails);
      Procedure SetHotspotAttributes(o : TWPDocumentObject); Overload;
      Procedure SetHotspotAttributes(oHotspot : TWPHotspot); Overload;
      Procedure WriteStyle(oStyle : TWPStyle);
      Procedure WriteStyles(oStyles : TWPStyles);
      Procedure WriteBorder(Const sName : String; oBorder : TWPBorder);
      Procedure WriteImageMap(oMap : TWPImageMap);
      Procedure WriteImageMapArea(oArea : TWPImageMapArea);
      Procedure WriteImageMapCoordinate(oCoordinate : TWPCoordinate);
      Procedure SetImageAttributes(oImage : TWPDocumentImage);
      Function SaveImageToBuffer(oImage : TWPDocumentImage; oImg : TFslGraphic; oBuffer : TFslBuffer) : String;
      Procedure WriteImageSource(oImage : TWPDocumentImage; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
      Procedure WriteTableItemAttributes(oTableItem : TWPDocumentTableItem);

      Procedure WriteBlocks(oBlocks : TWPDocumentBlocks);
      Procedure WriteContents(oContents : TWPDocumentContents);

      Procedure WriteText(oText : TWPDocumentText);
      Procedure WriteImage(oImage : TWPDocumentImage);
      Procedure WriteLineBreak(oLineBreak : TWPDocumentLineBreak);
      Procedure WriteField(oField : TWPDocumentField);
      Procedure WriteParagraph(oParagraph : TWPDocumentParagraph);
      Procedure WriteBreak(oBreak : TWPDocumentBreak);
      Procedure WriteSection(oSection : TWPDocumentSection);
      Procedure WriteTable(oTable : TWPDocumentTable);
      Procedure WriteTableRow(oTableRow : TWPDocumentTableRow);
      Procedure WriteTableCell(oTableCell : TWPDocumentTableCell);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Procedure WriteDocument(oDocument : TWPDocument); Virtual;

      Property Formatter : TFslXMLFormatter Read FFormatter Write SetFormatter;
  End;

Type
  TWPSnapshotReader = Class (TWPReader)
    Protected
      Function ReadEnum(Const sValue : String; Const sNames : Array of String) : Integer;
      Function Attribute(oElement : TMXmlElement; Const sName : String):String;
      Function AttributeInt(oElement : TMXmlElement; Const sName : String):Integer;
      Function AttributeBool(oElement : TMXmlElement; Const sName : String):Boolean;
      Function AttributeReal(oElement : TMXmlElement; Const sName : String):Real;
      Function AttributeColour(oElement : TMXmlElement; Const sName : String):TColour;
      Function AttributeTriState(oElement : TMXmlElement; Const sName : String):TWPSTriState;
      Function AttributeWPSFontState(oElement : TMXmlElement; Const sName : String):  TWPSFontState;
      Function AttributeWPSCapsState(oElement : TMXmlElement; Const sName : String):  TWPSCapsState;
      Function AttributeWPSParagraphBulletType(oElement : TMXmlElement; Const sName : String):  TWPSParagraphBulletType;
      Function AttributeWPSParagraphNumberType(oElement : TMXmlElement; Const sName : String):  TWPSParagraphNumberType;
      Function AttributeWPSParagraphNumberFormat(oElement : TMXmlElement; Const sName : String):  TWPSParagraphNumberFormat;
      Function AttributeWordProcessorAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorAlignment;
      Function AttributeWordProcessorVerticalAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorVerticalAlignment;
      Function AttributeWordProcessorParagraphAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorParagraphAlignment;
      Function AttributeWordProcessorImageVerticalAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorImageVerticalAlignment;
      Function AttributeWPSParagraphListType(oElement : TMXmlElement; Const sName : String):TWPSParagraphListType;
      Function AttributeWPWorkingDocumentBreakPieceType(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentBreakPieceType;
      Function AttributeFslPenStyle(oElement : TMXmlElement; Const sName : String):TFslPenStyle;
      Function AttributeFslPenEndStyle(oElement : TMXmlElement; Const sName : String):TFslPenEndStyle;
      Function AttributeWPWorkingDocumentTableBorderPolicy(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentTableBorderPolicy;
      Function AttributeWPWorkingDocumentStopType(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentStopType;
      Function AttributeWPWorkingDocumentSectionDisplayTyp(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentSectionDisplayType;

      Procedure ReadParaFormat(oElement : TMXmlElement; oPiece : TWPWorkingDocumentParaPiece);
      Procedure ReadBorder(oElement : TMXmlElement; Const sName : String; oBorder : TWPBorder);

      Procedure ReadContainerDetails(oElement : TMXmlElement; oPiece : TWPWorkingDocumentContainerPiece);
      Procedure ReadPieceDetails(oElement : TMXmlElement; oPiece : TWPWorkingDocumentPiece);

      Procedure ReadPieceText(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceImage(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceFieldStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceFieldStop(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceLineBreak(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceBreak(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPiecePara(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceTableStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceRowStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceCellStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceStop(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadPieceSectionStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);

      Procedure ExpectedName(oElement : TMXmlElement; Const sName : String);

      Procedure ReadStyle(oElement : TMXmlElement; oStyle : TWPStyle);
      Procedure ReadStyles(oElement : TMXmlElement);
      Procedure ReadDocument(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
      Procedure ReadSnapshot(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
    Public
      Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;
  End;

  TWPSnapshotWriter = Class (TFslXmlFormatter)
    Private
      FIds : TObjectList;
      FFilename: String;

    Protected
      Function GetId(oObject : TObject) : String;
      Procedure ProduceData(oData : TFslStringMatch);
      Procedure ProduceParaFormat(oFormat : TWPSParagraphDetails);
      Procedure ProduceBorder(Const sName : String; oBorder : TWPBorder);
      Procedure ProduceRow(oMap: TWPMapRow);
      Procedure ProduceContainer(oMap: TWPMapContainer);

      Procedure ProduceMapObjectOpen(Const sName : String; oMap : TWPMapObject);
      Procedure ProduceMap(oMap : TWPMapItem);
      Procedure ProduceMetrics(oMetrics : TWPMetrics);
      Procedure ProduceFormat(oFont : TWPSFontDetails);
      Procedure ProduceHotspot(oHotspot : TWPHotspot);

      Procedure ProducePieceStart(Const sName : String; oPiece : TWPWorkingDocumentPiece);
      Procedure ProduceContainerPieceStart(Const sName : String; oPiece : TWPWorkingDocumentContainerPiece);
      Procedure ProduceTableItemPieceStart(Const sName : String; oPiece : TWPWorkingDocumentTableItemPiece);
      Procedure ProducePieceClose(Const sName : String; oPiece : TWPWorkingDocumentPiece);

      Procedure ProducePieceText(oPiece : TWPWorkingDocumentTextPiece);
      Procedure ProducePieceImage(oPiece : TWPWorkingDocumentImagePiece);
      Procedure ProducePieceFieldStart(oPiece : TWPWorkingDocumentFieldStartPiece);
      Procedure ProducePieceFieldStop(oPiece : TWPWorkingDocumentFieldStopPiece);
      Procedure ProducePieceLineBreak(oPiece : TWPWorkingDocumentLineBreakPiece);
      Procedure ProducePieceBreak(oPiece : TWPWorkingDocumentBreakPiece);
      Procedure ProducePiecePara(oPiece : TWPWorkingDocumentParaPiece);
      Procedure ProducePieceTableStart(oPiece : TWPWorkingDocumentTableStartPiece);
      Procedure ProducePieceRowStart(oPiece : TWPWorkingDocumentTableRowStartPiece);
      Procedure ProducePieceCellStart(oPiece : TWPWorkingDocumentTableCellStartPiece);
      Procedure ProducePieceStop(oPiece : TWPWorkingDocumentStopPiece);
      Procedure ProducePieceSectionStart(oPiece : TWPWorkingDocumentSectionStartPiece);
      Procedure ProducePiece(oPiece : TWPWorkingDocumentPiece);
      Procedure ProduceStyle(oStyle : TWPStyle);
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;
      Procedure Start(Const sCause : String);
      Procedure Stop;
      Procedure ProduceDocument(oDocument : TWPWorkingDocument);
      Procedure ProduceStyles(Const sName : String; oStyles : TWPStyles);

      Property Filename : String Read FFilename Write FFilename;
  End;

 Type
  TFslXMLKnownType = (TFslXMLKnownHeaderType, TFslXMLKnownCommentType, TFslXMLKnownElementType, TFslXMLKnownTextType);
  TFslXMLKnownTypes = Set Of TFslXMLKnownType;


Const
  ALL_XML_KNOWN_TYPE = [TFslXMLKnownHeaderType..TFslXMLKnownTextType];


Implementation

Function AllowedItemsToString(aAllowed : TWPNativeReaderAllowedItems): String; Overload;
Var
  aLoop : TWPNativeReaderAllowedItem;
Begin
  Result := '';
  For aLoop := Low(TWPNativeReaderAllowedItem) To High(TWPNativeReaderAllowedItem) Do
  Begin
    If aLoop In aAllowed Then
      StringAppend(Result, NAMES_NATIVEREADERALLOWEDITEM[aLoop], ',');
  End;
End;


Constructor TWPNativeReader.Create;
Begin
  Inherited;

  FAnnotationMap := TFslStringIntegerMatch.Create;
End;


Destructor TWPNativeReader.Destroy;
Begin
  FAnnotationMap.Free;

  Inherited;
End;


Procedure TWPNativeReader.Read(oDocument : TWPWorkingDocument);
Begin
  Inherited;

  FActiveExtractor := TFslXMLExtractor.Create(Stream.Link);
  FActiveExtractor.ConsumeOpen(TAG_NAME_STREAM);
  FVersion := FActiveExtractor.Attributes[ATTR_NAME_VERSION];

  Try
    If IsFragment Then
    Begin
      ReadContent(oDocument, [WPNativeReaderAllowedItemStyle, WPNativeReaderAllowedItemDocument,
        WPNativeReaderAllowedItemTable, WPNativeReaderAllowedItemSection, WPNativeReaderAllowedItemParagraph, WPNativeReaderAllowedItemLineBreak,
        WPNativeReaderAllowedItemField, WPNativeReaderAllowedItemText, WPNativeReaderAllowedItemImage, WPNativeReaderAllowedItemBreak]);
    End
    Else
    Begin
      ReadContent(oDocument, [WPNativeReaderAllowedItemStyle, WPNativeReaderAllowedItemDocument]);
    End;

  Finally
    FActiveExtractor.ConsumeClose(TAG_NAME_STREAM);
    FActiveExtractor.Free;
    FActiveExtractor := nil;
  End;

  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;


Function TWPNativeReader.GetDefaultFontSize : Integer;
Begin
  If HasStyles And Styles.HasDefaultStyle Then
    Result := Styles.DefaultStyle.Font.Size
  Else
    Result := 11;
End;


Function TWPNativeReader.GetDefaultFontName : String;
Begin
  If HasStyles And Styles.HasDefaultStyle Then
    Result := Styles.DefaultStyle.Font.Name
  Else
    Result := 'Verdana';
End;


Function TWPNativeReader.ReadEnumeratedAttribute(Const sValue : String; Const aValues : Array Of String; Const aDefault : Byte) : Byte;
Var
  iResult : Integer;
Begin
  iResult := StringArrayIndexOf(aValues, sValue);
  If iResult = -1 Then
    Result := aDefault
  Else
    Result := iResult;
End;


Function TWPNativeReader.ReadTristateAttribute(Const sValue : String) : TWPSTristate;
Begin
  Result := TWPSTriState(ReadEnumeratedAttribute(sValue, NAMES_WPSTRISTATE, Ord(tsUnknown)));
End;


Function TWPNativeReader.ReadFontstateAttribute(Const sValue : String) : TWPSFontstate;
Begin
  Result := TWPSFontstate(ReadEnumeratedAttribute(sValue, NAMES_WPSFONTSTATE, Ord(fsUnknown)));
End;


Function TWPNativeReader.ReadCapsStateAttribute(Const sValue : String) : TWPSCapsstate;
Begin
  Result := TWPSCapsState(ReadEnumeratedAttribute(sValue, NAMES_WPSCAPSSTATE, Ord(fcsUnknown)));
End;
                                                                                    

Procedure TWPNativeReader.ReadStyleAttributes(oPiece: TWPWorkingDocumentPiece);
Begin
  oPiece.Style := FActiveExtractor.Attributes[ATTR_NAME_STYLE];

  ReadFontAttributes(oPiece.Font);
End;


Procedure TWPNativeReader.ReadFontAttributes(oFont : TWPSFontDetails);
Begin
  oFont.Name := FActiveExtractor.Attributes[ATTR_NAME_FONTNAME];
  oFont.Size := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_SIZE], DEF_WORD);
  oFont.Bold := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_BOLD]);
  oFont.Italic := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_ITALIC]);
  oFont.Underline := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_UNDERLINE]);
  oFont.Strikethrough := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_STRIKETHROUGH]);
  oFont.State := ReadFontstateAttribute(FActiveExtractor.Attributes[ATTR_NAME_FONTSTATE]);
  oFont.Foreground := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_FOREGROUND], DEF_COLOUR);
  oFont.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BACKGROUND], DEF_COLOUR);
  oFont.Capitalization := ReadCapsStateAttribute(FActiveExtractor.Attributes[ATTR_NAME_CAPITALIZATION]);
End;


Procedure TWPNativeReader.ReadParagraphAttributes(oParagraph : TWPSParagraphDetails);
Begin
  oParagraph.Align := TWordProcessorParagraphAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_ALIGN], NAMES_WPSPARAGRAPHALIGNMENT, ord(WordProcessorParagraphAlignmentUnknown)));
  oParagraph.BulletType := TWPSParagraphBulletType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BULLETTYPE], NAMES_WPSPARAGRAPHBULLETTYPE, ord(tbUnknown)));
  oParagraph.ListType := TWPSParagraphListType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_LISTTYPE], NAMES_WPSPARAGRAPHLISTTYPE, ord(WPSParagraphListTypeUnknown)));
  oParagraph.NumberType := TWPSParagraphNumberType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_NUMBERTYPE], NAMES_WPSPARAGRAPHNUMBERTYPE, ord(tnUnknown)));
  oParagraph.NumberFormat := TWPSParagraphNumberFormat(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_NUMBERFORMAT], NAMES_WPSPARAGRAPHNUMBERFORMAT, ord(nwUnknown)));
  oParagraph.FixedNumber := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_FIXEDNUMBER], DEF_WORD);
  oParagraph.LeftIndent := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LEFTINDENT], DEF_WORD);
  oParagraph.RightIndent := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_RIGHTINDENT], DEF_WORD);
  oParagraph.MarginBottom := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_BOTTOM], DEF_WORD);
End;


Procedure TWPNativeReader.ReadStyle;
Var
  oStyle : TWPStyle;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_STYLE + '/');

  oStyle := TWPStyle(Styles.New);
  Try
    oStyle.Name := FActiveExtractor.Attributes[ATTR_NAME_STYLE];

    If Not Styles.ExistsByName(oStyle.Name) Then
    Begin
      ReadFontAttributes(oStyle.Font);
      ReadParagraphAttributes(oStyle.Paragraph);
      oStyle.ResetOnNewParagraph := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_RESET_STYLE], False);
      Styles.Add(oStyle.Link);
    End;
  Finally
    oStyle.Free;
  End;
End;

Procedure TWPNativeReader.ReadBorder(Const sName: String; oBorder : TWPBorder);
Var
  oMem : TFslMemoryStream;
Begin
  oBorder.Clear;
  If StringReplace(FActiveExtractor.PeekXml, '/', '') = sName Then
  Begin
    FActiveExtractor.ConsumeOpen;
    oBorder.Defined := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_DEFINED], False);
    oBorder.Fancy := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_FANCY], False);
    oBorder.OuterColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_OUTERCOLOUR], DEF_COLOUR);
    oBorder.OuterColour2 := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_OUTERCOLOUR2], DEF_COLOUR);
    oBorder.Colour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_COLOUR], DEF_COLOUR);
    oBorder.Width := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    oBorder.Style := TFslPenStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_STYLE], ADVPENSTYLE_CODES, Integer(apsSolid)));
    oBorder.LowOuterlimit := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LOW_OUTER], DEF_WORD);
    oBorder.HighOuterLimit := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_HIGH_OUTER], DEF_WORD);
    If FActiveExtractor.PeekIsOpenTag(TAG_NAME_BRUSH) Then
    Begin
      FActiveExtractor.ConsumeOpen;
      oMem := TFslMemoryStream.Create;
      Try
        oMem.Buffer.AsBytes := DecodeBase64(FActiveExtractor.ConsumeTextBody);
        oBorder.BrushImage := TFslBitmapGraphic.Create;
        oMem.Position := 0;
        oBorder.BrushImage.LoadFromStream(oMem);
      Finally
        oMem.Free;
      End;
      FActiveExtractor.ConsumeClose;
      FActiveExtractor.ConsumeClose;
    End;
  End;
End;


Function StringToIntegerOrDefault(Const sValue : String; Const iValue : Integer) : Integer;
Begin
  If StringIsInteger32(sValue) Then
    Result := StringToInteger32(sValue)
  Else
    Result := iValue;
End;


Procedure TWPNativeReader.ReadTable(oDocument : TWPWorkingDocument);
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE);

  oStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.BorderPolicy := TWPWorkingDocumentTableBorderPolicy(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BORDERPOLICY], CODES_TWPWorkingDocumentTableBORDERPOLICY, ord(tbpNone)));
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_TABLE_BACKGROUND], DEF_COLOUR);
    oStart.HorizontalMargin := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_HORIZONTAL_MARGIN], DEF_WORD);
    oStart.VerticalMargin := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_VERTICAL_MARGIN], DEF_WORD);
    oStart.ExpandLastColumn := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_EXPAND_LAST], false);
    ReadTableItem(oStart);
    ReadBorder(TAG_NAME_CENTERHORIZONTALBORDER, oStart.CenterHorizontalBorder);
    ReadBorder(TAG_NAME_CENTERVERTICALBORDER, oStart.CenterVerticalBorder);

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
    ReadTableRow(oDocument, Nil);

  oStop := TWPWorkingDocumentStopPiece.Create(stTable);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE);
End;


Procedure TWPNativeReader.ReadTableItem(oItem : TWPWorkingDocumentTableItemPiece);
Begin
  ReadBorder(TAG_NAME_LEFTBORDER, oItem.LeftBorder);
  ReadBorder(TAG_NAME_RIGHTBORDER, oItem.RightBorder);
  ReadBorder(TAG_NAME_TOPBORDER, oItem.TopBorder);
  ReadBorder(TAG_NAME_BOTTOMBORDER, oItem.BottomBorder);
End;


Procedure TWPNativeReader.ReadTableRow(oDocument : TWPWorkingDocument; oOwner : TWPWorkingDocumentTableRowStartPiece);
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE_ROW);

  oStart := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    ReadHotspot(oStart);
    oStart.Header := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_HEADER], False);
    oStart.BreakBefore := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_BREAK_BEFORE], False);
    oStart.LowerPaddingSize := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LOWER_PADDING_SIZE], 0);
    oStart.LowerPaddingColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_LOWER_PADDING_COLOUR], DEF_COLOUR);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_ROW_BACKGROUND], DEF_COLOUR);
    oStart.Owner := oOwner;

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen And (FActiveExtractor.PeekXML = TAG_NAME_TABLE_CELL) Do
      ReadTableCell(oDocument);

  oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen And (FActiveExtractor.PeekXML = TAG_NAME_TABLE_ROW) Do
      ReadTableRow(oDocument, oStart);

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE_ROW);
End;


Procedure TWPNativeReader.ReadTableCell(oDocument : TWPWorkingDocument);
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE_CELL);

  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    ReadHotspot(oStart);
    oStart.Span := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_SPAN], 1);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_CELL_BACKGROUND], DEF_COLOUR);
    oStart.MarginLeft := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_LEFT], DEF_WORD);
    oStart.MarginRight := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_RIGHT], DEF_WORD);
    oStart.MarginTop := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_TOP], DEF_WORD);
    oStart.MarginBottom := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_BOTTOM], DEF_WORD);
    oStart.VerticalAlignment := TWordProcessorVerticalAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_VERTICALALIGNMENT], NAMES_WORDPROCESSORVERTICALALIGNMENT, ord(VerticalAlignmentTop)));

    If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_WIDTH) Then
      oStart.Width := StrToFloatDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    ReadTableItem(oStart);

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  ReadContent(oDocument, [WPNativeReaderAllowedItemParagraph, WPNativeReaderAllowedItemBreak]);

  oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE_CELL);
End;


Procedure TWPNativeReader.ReadParagraph(oDocument : TWPWorkingDocument);
Var
  oParagraph : TWPWorkingDocumentParaPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_PARAGRAPH);

  oParagraph := TWPWorkingDocumentParaPiece.Create;
  Try
    oParagraph.SpeechMagicDouble := SpeechMagicDouble;
    ReadStyleAttributes(oParagraph);
    ReadParagraphAttributes(oParagraph.Format);

    ReadContent(oDocument, [WPNativeReaderAllowedItemField, WPNativeReaderAllowedItemText, WPNativeReaderAllowedItemImage, WPNativeReaderAllowedItemLineBreak]);

    oDocument.Pieces.Add(oParagraph.Link);
  Finally
    oParagraph.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_PARAGRAPH);
End;


Procedure TWPNativeReader.ReadField;
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  sTemp : String;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_FIELD);

  oStop := TWPWorkingDocumentFieldStopPiece.Create;
  Try
    oStart := TWPWorkingDocumentFieldStartPiece.Create;
    Try
      oStart.Namespace := FActiveExtractor.Attributes[ATTR_NAME_NAMESPACE];
      oStart.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
      oStart.Deletable := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_DELETABLE], False);
      oStart.RawDataAsText := FActiveExtractor.Attributes[ATTR_NAME_DATA];
      oStart.CheckedIndex := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_INDEX], 0);
      oStart.AnnotationId := ResolveAnnotation(FActiveExtractor.Attributes[ATTR_NAME_ANNOTATION]);

      sTemp := FActiveExtractor.Attributes[ATTR_NAME_MASK];
      If (sTemp <> '') Then
        oStart.DataValue['Mask'] := sTemp;

      oStart.FixedFormat := ReadFixedFormatAttribute(FActiveExtractor.Attributes[ATTR_NAME_FIXEDFORMAT]);

      oStart.Width := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
      sTemp := FActiveExtractor.Attributes[ATTR_NAME_VOCABULARY];
      If (sTemp <> '') Then
        oStart.DataValue['List'] := sTemp;

      ReadHotspot(oStart);
      oStart.ReadOnly :=  ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);

      ReadStyleAttributes(oStart);

      oDocument.Pieces.Add(oStart.Link);

      ReadContent(oDocument, [WPNativeReaderAllowedItemText, WPNativeReaderAllowedItemLineBreak, WPNativeReaderAllowedItemImage]);

      oStop.Style := oStart.Style;
      oStop.Font.Assign(oStart.Font);

    Finally
      oStart.Free;
    End;

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_FIELD);
End;


Procedure TWPNativeReader.ReadLineBreak(oDocument : TWPWorkingDocument);
Var
  oBreak : TWPWorkingDocumentLineBreakPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_LINEBREAK + '/');

  oBreak := TWPWorkingDocumentLineBreakPiece.Create;
  Try
    ReadStyleAttributes(oBreak);
    oBreak.AnnotationId := ResolveAnnotation(FActiveExtractor.Attributes[ATTR_NAME_ANNOTATION]);

    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;
End;


Procedure TWPNativeReader.ReadText(oDocument : TWPWorkingDocument);

  Procedure AddText(Const sContent : String);
  Var
    oText : TWPWorkingDocumentTextPiece;
  Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      ReadStyleAttributes(oText);
      oText.AnnotationId := ResolveAnnotation(FActiveExtractor.Attributes[ATTR_NAME_ANNOTATION]);
      oText.DrawnFont := FActiveExtractor.Attributes[ATTR_NAME_DRAWNFONT];

      oText.Content := sContent;

      oDocument.Pieces.Add(oText.Link);
    Finally
      oText.Free;
    End;
  End;

Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TEXT);

  Splitter.Init(FActiveExtractor.ConsumeTextBody);

  While Splitter.More Do
    AddText(Splitter.Next);

  FActiveExtractor.ConsumeClose(TAG_NAME_TEXT);
End;


Procedure TWPNativeReader.ReadBreak(oDocument : TWPWorkingDocument);
Var
  oBreak : TWPWorkingDocumentBreakPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_BREAK + '/');

  oBreak := TWPWorkingDocumentBreakPiece.Create;
  Try
    ReadStyleAttributes(oBreak);
    oBreak.BreakType := TWPWorkingDocumentBreakPieceType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BREAKTYPE], WPWorkingDocumentBreakPIECETYPE_NAMES, ord(btLine)));
    oBreak.Alignment := TWordProcessorAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_ALIGNMENT], NAMES_WPSALIGNMENT, ord(WordProcessorAlignmentUnknown)));
    oBreak.Width := StrToFloatDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 1);
    oBreak.PenColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_PENCOLOUR], clBlack);
    oBreak.PenWidth := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_PENWIDTH], 1);
    oBreak.PenStyle := TFslPenStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_PENSTYLE], ADVPENSTYLE_CODES, ord(apsSolid)));
    oBreak.EndStyle := TFslPenEndStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_PENENDSTYLE], ADVPENENDSTYLE_CODES, ord(apesRound)));
    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;
End;


Procedure TWPNativeReader.ReadImage;
Var
  oImage : TWPWorkingDocumentImagePiece;
  oBuffer : TFslBuffer;
  oSelectionBuffer : TFslBuffer;
  bNoImage : Boolean;
  sSelectionName : String;
  aFormat : TWPImageFormat; // = (ifUnknown, ifJPEG, ifPNG, ifBMP);
  oAttach : TWPWorkingAttachment;
Begin
  bNoImage := StringEquals(FActiveExtractor.PeekXML, TAG_NAME_IMAGE+'/');
  If bNoImage Then
    FActiveExtractor.ConsumeOpen(TAG_NAME_IMAGE+'/')
  Else
    FActiveExtractor.ConsumeOpen(TAG_NAME_IMAGE);

  oImage := TWPWorkingDocumentImagePiece.Create;
  Try
    ReadStyleAttributes(oImage);
    ReadHotspot(oImage);

    oImage.Border := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_BORDERWIDTH], 0);
    oImage.BorderColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BORDERCOLOR], DEF_COLOUR);

    oImage.Height := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_HEIGHT], 0);
    oImage.Width := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    oImage.AnnotationId := ResolveAnnotation(FActiveExtractor.Attributes[ATTR_NAME_ANNOTATION]);
    oImage.FrameIndex := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_FRAMEINDEX], 0);

    oImage.Name := FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF];
    sSelectionName := FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF_SELECTION];
    oImage.VerticalAlignment := TWordProcessorImageVerticalAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_VERTICALALIGNMENT], NAMES_IMAGEVERTICALALIGNMENT, ord(ivaBaseLine)));
    oImage.SizePolicy := TWordProcessorImageSizePolicy(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_SIZEPOLICY], NAMES_IMAGESIZEPOLICY, ord(ImageSizeManual)));
    oImage.TransparentColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_TRANSPARENTCOLOR], DEF_COLOUR);

    aFormat := ifUnknown;
    If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_FORMAT) Then
    Begin
      If FActiveExtractor.Attributes[ATTR_NAME_FORMAT] = 'jpg' Then
        aFormat := ifJPEG
      Else If FActiveExtractor.Attributes[ATTR_NAME_FORMAT] = 'png' Then
        aFormat := ifPNG
      Else If FActiveExtractor.Attributes[ATTR_NAME_FORMAT] = 'pdf' Then
        aFormat := ifPDF;
    End;

    oBuffer := Nil;
    Try
      If oImage.Name <> '' Then
      Begin
        oAttach := oDocument.Attachments.GetById(oImage.Name);
        if (oAttach <> nil) then
        begin
//          if oAttach.mimeType = 'application/pdf' then
//            oImage.Image := TWPPDFGraphic.Create(oAttach.Link)
//          else
            raise EWPException.create('Unknown attachment type '+oAttach.MimeType);
        end
        else If Assigned(OnLoadImage) Then
          OnLoadImage(Self, Context, oImage.Name, oBuffer);
      End
      Else
      Begin
        oImage.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
        oBuffer := TFslBuffer.Create;

        If FVersion <> VERSION_NATIVE_ONE Then
          FActiveExtractor.ConsumeOpen(TAG_NAME_DATA);
        oBuffer.AsAscii := ZDecompressStr(DecodeBase64(FActiveExtractor.ConsumeBody));
//        if FVersion >= VERSION_NATIVE_THREE then
//          oBuffer.AsAscii := ZDecompressStr(oBuffer.AsAscii);
        If FVersion <> VERSION_NATIVE_ONE Then
          FActiveExtractor.ConsumeClose(TAG_NAME_DATA);
      End;


      oSelectionBuffer := Nil;
      Try
        If sSelectionName <> '' Then
        Begin
          If Assigned(OnLoadImage) Then
            OnLoadImage(Self, Context, oImage.Name+'-selection', oSelectionBuffer);
        End
        Else If FActiveExtractor.PeekIsOpenTag(TAG_NAME_SELECTION) Then
        Begin
          oSelectionBuffer := TFslBuffer.Create;
          FActiveExtractor.ConsumeOpen(TAG_NAME_SELECTION);
          oSelectionBuffer.AsBytes := DecodeBase64(FActiveExtractor.ConsumeTextBody);
          if FVersion >= VERSION_NATIVE_THREE then
            oBuffer.AsText := ZDecompressStr(oBuffer.AsBytes);
          FActiveExtractor.ConsumeClose(TAG_NAME_SELECTION);
        End;

        If Assigned(oSelectionBuffer) Then
        Begin
          Try
            LoadImage(oImage, oBuffer, aFormat, True);
          Except
          End;
        End;

      Finally
        oSelectionBuffer.Free;
      End;

      If (FVersion <> VERSION_NATIVE_ONE) And (FActiveExtractor.PeekIsOpenTag(TAG_NAME_MAP)) Then
      Begin
        oImage.HasImageMap := True;
        ReadMap(oImage.ImageMap);
      End
      Else
        oImage.HasImageMap := False;
      If (FVersion <> VERSION_NATIVE_ONE) Then
        While (FActiveExtractor.PeekIsOpenTag(TAG_NAME_ADORNMENT)) Do
          ReadAdornment(oImage.Adornments);

      If Assigned(oBuffer) Then
      Begin
        Try
          LoadImage(oImage, oBuffer, aFormat, False);
        Except
          // well, hard to know what to do. but it can happen, so we just create a pretend image.
          CreateUnloadedImage(oImage);
        End;
      End;

      If Assigned(oBuffer) or Assigned(oImage.Image) Or FLoadImageHolders Then
        oDocument.Pieces.Add(oImage.Link);
    Finally
      oBuffer.Free;
    End;
  Finally
    oImage.Free;
  End;

  If Not bNoImage Then
    FActiveExtractor.ConsumeClose(TAG_NAME_IMAGE);
End;


Procedure TWPNativeReader.ReadSection(oDocument : TWPWorkingDocument);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_SECTION);
  oSection := Nil;

  If Not SuppressSections Then
  Begin
    oSection := TWPWorkingDocumentSectionStartPiece.Create;
    Try
      oSection.Namespace := FActiveExtractor.Attributes[ATTR_NAME_NAMESPACE];
      oSection.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
      oSection.RawDataAsText := FActiveExtractor.Attributes[ATTR_NAME_DATA];
      oSection.DisplayName := FActiveExtractor.Attributes[ATTR_NAME_DISPLAYNAME];
      oSection.Key := FActiveExtractor.Attributes[ATTR_NAME_KEY];
      oSection.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
      oSection.DisplayType := TWPWorkingDocumentSectionDisplayType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_DISPLAYTYPE], NAMES_WPWorkingDocumentSectionDISPLAYTYPE, ord(sdtNone)));
      oSection.Deletable := StringToBoolean(FActiveExtractor.Attributes[ATTR_NAME_DELETABLE]);
      oSection.IsField := StringToBoolean(FActiveExtractor.Attributes[ATTR_NAME_ISFIELD]); 

      ReadStyleAttributes(oSection);

      oDocument.Pieces.Add(oSection.Link);
    Finally
      oSection.Free;
    End;
  End;

  ReadContent(oDocument, [WPNativeReaderAllowedItemParagraph, WPNativeReaderAllowedItemSection, WPNativeReaderAllowedItemTable, WPNativeReaderAllowedItemBreak]);

  If Not SuppressSections Then
  Begin
    oStop := TWPWorkingDocumentStopPiece.Create(stSection);
    Try
      oStop.AssignStyle(oSection);
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_SECTION);
End;


Procedure TWPNativeReader.ReadContent(oDocument : TWPWorkingDocument; aItems : TWPNativeReaderAllowedItems);
Var
  sTag : String;
  aItem : TWPNativeReaderAllowedItem;
  iIndex : Integer;
Begin
  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
  Begin
    sTag := StringStrip(FActiveExtractor.PeekXML, '/');

    iIndex := StringArrayIndexOfSensitive(NAMES_NATIVEREADERALLOWEDITEM, sTag);

    If iIndex < 0 Then
      RaiseError('ReadContent', 'Unhandled content - found ' + sTag + '.');

    aItem := TWPNativeReaderAllowedItem(iIndex);

    If Not (aItem In aItems) Then
      RaiseError('ReadContent', 'Unexpected content - found ' + sTag + ' expecting ' + AllowedItemsToString(aItems));

    Case aItem Of
      WPNativeReaderAllowedItemDocument : ReadDocument(oDocument);
      WPNativeReaderAllowedItemSection : ReadSection(oDocument);
      WPNativeReaderAllowedItemParagraph : ReadParagraph(oDocument);
      WPNativeReaderAllowedItemField : ReadField(oDocument);
      WPNativeReaderAllowedItemLineBreak : ReadLineBreak(oDocument);
      WPNativeReaderAllowedItemText : ReadText(oDocument);
      WPNativeReaderAllowedItemStyle : ReadStyle;
      WPNativeReaderAllowedItemAnnotations : ReadAnnotations(oDocument);
      WPNativeReaderAllowedItemAttachments : ReadAttachments(oDocument);
      WPNativeReaderAllowedItemTable : ReadTable(oDocument);
      WPNativeReaderAllowedItemRow : ReadTableRow(oDocument, Nil);
      WPNativeReaderAllowedItemCell : ReadTableCell(oDocument);
      WPNativeReaderAllowedItemImage : ReadImage(oDocument);
      WPNativeReaderAllowedItemBreak : ReadBreak(oDocument);
    Else
      RaiseError('ReadContent', 'Unhandled content - found ' + sTag + '.');
    End;
  End;
End;


Procedure ReadTokens(oList : TFslStringList; Const sTokens : String);
Var
  i, j: Integer;
Begin
  i := 1;
  While i <= Length(sTokens) Do
  Begin
    j := i;
    While (i <= Length(sTokens)) And (sTokens[i] <> ' ') Do
      Inc(i);
    oList.Add(StringTrimWhitespace(Copy(sTokens, j, i - j + 1)));
    Inc(i);
  End;
End;

Procedure TWPNativeReader.ReadDocument(oDocument : TWPWorkingDocument);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_DOCUMENT);
  ReadTokens(oDocument.AllowedWords, FActiveExtractor.Attributes[ATTR_NAME_ALLOWED_WORDS]);

  If IsFragment Then
  Begin
    ReadContent(oDocument, [WPNativeReaderAllowedItemStyle, WPNativeReaderAllowedItemAnnotations, WPNativeReaderAllowedItemAttachments, WPNativeReaderAllowedItemDocument, WPNativeReaderAllowedItemTable,
      WPNativeReaderAllowedItemSection, WPNativeReaderAllowedItemParagraph, WPNativeReaderAllowedItemField, WPNativeReaderAllowedItemLineBreak,
      WPNativeReaderAllowedItemText, WPNativeReaderAllowedItemImage, WPNativeReaderAllowedItemBreak]);
  End
  Else
  Begin
    ReadContent(oDocument, [WPNativeReaderAllowedItemAnnotations, WPNativeReaderAllowedItemAttachments, WPNativeReaderAllowedItemSection, WPNativeReaderAllowedItemParagraph,
      WPNativeReaderAllowedItemTable, WPNativeReaderAllowedItemBreak]);
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_DOCUMENT);
End;


Procedure TWPNativeReader.ReadMap(oMap : TWPImageMap);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_MAP);
  While FActiveExtractor.PeekIsOpenTag(TAG_NAME_AREA) Do
    ReadMapArea(oMap);
  FActiveExtractor.ConsumeClose(TAG_NAME_MAP);
End;


Procedure TWPNativeReader.ReadMapArea(oMap : TWPImageMap);
Var
  oArea : TWPImageMapArea;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_AREA);
  oArea := TWPImageMapArea.Create;
  Try
    ReadHotspot(oArea);
    While Not FActiveExtractor.PeekIsClose Do
      ReadMapCoordinate(oArea.Coordinates);
    oMap.Areas.Add(oArea.Link);
  Finally
    oArea.Free;
  End;
  FActiveExtractor.ConsumeClose(TAG_NAME_AREA);
End;


Procedure TWPNativeReader.ReadMapCoordinate(oCoords : TWPCoordinateList);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_COORD + '/');
  oCoords.Add(StringToInteger32(FActiveExtractor.Attributes[ATTR_NAME_X]), StringToInteger32(FActiveExtractor.Attributes[ATTR_NAME_Y]));
End;


Procedure TWPNativeReader.ReadHotspot(oPiece: TWPWorkingDocumentPiece);
Begin
  If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_URL) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_KEY) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_TITLE) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_LINKCOLOUR) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_HOVERCOLOUR) Then
    Begin
    oPiece.HasHotspot := True;
    ReadHotspot(oPiece.Hotspot);
    End;
End;


Procedure TWPNativeReader.ReadHotspot(oHotspot : TWPHotspot);
Begin
  oHotspot.URL := FActiveExtractor.Attributes[ATTR_NAME_URL];
  oHotspot.Title := FActiveExtractor.Attributes[ATTR_NAME_TITLE];
  oHotspot.Key := FActiveExtractor.Attributes[ATTR_NAME_KEY];
  oHotspot.LinkColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_LINKCOLOUR], DEF_COLOUR);
  oHotspot.HoverColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_HOVERCOLOUR], DEF_COLOUR);
End;



function TWPNativeReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAnnotationMap.sizeInBytes);
  inc(result, FActiveExtractor.sizeInBytes);
  inc(result, (FVersion.length * sizeof(char)) + 12);
end;

Constructor TWPNativeWriter.Create;
Begin
  Inherited;

End;


Destructor TWPNativeWriter.Destroy;
Begin

  Inherited;
End;


Procedure TWPNativeWriter.Initialise;
Begin
  Inherited;

  FActiveFormatter := TFslXMLFormatter.Create;
  FActiveFormatter.HasWhitespace := True;
  FActiveFormatter.Clear;
  FActiveFormatter.Stream := Stream.Link;

  FActiveFormatter.Attributes.Add(ATTR_NAME_XMLNS, NAMESPACE_WP_NATIVE);
  FActiveFormatter.Attributes.Add(ATTR_NAME_VERSION, VERSION_NATIVE_THREE);
  FActiveFormatter.ProduceOpen(TAG_NAME_STREAM);

  FTextOpen := False;

  If EmbedStyles Then
    WriteStyles;
End;


Procedure TWPNativeWriter.Finalise;
Begin
  If FTextOpen Then
    CloseText;

  FActiveFormatter.ProduceClose(TAG_NAME_STREAM);
  FActiveFormatter.Stream := Nil;
  FActiveFormatter.Free;
  FActiveFormatter := nil;

  Inherited;
End;

Function AsTokens(oList : TFslStringList):String;
Var
  oBldr : TFslStringBuilder;
  i : Integer;
Begin
  oBldr := TFslStringBuilder.Create;
  Try
    For i := 0 To oList.Count - 1 Do
    Begin
      If i > 0 Then
        oBldr.Append(' ');
      oBldr.Append(oList[i]);
    End;
    Result := oBldr.ToString;
  Finally
    oBldr.Free;
  End;
End;

Procedure TWPNativeWriter.WriteDocumentStart(oDocument : TWPWorkingDocument);
var
  i : integer;
  b : TBytes;
  sName : String;
Begin
  FActiveFormatter.Attributes.Add(ATTR_NAME_ALLOWED_WORDS, AsTokens(oDocument.AllowedWords));
  FActiveFormatter.ProduceOpen(TAG_NAME_DOCUMENT);
  FActiveFormatter.ProduceOpen(TAG_NAME_ANNOTATIONS);
  For i := 0 to oDocument.AllAnnotations.Count - 1 Do
    if oDocument.AllAnnotations[i].InUse Then
    Begin
      FActiveFormatter.Attributes.Add(ATTR_NAME_ID, IntegerToString(i+1));
      FActiveFormatter.Attributes.Add(ATTR_NAME_NAMESPACE, oDocument.AllAnnotations[i].Owner);
      FActiveFormatter.Attributes.Add(ATTR_NAME_VALUE, oDocument.AllAnnotations[i].Text);
      FActiveFormatter.ProduceTag(TAG_NAME_ANNOTATION);
    End;
  FActiveFormatter.ProduceClose(TAG_NAME_ANNOTATIONS);
  FActiveFormatter.ProduceOpen(TAG_NAME_ATTACHMENTS);
  For i := 0 to oDocument.Attachments.Count - 1 Do
    if oDocument.Attachments[i].InUse Then
    Begin
      FActiveFormatter.Attributes.Add(ATTR_NAME_ID, oDocument.Attachments[i].Id);
      FActiveFormatter.Attributes.Add(ATTR_NAME_MIMETYPE, oDocument.Attachments[i].MimeType);
      FActiveFormatter.Attributes.Add(ATTR_NAME_EXTENSION, oDocument.Attachments[i].Extension);

      if ImagesAsReferences then
      begin
        sName := oDocument.Attachments[i].Id;
        OnSaveImage(Self, ImageContext, oDocument.Attachments[i].Content, oDocument.Attachments[i].Extension, sName);
        FActiveFormatter.Attributes.Add(ATTR_NAME_IMAGEREF, sName);
        FActiveFormatter.ProduceTag(TAG_NAME_ATTACHMENT);
      end
      else
      begin
        b := ZCompressStr(oDocument.Attachments[i].Content.AsAscii, zcMax);
        FActiveFormatter.ProduceText(TAG_NAME_ATTACHMENT, EncodeBase64(b));
      end;
    End;
  FActiveFormatter.ProduceClose(TAG_NAME_ATTACHMENTS);
End;


Procedure TWPNativeWriter.WriteDocumentStop(oDocument : TWPWorkingDocument);
Begin
  Assert(CheckCondition(Not FTextOpen, 'WriteDocumentStop', 'Text is still open'));
  FActiveFormatter.ProduceClose(TAG_NAME_DOCUMENT);
End;


Procedure TWPNativeWriter.WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  If (oSection.Namespace <> '') Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NAMESPACE, oSection.Namespace);
  If (oSection.Name <> '') Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NAME, oSection.Name);
  If (oSection.HasData) Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_DATA, oSection.RawDataAsText);
  FActiveFormatter.Attributes.Add(ATTR_NAME_DELETABLE, BooleanToString(oSection.Deletable));
  FActiveFormatter.Attributes.Add(ATTR_NAME_ISFIELD, BooleanToString(oSection.IsField));
  If oSection.Key <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_KEY, oSection.Key);
  If oSection.DisplayName <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_DISPLAYNAME, oSection.DisplayName);
  If oSection.ReadOnly <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_READONLY, NAMES_WPSTRISTATE[oSection.ReadOnly]);
  If oSection.DisplayType <> sdtNone Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_DISPLAYTYPE, NAMES_WPWorkingDocumentSectionDISPLAYTYPE[oSection.DisplayType]);
  SetStyleAttributes(oSection);
  FActiveFormatter.ProduceOpen(TAG_NAME_SECTION);
End;


Procedure TWPNativeWriter.WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  Assert(CheckCondition(Not FTextOpen, 'WriteDocumentStop', 'Text is still open'));
  FActiveFormatter.ProduceClose(TAG_NAME_SECTION);
End;


Procedure TWPNativeWriter.WriteBorder(Const sName : String; oBorder : TWPBorder);
Var
  oMem : TFslMemoryStream;
  b : TBytes;
Begin
  If oBorder.Defined Then
    Begin
    FActiveFormatter.Attributes.Add(ATTR_NAME_DEFINED, BooleanToString(oBorder.Defined));
    FActiveFormatter.Attributes.Add(ATTR_NAME_FANCY, BooleanToString(oBorder.Fancy));
    If oBorder.Colour <> DEF_COLOUR Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_COLOUR, ColourToXMLColourString(oBorder.Colour));
    If oBorder.OuterColour <> DEF_COLOUR Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_OUTERCOLOUR, ColourToXMLColourString(oBorder.OuterColour));
    If oBorder.OuterColour2 <> DEF_COLOUR Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_OUTERCOLOUR2, ColourToXMLColourString(oBorder.OuterColour2));
    If oBorder.Width <> 0 Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_WIDTH, IntegerToString(oBorder.Width));
    If oBorder.Style <> apsSolid Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_STYLE, ADVPENSTYLE_CODES[oBorder.Style]);
    If oBorder.LowOuterlimit <> DEF_WORD Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_LOW_OUTER, IntegerToString(oBorder.LowOuterlimit));
    If oBorder.HighOuterLimit <> DEF_WORD Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_HIGH_OUTER, IntegerToString(oBorder.HighOuterLimit));

    If oBorder.BrushImage <> Nil Then
    Begin
      FActiveFormatter.ProduceOpen(sName);
      oMem := TFslMemoryStream.Create;
      Try
        oBorder.BrushImage.SaveToStream(oMem);
        b := oMem.Buffer.AsBytes;
        FActiveFormatter.ProduceText(TAG_NAME_BRUSH, EncodeBase64(b));
      Finally
        oMem.Free;
      End;
      FActiveFormatter.ProduceClose(sName);
    End
    Else
      FActiveFormatter.ProduceTag(sName);
    End;
End;


Procedure TWPNativeWriter.WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece);
Begin
  SetStyleAttributes(oTable);
  If oTable.ReadOnly <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_READONLY, NAMES_WPSTRISTATE[oTable.ReadOnly]);
  If oTable.BorderPolicy <> tbpNone Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_BORDERPOLICY, CODES_TWPWorkingDocumentTableBORDERPOLICY[oTable.BorderPolicy]);
  If oTable.Background <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_TABLE_BACKGROUND, ColourToXMLColourString(oTable.Background));
  If oTable.HorizontalMargin <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_HORIZONTAL_MARGIN, IntegerToString(oTable.HorizontalMargin));
  If oTable.VerticalMargin <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_VERTICAL_MARGIN, IntegerToString(oTable.VerticalMargin));
  FActiveFormatter.Attributes.Add(ATTR_NAME_EXPAND_LAST, BooleanToString(oTable.ExpandLastColumn));

  FActiveFormatter.ProduceOpen(TAG_NAME_TABLE);
  WriteTableItemAttributes(oTable);
  WriteBorder(TAG_NAME_CENTERHORIZONTALBORDER, oTable.CenterHorizontalBorder);
  WriteBorder(TAG_NAME_CENTERVERTICALBORDER, oTable.CenterVerticalBorder);
End;


Procedure TWPNativeWriter.WriteTableItemAttributes(oTableItem : TWPWorkingDocumentTableItemPiece);
Begin
  WriteBorder(TAG_NAME_LEFTBORDER, oTableItem.LeftBorder);
  WriteBorder(TAG_NAME_RIGHTBORDER, oTableItem.RightBorder);
  WriteBorder(TAG_NAME_TOPBORDER, oTableItem.TopBorder);
  WriteBorder(TAG_NAME_BOTTOMBORDER, oTableItem.BottomBorder);
End;


Procedure TWPNativeWriter.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  FActiveFormatter.ProduceClose(TAG_NAME_TABLE);
End;


Procedure TWPNativeWriter.WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece);
Begin
  SetStyleAttributes(oTableRow);
  SetHotspotAttributes(oTableRow);
  If oTableRow.ReadOnly <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_READONLY, NAMES_WPSTRISTATE[oTableRow.ReadOnly]);
  If oTableRow.Header Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_HEADER, BooleanToString(oTableRow.Header));
  If oTableRow.BreakBefore Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_BREAK_BEFORE, BooleanToString(oTableRow.BreakBefore));
  If oTableRow.LowerPaddingSize > 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_LOWER_PADDING_SIZE, IntegerToString(oTableRow.LowerPaddingSize));
  If oTableRow.LowerPaddingColour <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_LOWER_PADDING_COLOUR, ColourToXMLColourString(oTableRow.LowerPaddingColour));
  If oTableRow.Background <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ROW_BACKGROUND, ColourToXMLColourString(oTableRow.Background));
  FActiveFormatter.ProduceOpen(TAG_NAME_TABLE_ROW);
End;


Procedure TWPNativeWriter.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean);
Begin
  FActiveFormatter.ProduceClose(TAG_NAME_TABLE_ROW);
End;


Procedure TWPNativeWriter.WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece);
Begin
  SetStyleAttributes(oTableCell);
  SetHotspotAttributes(oTableCell);
  If oTableCell.ReadOnly <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_READONLY, NAMES_WPSTRISTATE[oTableCell.ReadOnly]);
  If oTableCell.Span <> 1 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_SPAN, IntegerToString(oTableCell.Span));
  If oTableCell.Width <> 0 Then                  
    FActiveFormatter.Attributes.Add(ATTR_NAME_WIDTH, RealToString(oTableCell.Width));
  If oTableCell.Background <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_CELL_BACKGROUND, ColourToXMLColourString(oTableCell.Background));
  If oTableCell.MarginLeft <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_MARGIN_LEFT, IntegerToString(oTableCell.MarginLeft));
  If oTableCell.MarginRight <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_MARGIN_RIGHT, IntegerToString(oTableCell.MarginRight));
  If oTableCell.MarginTop <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_MARGIN_TOP, IntegerToString(oTableCell.MarginTop));
  If oTableCell.MarginBottom <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_MARGIN_BOTTOM, IntegerToString(oTableCell.MarginBottom));
  If oTableCell.VerticalAlignment <> VerticalAlignmentTop Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_VERTICALALIGNMENT, NAMES_WORDPROCESSORVERTICALALIGNMENT[oTableCell.VerticalAlignment]);
  FActiveFormatter.ProduceOpen(TAG_NAME_TABLE_CELL);
  WriteTableItemAttributes(oTableCell);
End;


Procedure TWPNativeWriter.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  FActiveFormatter.ProduceClose(TAG_NAME_TABLE_CELL);
End;


Procedure TWPNativeWriter.WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece);
Begin
  If Assigned(oParagraph) Then
  Begin
    SetParagraphAttributes(oParagraph.Format);
    SetStyleAttributes(oParagraph);
    FActiveFormatter.ProduceOpen(TAG_NAME_PARAGRAPH);
  End;
End;

Procedure TWPNativeWriter.WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  If FTextOpen Then
    CloseText;
  If Assigned(oParagraph) Then
    FActiveFormatter.ProduceClose(TAG_NAME_PARAGRAPH);
End;


Procedure TWPNativeWriter.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
  SetStyleAttributes(oBreak);
  FActiveFormatter.Attributes.Add(ATTR_NAME_BREAKTYPE, WPWorkingDocumentBreakPIECETYPE_NAMES[oBreak.BreakType]);
  FActiveFormatter.Attributes.Add(ATTR_NAME_ALIGNMENT, NAMES_WPSALIGNMENT[oBreak.Alignment]);
  If oBreak.Width <> 1 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_WIDTH, RealToString(oBreak.Width));
  If oBreak.PenWidth <> 0 Then
  Begin
    FActiveFormatter.Attributes.Add(ATTR_NAME_PENCOLOUR, ColourToXMLColourString(oBreak.PenColour));
    FActiveFormatter.Attributes.Add(ATTR_NAME_PENWIDTH, IntegerToString(oBreak.PenWidth));
    FActiveFormatter.Attributes.Add(ATTR_NAME_PENSTYLE, ADVPENSTYLE_CODES[oBreak.PenStyle]);
    FActiveFormatter.Attributes.Add(ATTR_NAME_PENENDSTYLE, ADVPENENDSTYLE_CODES[oBreak.EndStyle]);
  End;
  FActiveFormatter.ProduceTag(TAG_NAME_BREAK);
End;


Procedure TWPNativeWriter.WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece);
Begin
  If FTextOpen Then
    CloseText;
  if oField.AnnotationId <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ANNOTATION, IntegerToString(oField.AnnotationId));
  If oField.Namespace <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NAMESPACE, oField.Namespace);
  If oField.Name <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NAME, oField.Name);
  If oField.Deletable Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_DELETABLE, BooleanToString(oField.Deletable));
  If oField.HasData Then
  Begin
    FActiveFormatter.Attributes.Add(ATTR_NAME_DATA, oField.RawDataAsText);
    If oField.DataValue['Mask'] <> ''  Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_MASK, oField.DataValue['Mask']);
  End;
  If oField.CheckedIndex <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_INDEX, IntegerToString(oField.CheckedIndex));

  FActiveFormatter.Attributes.Add(ATTR_NAME_FIXEDFORMAT, TWPDOCUMENTOBJECT_FIXED_FORMAT[oField.FixedFormat]);
  If oField.Width <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_WIDTH, IntegerToString(oField.Width));
  SetHotspotAttributes(oField);
  If oField.ReadOnly <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_READONLY, NAMES_WPSTRISTATE[oField.ReadOnly]);
  SetStyleAttributes(oField);
  FActiveFormatter.ProduceOpen(TAG_NAME_FIELD);
End;

Procedure TWPNativeWriter.WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece);
Begin
  If FTextOpen Then
    CloseText;
  FActiveFormatter.ProduceClose(TAG_NAME_FIELD);
End;

Procedure TWPNativeWriter.WriteText(oText : TWPWorkingDocumentTextPiece);
Begin
  If FTextOpen Then
    Begin
    If MatchesText(oText) Then
      FText := FText + oText.Content
    Else
      CloseText;
    End;
  If oText.DrawnFont <> '' Then
  Begin
    SetStyleAttributes(oText);
    if oText.AnnotationId <> 0 Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_ANNOTATION, IntegerToString(oText.AnnotationId));

    FActiveFormatter.Attributes.Add(ATTR_NAME_DRAWNFONT, oText.DrawnFont);
    FActiveFormatter.ProduceText(TAG_NAME_TEXT, oText.Content);
  End
  Else If Not FTextOpen Then
    OpenText(oText);
End;

Function TWPNativeWriter.SaveImageToBuffer(oImage : TWPWorkingDocumentImagePiece; oImg : TFslGraphic; oBuffer : TFslBuffer) : String;
Var
  oMemory : TFslMemoryStream;
Begin
  oMemory := TFslMemoryStream.Create;
  Try
    oMemory.Buffer := oBuffer.Link;
    oMemory.Expand := True;

    if not (oImg is TFslVCLGraphic) then
    begin
//      if (oImg is TWPPDFGraphic) then
//        // do nothing - this is saved as a reference
//      else
        oImg.SaveToStream(oMemory);
      Result := oImg.Extension;
    end
    else If TFslVCLGraphic(oImg).Handle Is TJPEGImage Then
    Begin
      oImg.SaveToStream(oMemory);
      Result := 'jpg';
    End
    Else
    Begin
      TFslPortableNetworkGraphic.SavePNGToStream(TFslVCLGraphic(oImg), oMemory);
      Result := 'png';
    End;
  Finally
    oMemory.Free;
  End;
End;


Procedure TWPNativeWriter.SetImageAttributes(oImage : TWPWorkingDocumentImagePiece);
Begin
  SetStyleAttributes(oImage);
  SetHotspotAttributes(oImage);
  if oImage.AnnotationId <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ANNOTATION, IntegerToString(oImage.AnnotationId));

  If oImage.Border <> 0 Then
  Begin
    FActiveFormatter.Attributes.Add(ATTR_NAME_BORDERWIDTH, IntegerToString(oImage.Border));
    If oImage.BorderColour <> DEF_COLOUR Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_BORDERCOLOR, ColourToXMLColourString(oImage.BorderColour));
  End;
  FActiveFormatter.Attributes.Add(ATTR_NAME_HEIGHT, IntegerToString(oImage.Height));
  FActiveFormatter.Attributes.Add(ATTR_NAME_WIDTH, IntegerToString(oImage.Width));
  FActiveFormatter.Attributes.Add(ATTR_NAME_FRAMEINDEX, IntegerToString(oImage.FrameIndex));
  FActiveFormatter.Attributes.Add(ATTR_NAME_VERTICALALIGNMENT, NAMES_IMAGEVERTICALALIGNMENT[oImage.VerticalAlignment]);
  FActiveFormatter.Attributes.Add(ATTR_NAME_SIZEPOLICY, NAMES_IMAGESIZEPOLICY[oImage.SizePolicy]);
  If oImage.TransparentColour <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_TRANSPARENTCOLOR, ColourToXMLColourString(oImage.TransparentColour));

End;


Procedure TWPNativeWriter.WriteImageReference(oImage : TWPWorkingDocumentImagePiece; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
Var
  sName : String;
  sSelectionName : String;
  i : integer;
Begin
//  if oImage.Image is TWPPDFGraphic then
//    sName := TWPPDFGraphic(oImage.Image).Attachment.Id
//  else
  begin
    Assert(CheckCondition(Assigned(OnSaveImage), 'WriteImageReference', 'OnSaveImage is not assigned'));
    sName := oImage.Name;
    OnSaveImage(Self, ImageContext, oBuffer, sExt, sName);
  end;

  If Assigned(oSelectionBuffer) Then
  Begin
    sSelectionName := sName+'-selection';
    OnSaveImage(Self, ImageContext, oSelectionBuffer, sExt, sSelectionName);
  End;
  If sName <> '' Then
    Begin
    oImage.Name := sName;
    SetImageAttributes(oImage);
    FActiveFormatter.Attributes.Add(ATTR_NAME_IMAGEREF, sName);
    If Assigned(oSelectionBuffer) Then
      FActiveFormatter.Attributes.Add(ATTR_NAME_IMAGEREF_SELECTION, sSelectionName);
    If oImage.hasImageMap  Or (oImage.Adornments.Count > 0)  Or (oImage.Adornments.Count > 0) Then
    Begin
      FActiveFormatter.ProduceOpen(TAG_NAME_IMAGE);
      If oImage.hasImageMap Then
        WriteImageMap(oImage.ImageMap);
      For i := 0 to oImage.Adornments.Count - 1 Do
        WriteImageAdornment(oImage.Adornments[i]);
      FActiveFormatter.ProduceClose(TAG_NAME_IMAGE);
    End
    Else
      FActiveFormatter.ProduceTag(TAG_NAME_IMAGE);
    End;
End;


Procedure TWPNativeWriter.WriteImageSource(oImage : TWPWorkingDocumentImagePiece; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
var
  iLoop : Integer;
Begin
  SetImageAttributes(oImage);
  FActiveFormatter.Attributes.Add(ATTR_NAME_NAME, oImage.Name);
  FActiveFormatter.Attributes.Add(ATTR_NAME_FORMAT, sExt);
  FActiveFormatter.ProduceOpen(TAG_NAME_IMAGE);
  FActiveFormatter.ProduceText(TAG_NAME_DATA, EncodeBase64(ZCompressStr(oBuffer.AsAscii, zcMax)));
  If Assigned(oSelectionBuffer) Then
  begin
    FActiveFormatter.ProduceText(TAG_NAME_SELECTION, EncodeBase64(ZCompressStr(oBuffer.AsAscii, zcMax)));
  end;
  If oImage.HasImageMap Then
    WriteImageMap(oImage.ImageMap);
  For iLoop := 0 to oImage.Adornments.Count - 1 Do
    WriteImageAdornment(oImage.Adornments[iLoop]);
  FActiveFormatter.ProduceClose(TAG_NAME_IMAGE);
End;


Procedure TWPNativeWriter.WriteImage(oImage : TWPWorkingDocumentImagePiece);
Var
  oBuffer : TFslBuffer;
  oSelectionBuffer : TFslBuffer;
  sExt : String;
Begin
  If FTextOpen Then
    CloseText;

  Assert(CheckCondition(Assigned(oImage.Image), 'WriteImage', 'image.image is nil'));

  oSelectionBuffer := Nil;
  oBuffer := TFslBuffer.Create;
  Try
    sExt := SaveImageToBuffer(oImage, oImage.Image, oBuffer);
    If oImage.HasSelectionImage Then
      oSelectionBuffer := TFslBuffer.Create;
    Try
      If oImage.HasSelectionImage Then
        SaveImageToBuffer(oImage, oImage.SelectionImage, oBuffer);

      If ImagesAsReferences or (oBuffer.Size = 0) Then
        WriteImageReference(oImage, oBuffer, oSelectionBuffer, sExt)
      Else
        WriteImageSource(oImage, oBuffer, oSelectionBuffer, sExt);
    Finally
      oSelectionBuffer.Free;
    End;
  Finally
    oBuffer.Free;
  End;
End;

Procedure TWPNativeWriter.WriteLineBreak(oLineBreak : TWPWorkingDocumentLineBreakPiece);
Begin
  If FTextOpen Then
    CloseText;
  SetStyleAttributes(oLineBreak);
  if oLineBreak.AnnotationId <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ANNOTATION, IntegerToString(oLineBreak.AnnotationId));
  FActiveFormatter.ProduceTag(TAG_NAME_LINEBREAK);
End;

Procedure TWPNativeWriter.OpenText(oText : TWPWorkingDocumentTextPiece);
Begin
  Assert(CheckCondition(Not FTextOpen, 'OpenText', 'Text is still open'));
  FTextOpen := True;
  FTextPiece := oText.Link;
  FText := oText.Content;
End;

Function TWPNativeWriter.MatchesText(oText : TWPWorkingDocumentTextPiece) : Boolean;
Begin
  Result := oText.StyleMatches(FTextPiece) And (oText.DrawnFont = '') And (oText.annotationId = FTextPiece.AnnotationId);
End;

Procedure TWPNativeWriter.CloseText;
Begin
  SetStyleAttributes(FTextPiece);
  if FTextPiece.AnnotationId <> 0 Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ANNOTATION, IntegerToString(FTextPiece.AnnotationId));
  FActiveFormatter.ProduceText(TAG_NAME_TEXT, FText);
  FTextPiece.Free;
  FTextPiece := Nil;
  FTextOpen := False;
End;

Procedure TWPNativeWriter.SetHotspotAttributes(oPiece : TWPWorkingDocumentPiece);
Begin
  If (oPiece.HasHotspot) Then
    SetHotspotAttributes(oPiece.Hotspot);
End;


Procedure TWPNativeWriter.SetHotspotAttributes(oHotspot : TWPHotspot);
Begin
  If oHotspot.URL <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_URL, oHotspot.URL);
  If oHotspot.Title <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_TITLE, oHotspot.Title);
  If oHotspot.Key <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_KEY, oHotspot.Key);
  If oHotspot.LinkColour <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_LINKCOLOUR, ColourToXMLColourString(oHotspot.LinkColour));
  If oHotspot.HoverColour <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_HOVERCOLOUR, ColourToXMLColourString(oHotspot.HoverColour));
End;


Procedure TWPNativeWriter.SetStyleAttributes(oPiece : TWPWorkingDocumentPiece);
Begin
  If oPiece.Style <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_STYLE, oPiece.Style);
  SetFontAttributes(oPiece.Font);
End;

Procedure TWPNativeWriter.SetFontAttributes(oFont : TWPSFontDetails);
Begin
  If oFont.Name <> '' Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_FONTNAME, oFont.Name);
  If oFont.Size <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_SIZE, IntegerToString(oFont.Size));
  If oFont.Bold <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_BOLD, NAMES_WPSTRISTATE[oFont.Bold]);
  If oFont.Italic <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ITALIC, NAMES_WPSTRISTATE[oFont.Italic]);
  If oFont.Underline <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_UNDERLINE, NAMES_WPSTRISTATE[oFont.Underline]);
  If oFont.Strikethrough <> tsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_STRIKETHROUGH, NAMES_WPSTRISTATE[oFont.Strikethrough]);
  If oFont.State <> fsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_FONTSTATE, NAMES_WPSFONTSTATE[oFont.State]);

  If oFont.Foreground <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_FOREGROUND, ColourToXMLColourString(oFont.Foreground));
  If oFont.Background <> DEF_COLOUR Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_BACKGROUND, ColourToXMLColourString(oFont.Background));
  If oFont.Capitalization <> fcsUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_CAPITALIZATION, NAMES_WPSCAPSSTATE[oFont.Capitalization]);
End;

Procedure TWPNativeWriter.SetParagraphAttributes(oFormat : TWPSParagraphDetails);
Begin
  If oFormat.Align <> WordProcessorParagraphAlignmentUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_ALIGN, NAMES_WPSPARAGRAPHALIGNMENT[oFormat.Align]);
  If (oFormat.LeftIndent <> 0) And (oFormat.LeftIndent <> DEF_WORD) Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_LEFTINDENT, IntegerToString(oFormat.LeftIndent));
  If (oFormat.RightIndent <> 0) And (oFormat.RightIndent <> DEF_WORD) Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_RIGHTINDENT, IntegerToString(oFormat.RightIndent));
  If oFormat.BulletType <> tbUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_BULLETTYPE, NAMES_WPSPARAGRAPHBULLETTYPE[oFormat.BulletType]);
  If oFormat.ListType <> WPSParagraphListTypeUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_LISTTYPE, NAMES_WPSPARAGRAPHLISTTYPE[oFormat.ListType]);
  If oFormat.NumberType <> tnUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NUMBERTYPE, NAMES_WPSPARAGRAPHNUMBERTYPE[oFormat.NumberType]);
  If oFormat.NumberFormat <> nwUnknown Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_NUMBERFORMAT, NAMES_WPSPARAGRAPHNUMBERFORMAT[oFormat.NumberFormat]);
  If oFormat.FixedNumber <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_FIXEDNUMBER, IntegerToString(oFormat.FixedNumber));
  If oFormat.MarginBottom <> DEF_WORD Then
    FActiveFormatter.Attributes.Add(ATTR_NAME_MARGIN_BOTTOM, IntegerToString(oFormat.MarginBottom));
End;

Procedure TWPNativeWriter.WriteStyles;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Styles.count - 1 Do
    WriteStyle(Styles[iLoop]);
End;


Procedure TWPNativeWriter.WriteStyle(oStyle: TWPStyle);
Begin
  FActiveFormatter.Attributes.Add(ATTR_NAME_STYLE, oStyle.Name);
  SetFontAttributes(oStyle.Font);
  SetParagraphAttributes(oStyle.Paragraph);
  FActiveFormatter.Attributes.Add(ATTR_NAME_RESET_STYLE, BooleanToString(oStyle.ResetOnNewParagraph));
  FActiveFormatter.ProduceTag(TAG_NAME_STYLE);
End;


Procedure TWPNativeWriter.WriteImageMap(oMap : TWPImageMap);
Var
  iLoop : Integer;
Begin
  FActiveFormatter.ProduceOpen(TAG_NAME_MAP);
  For iLoop := 0 To oMap.Areas.count - 1 Do
    WriteImageMapArea(oMap.Areas[iLoop]);
  FActiveFormatter.ProduceClose(TAG_NAME_MAP);
End;


Procedure TWPNativeWriter.WriteImageMapArea(oArea : TWPImageMapArea);
Var
  iLoop : Integer;
Begin
  SetHotspotAttributes(oArea);
  FActiveFormatter.ProduceOpen(TAG_NAME_AREA);
  For iLoop := 0 To oArea.Coordinates.count - 1 Do
    WriteImageMapCoordinate(oArea.Coordinates[iLoop]);
  FActiveFormatter.ProduceClose(TAG_NAME_AREA);
End;


Procedure TWPNativeWriter.WriteImageMapCoordinate(oCoordinate : TWPCoordinate);
Begin
  FActiveFormatter.Attributes.Add(ATTR_NAME_X, IntegerToString(oCoordinate.X));
  FActiveFormatter.Attributes.Add(ATTR_NAME_Y, IntegerToString(oCoordinate.Y));
  FActiveFormatter.ProduceTAG(TAG_NAME_COORD);
End;


Function TWPNativeWriter.SupportsNestedRows: Boolean;
Begin
  Result := True;
End;




Function AllowedItemsToString(aAllowed : TWPNativeDocumentReaderAllowedItems): String; Overload;
Var
  aLoop : TWPNativeDocumentReaderAllowedItem;
Begin
  Result := '';
  For aLoop := Low(TWPNativeDocumentReaderAllowedItem) To High(TWPNativeDocumentReaderAllowedItem) Do
  Begin
    If aLoop In aAllowed Then
      StringAppend(Result, NAMES_NativeDocumentREADERALLOWEDITEM[aLoop], ',');
  End;
End;


function TWPNativeWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FActiveFormatter.sizeInBytes);
  inc(result, (FText.length * sizeof(char)) + 12);
  inc(result, FTextPiece.sizeInBytes);
end;

Constructor TWPNativeDocumentReader.Create;
Begin
  Inherited;

End;


Destructor TWPNativeDocumentReader.Destroy;
Begin

  Inherited;
End;


Procedure TWPNativeDocumentReader.Read(oDocument : TWPWorkingDocument);
Begin
  Inherited;

  FActiveExtractor := TFslXMLExtractor.Create(Stream.Link);
  FActiveExtractor.ConsumeOpen(TAG_NAME_STREAM);
  FVersion := FActiveExtractor.Attributes[ATTR_NAME_VERSION];

  Try
    If IsFragment Then
    Begin
      ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemStyle, WPNativeDocumentReaderAllowedItemDocument, WPNativeDocumentReaderAllowedItemLineBreak,
        WPNativeDocumentReaderAllowedItemTable, WPNativeDocumentReaderAllowedItemSection, WPNativeDocumentReaderAllowedItemParagraph,
        WPNativeDocumentReaderAllowedItemField, WPNativeDocumentReaderAllowedItemText, WPNativeDocumentReaderAllowedItemImage, WPNativeDocumentReaderAllowedItemBreak]);
    End
    Else
    Begin
      ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemStyle, WPNativeDocumentReaderAllowedItemDocument]);
    End;

  Finally
    FActiveExtractor.ConsumeClose(TAG_NAME_STREAM);
    FActiveExtractor.Free;
    FActiveExtractor := nil;
  End;

  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;


Function TWPNativeDocumentReader.GetDefaultFontSize : Integer;
Begin
  If HasStyles And Styles.HasDefaultStyle Then
    Result := Styles.DefaultStyle.Font.Size
  Else
    Result := 11;
End;


Function TWPNativeDocumentReader.GetDefaultFontName : String;
Begin
  If HasStyles And Styles.HasDefaultStyle Then
    Result := Styles.DefaultStyle.Font.Name
  Else
    Result := 'Verdana';
End;


Function TWPNativeDocumentReader.ReadEnumeratedAttribute(Const sValue : String; Const aValues : Array Of String; Const aDefault : Byte) : Byte;
Var
  iResult : Integer;
Begin
  iResult := StringArrayIndexOf(aValues, sValue);
  If iResult = -1 Then
    Result := aDefault
  Else
    Result := iResult;
End;


Function TWPNativeDocumentReader.ReadTristateAttribute(Const sValue : String) : TWPSTristate;
Begin
  Result := TWPSTriState(ReadEnumeratedAttribute(sValue, NAMES_WPSTRISTATE, Ord(tsUnknown)));
End;


Function TWPNativeDocumentReader.ReadFontstateAttribute(Const sValue : String) : TWPSFontstate;
Begin
  Result := TWPSFontstate(ReadEnumeratedAttribute(sValue, NAMES_WPSFONTSTATE, Ord(fsUnknown)));
End;


Function TWPNativeDocumentReader.ReadCapsStateAttribute(Const sValue : String) : TWPSCapsstate;
Begin
  Result := TWPSCapsState(ReadEnumeratedAttribute(sValue, NAMES_WPSCAPSSTATE, Ord(fcsUnknown)));
End;
                                                                                    

Procedure TWPNativeDocumentReader.ReadStyleAttributes(oPiece: TWPWorkingDocumentPiece);
Begin
  oPiece.Style := FActiveExtractor.Attributes[ATTR_NAME_STYLE];

  ReadFontAttributes(oPiece.Font);
End;


Procedure TWPNativeDocumentReader.ReadFontAttributes(oFont : TWPSFontDetails);
Begin
  oFont.Name := FActiveExtractor.Attributes[ATTR_NAME_FONTNAME];
  oFont.Size := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_SIZE], DEF_WORD);
  oFont.Bold := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_BOLD]);
  oFont.Italic := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_ITALIC]);
  oFont.Underline := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_UNDERLINE]);
  oFont.Strikethrough := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_STRIKETHROUGH]);
  oFont.State := ReadFontstateAttribute(FActiveExtractor.Attributes[ATTR_NAME_FONTSTATE]);
  oFont.Foreground := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_FOREGROUND], DEF_COLOUR);
  oFont.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BACKGROUND], DEF_COLOUR);
  oFont.Capitalization := ReadCapsStateAttribute(FActiveExtractor.Attributes[ATTR_NAME_CAPITALIZATION]);
End;


Procedure TWPNativeDocumentReader.ReadParagraphAttributes(oParagraph : TWPSParagraphDetails);
Begin
  oParagraph.Align := TWordProcessorParagraphAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_ALIGN], NAMES_WPSPARAGRAPHALIGNMENT, ord(WordProcessorParagraphAlignmentUnknown)));
  oParagraph.BulletType := TWPSParagraphBulletType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BULLETTYPE], NAMES_WPSPARAGRAPHBULLETTYPE, ord(tbUnknown)));
  oParagraph.ListType := TWPSParagraphListType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_LISTTYPE], NAMES_WPSPARAGRAPHLISTTYPE, ord(WPSParagraphListTypeUnknown)));
  oParagraph.NumberType := TWPSParagraphNumberType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_NUMBERTYPE], NAMES_WPSPARAGRAPHNUMBERTYPE, ord(tnUnknown)));
  oParagraph.NumberFormat := TWPSParagraphNumberFormat(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_NUMBERFORMAT], NAMES_WPSPARAGRAPHNUMBERFORMAT, ord(nwUnknown)));
  oParagraph.FixedNumber := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_FIXEDNUMBER], DEF_WORD);
  oParagraph.LeftIndent := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LEFTINDENT], DEF_WORD);
  oParagraph.RightIndent := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_RIGHTINDENT], DEF_WORD);
  oParagraph.MarginBottom := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_BOTTOM], DEF_WORD);
End;


Procedure TWPNativeDocumentReader.ReadStyle;
Var
  oStyle : TWPStyle;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_STYLE + '/');

  oStyle := TWPStyle(Styles.New);
  Try
    oStyle.Name := FActiveExtractor.Attributes[ATTR_NAME_STYLE];

    If Not Styles.ExistsByName(oStyle.Name) Then
    Begin
      ReadFontAttributes(oStyle.Font);
      ReadParagraphAttributes(oStyle.Paragraph);

      Styles.Add(oStyle.Link);
    End;
  Finally
    oStyle.Free;
  End;
End;

Procedure TWPNativeDocumentReader.ReadBorder(Const sName: String; oBorder : TWPBorder);
Var
  oMem : TFslMemoryStream;
Begin
  oBorder.Clear;
  If StringReplace(FActiveExtractor.PeekXML, '/', '') = sName Then
  Begin
    FActiveExtractor.ConsumeOpen;
    oBorder.Defined := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_DEFINED], False);
    oBorder.Fancy := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_FANCY], False);
    oBorder.OuterColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_OUTERCOLOUR], DEF_COLOUR);
    oBorder.OuterColour2 := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_OUTERCOLOUR2], DEF_COLOUR);
    oBorder.Colour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_COLOUR], DEF_COLOUR);
    oBorder.Width := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    oBorder.Style := TFslPenStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_STYLE], ADVPENSTYLE_CODES, Integer(apsSolid)));
    oBorder.LowOuterlimit := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LOW_OUTER], DEF_WORD);
    oBorder.HighOuterLimit := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_HIGH_OUTER], DEF_WORD);
    If FActiveExtractor.PeekIsOpenTag(TAG_NAME_BRUSH) Then
    Begin
      FActiveExtractor.ConsumeOpen;
      oMem := TFslMemoryStream.Create;
      Try
        oMem.Buffer.AsBytes := DecodeBase64(FActiveExtractor.ConsumeTextBody);
        oBorder.BrushImage := TFslBitmapGraphic.Create;
        oMem.Position := 0;
        oBorder.BrushImage.LoadFromStream(oMem);
      Finally
        oMem.Free;
      End;
      FActiveExtractor.ConsumeClose;
      FActiveExtractor.ConsumeClose;
    End;
  End;
End;



Procedure TWPNativeDocumentReader.ReadTable(oDocument : TWPWorkingDocument);
Var
  oStart : TWPWorkingDocumentTableStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE);

  oStart := TWPWorkingDocumentTableStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.BorderPolicy := TWPWorkingDocumentTableBorderPolicy(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BORDERPOLICY], CODES_TWPWorkingDocumentTableBORDERPOLICY, ord(tbpNone)));
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BACKGROUND], DEF_COLOUR);
    oStart.HorizontalMargin := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_HORIZONTAL_MARGIN], DEF_WORD);
    oStart.VerticalMargin := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_VERTICAL_MARGIN], DEF_WORD);
    oStart.ExpandLastColumn := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_EXPAND_LAST], false);
    ReadTableItem(oStart);
    ReadBorder(TAG_NAME_CENTERHORIZONTALBORDER, oStart.CenterHorizontalBorder);
    ReadBorder(TAG_NAME_CENTERVERTICALBORDER, oStart.CenterVerticalBorder);

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
    ReadTableRow(oDocument, Nil);

  oStop := TWPWorkingDocumentStopPiece.Create(stTable);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE);
End;


Procedure TWPNativeDocumentReader.ReadTableItem(oItem : TWPWorkingDocumentTableItemPiece);
Begin
  ReadBorder(TAG_NAME_LEFTBORDER, oItem.LeftBorder);
  ReadBorder(TAG_NAME_RIGHTBORDER, oItem.RightBorder);
  ReadBorder(TAG_NAME_TOPBORDER, oItem.TopBorder);
  ReadBorder(TAG_NAME_BOTTOMBORDER, oItem.BottomBorder);
End;


Procedure TWPNativeDocumentReader.ReadTableRow(oDocument : TWPWorkingDocument; oOwner : TWPWorkingDocumentTableRowStartPiece);
Var
  oStart : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE_ROW);

  oStart := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    ReadHotspot(oStart);
    oStart.Header := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_HEADER], False);
    oStart.BreakBefore := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_BREAK_BEFORE], False);
    oStart.LowerPaddingSize := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_LOWER_PADDING_SIZE], 0);
    oStart.LowerPaddingColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_LOWER_PADDING_COLOUR], DEF_COLOUR);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BACKGROUND], DEF_COLOUR);
    oStart.Owner := oOwner;

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen And (FActiveExtractor.PeekXml = TAG_NAME_TABLE_CELL) Do
      ReadTableCell(oDocument);

  oStop := TWPWorkingDocumentStopPiece.Create(stTableRow);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen And (FActiveExtractor.PeekXml = TAG_NAME_TABLE_ROW) Do
      ReadTableRow(oDocument, oStart);

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE_ROW);
End;


Procedure TWPNativeDocumentReader.ReadTableCell(oDocument : TWPWorkingDocument);
Var
  oStart : TWPWorkingDocumentTableCellStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TABLE_CELL);

  oStart := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ReadStyleAttributes(oStart);
    ReadHotspot(oStart);
    oStart.Span := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_SPAN], 1);
    oStart.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
    oStart.Background := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BACKGROUND], DEF_COLOUR);
    oStart.MarginLeft := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_LEFT], DEF_WORD);
    oStart.MarginRight := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_RIGHT], DEF_WORD);
    oStart.MarginTop := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_TOP], DEF_WORD);
    oStart.MarginBottom := StringToIntegerOrDefault(FActiveExtractor.Attributes[ATTR_NAME_MARGIN_BOTTOM], DEF_WORD);
    oStart.VerticalAlignment := TWordProcessorVerticalAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_VERTICALALIGNMENT], NAMES_WORDPROCESSORVERTICALALIGNMENT, ord(VerticalAlignmentTop)));

    If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_WIDTH) Then
      oStart.Width := StrToFloatDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);
    ReadTableItem(oStart);

    oDocument.Pieces.Add(oStart.Link);
  Finally
    oStart.Free;
  End;

  ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemParagraph, WPNativeDocumentReaderAllowedItemBreak]);

  oStop := TWPWorkingDocumentStopPiece.Create(stTableCell);
  Try
//    ReadStyleAttributes(oStop);

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_TABLE_CELL);
End;


Procedure TWPNativeDocumentReader.ReadParagraph(oDocument : TWPWorkingDocument);
Var
  oParagraph : TWPWorkingDocumentParaPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_PARAGRAPH);

  oParagraph := TWPWorkingDocumentParaPiece.Create;
  Try
    oParagraph.SpeechMagicDouble := SpeechMagicDouble;
    ReadStyleAttributes(oParagraph);
    ReadParagraphAttributes(oParagraph.Format);

    ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemField, WPNativeDocumentReaderAllowedItemText, WPNativeDocumentReaderAllowedItemImage, WPNativeDocumentReaderAllowedItemLineBreak]);

    oDocument.Pieces.Add(oParagraph.Link);
  Finally
    oParagraph.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_PARAGRAPH);
End;


Procedure TWPNativeDocumentReader.ReadField;
Var
  oStart : TWPWorkingDocumentFieldStartPiece;
  oStop : TWPWorkingDocumentFieldStopPiece;
  sTemp : String;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_FIELD);

  oStop := TWPWorkingDocumentFieldStopPiece.Create;
  Try
    oStart := TWPWorkingDocumentFieldStartPiece.Create;
    Try
      oStart.Namespace := FActiveExtractor.Attributes[ATTR_NAME_NAMESPACE];
      oStart.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
      oStart.Deletable := StrToBoolDef(FActiveExtractor.Attributes[ATTR_NAME_DELETABLE], False);
      oStart.RawDataAsText := FActiveExtractor.Attributes[ATTR_NAME_DATA];
      sTemp := FActiveExtractor.Attributes[ATTR_NAME_MASK];
      If (sTemp <> '') Then
        oStart.DataValue['Mask'] := sTemp;

      oStart.FixedFormat := ReadFixedFormatAttribute(FActiveExtractor.Attributes[ATTR_NAME_FIXEDFORMAT]);
      sTemp := FActiveExtractor.Attributes[ATTR_NAME_VOCABULARY];
      If (sTemp <> '') Then
        oStart.DataValue['List'] := sTemp;

      ReadHotspot(oStart);
      oStart.ReadOnly :=  ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);

      ReadStyleAttributes(oStart);

      oDocument.Pieces.Add(oStart.Link);

      ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemText, WPNativeDocumentReaderAllowedItemLineBreak, WPNativeDocumentReaderAllowedItemImage]);

      oStop.Style := oStart.Style;
      oStop.Font.Assign(oStart.Font);

    Finally
      oStart.Free;
    End;

    oDocument.Pieces.Add(oStop.Link);
  Finally
    oStop.Free;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_FIELD);
End;


Procedure TWPNativeDocumentReader.ReadLineBreak(oDocument : TWPWorkingDocument);
Var
  oBreak : TWPWorkingDocumentLineBreakPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_LINEBREAK + '/');

  oBreak := TWPWorkingDocumentLineBreakPiece.Create;
  Try
    ReadStyleAttributes(oBreak);

    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;
End;


Procedure TWPNativeDocumentReader.ReadText(oDocument : TWPWorkingDocument);

  Procedure AddText(Const sContent : String);
  Var
    oText : TWPWorkingDocumentTextPiece;
  Begin
    oText := TWPWorkingDocumentTextPiece.Create;
    Try
      ReadStyleAttributes(oText);

      oText.Content := sContent;

      oDocument.Pieces.Add(oText.Link);
    Finally
      oText.Free;
    End;
  End;

Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_TEXT);

  Splitter.Init(FActiveExtractor.ConsumeTextBody);

  While Splitter.More Do
    AddText(Splitter.Next);

  FActiveExtractor.ConsumeClose(TAG_NAME_TEXT);
End;


Procedure TWPNativeDocumentReader.ReadBreak(oDocument : TWPWorkingDocument);
Var
  oBreak : TWPWorkingDocumentBreakPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_BREAK + '/');

  oBreak := TWPWorkingDocumentBreakPiece.Create;
  Try
    ReadStyleAttributes(oBreak);
    oBreak.BreakType := TWPWorkingDocumentBreakPieceType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_BREAKTYPE], WPWorkingDocumentBreakPIECETYPE_NAMES, ord(btLine)));
    oBreak.Alignment := TWordProcessorAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_ALIGNMENT], NAMES_WPSALIGNMENT, ord(WordProcessorAlignmentUnknown)));
    oBreak.Width := StrToFloatDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 1);
    oBreak.PenColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_PENCOLOUR], clBlack);
    oBreak.PenWidth := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_PENWIDTH], 1);
    oBreak.PenStyle := TFslPenStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_PENSTYLE], ADVPENSTYLE_CODES, ord(apsSolid)));
    oBreak.EndStyle := TFslPenEndStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_PENENDSTYLE], ADVPENENDSTYLE_CODES, ord(apesRound)));
    oDocument.Pieces.Add(oBreak.Link);
  Finally
    oBreak.Free;
  End;
End;


Procedure TWPNativeDocumentReader.ReadImage;
Var
  oImage : TWPWorkingDocumentImagePiece;
  oBuffer : TFslBuffer;
  oSelectionBuffer : TFslBuffer;
  bNoImage : Boolean;
  sSelectionName : String;
  aFormat : TWPImageFormat; // = (ifUnknown, ifJPEG, ifPNG, ifBMP);
Begin
  bNoImage := StringEquals(FActiveExtractor.PeekXml, TAG_NAME_IMAGE+'/');
  If bNoImage Then
    FActiveExtractor.ConsumeOpen(TAG_NAME_IMAGE+'/')
  Else
    FActiveExtractor.ConsumeOpen(TAG_NAME_IMAGE);

  oImage := TWPWorkingDocumentImagePiece.Create;
  Try
    ReadStyleAttributes(oImage);
    ReadHotspot(oImage);

    oImage.Border := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_BORDERWIDTH], 0);
    oImage.BorderColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_BORDERCOLOR], DEF_COLOUR);

    oImage.Height := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_HEIGHT], 0);
    oImage.Width := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_WIDTH], 0);

    oImage.Name := FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF];
    sSelectionName := FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF_SELECTION];
    oImage.VerticalAlignment := TWordProcessorImageVerticalAlignment(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_VERTICALALIGNMENT], NAMES_IMAGEVERTICALALIGNMENT, ord(ivaBaseLine)));
    oImage.SizePolicy := TWordProcessorImageSizePolicy(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_SIZEPOLICY], NAMES_IMAGESIZEPOLICY, ord(ImageSizeManual)));
    oImage.TransparentColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_TRANSPARENTCOLOR], DEF_COLOUR);

    aFormat := ifUnknown;
    If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_FORMAT) Then
    Begin
      If FActiveExtractor.Attributes[ATTR_NAME_FORMAT] = 'jpg' Then
        aFormat := ifJPEG
      Else If FActiveExtractor.Attributes[ATTR_NAME_FORMAT] = 'png' Then
        aFormat := ifPNG;
    End;

    oBuffer := Nil;
    Try
      If oImage.Name <> '' Then
      Begin
        If Assigned(OnLoadImage) Then
          OnLoadImage(Self, Context, oImage.Name, oBuffer);
      End
      Else
      Begin
        oImage.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
        oBuffer := TFslBuffer.Create;

        If FVersion <> VERSION_NATIVE_ONE Then
          FActiveExtractor.ConsumeOpen(TAG_NAME_DATA);
        oBuffer.AsBytes := DecodeBase64(FActiveExtractor.ConsumeTextBody);
        If FVersion <> VERSION_NATIVE_ONE Then
          FActiveExtractor.ConsumeClose(TAG_NAME_DATA);
      End;


      oSelectionBuffer := Nil;
      Try
        If sSelectionName <> '' Then
        Begin
          If Assigned(OnLoadImage) Then
            OnLoadImage(Self, Context, oImage.Name+'-selection', oSelectionBuffer);
        End
        Else If FActiveExtractor.PeekIsOpenTag(TAG_NAME_SELECTION) Then
        Begin
          oSelectionBuffer := TFslBuffer.Create;
          FActiveExtractor.ConsumeOpen(TAG_NAME_SELECTION);
          oSelectionBuffer.AsBytes := DecodeBase64(FActiveExtractor.ConsumeTextBody);
          FActiveExtractor.ConsumeClose(TAG_NAME_SELECTION);
        End;

        If Assigned(oSelectionBuffer) Then
        Begin
          Try
            LoadImage(oImage, oBuffer, aFormat, True);
          Except
          End;
        End;

      Finally
        oSelectionBuffer.Free;
      End;

      If (FVersion <> VERSION_NATIVE_ONE) And (FActiveExtractor.PeekIsOpenTag(TAG_NAME_MAP)) Then
      Begin
        oImage.HasImageMap := True;
        ReadMap(oImage.ImageMap);
      End
      Else
        oImage.HasImageMap := False;

      If Assigned(oBuffer) Then
      Begin
        Try
          LoadImage(oImage, oBuffer, aFormat, False);
        Except
          // well, hard to know what to do. but it can happen, so we just create a pretend image.
          CreateUnloadedImage(oImage);
        End;
      End;

      If Assigned(oBuffer) Or FLoadImageHolders Then
        oDocument.Pieces.Add(oImage.Link);
    Finally
      oBuffer.Free;
    End;
  Finally
    oImage.Free;
  End;

  If Not bNoImage Then
    FActiveExtractor.ConsumeClose(TAG_NAME_IMAGE);
End;


Procedure TWPNativeDocumentReader.ReadSection(oDocument : TWPWorkingDocument);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_SECTION);
  oSection := Nil;

  If Not SuppressSections Then
  Begin
    oSection := TWPWorkingDocumentSectionStartPiece.Create;
    Try
      oSection.Namespace := FActiveExtractor.Attributes[ATTR_NAME_NAMESPACE];
      oSection.Name := FActiveExtractor.Attributes[ATTR_NAME_NAME];
      oSection.RawDataAsText := FActiveExtractor.Attributes[ATTR_NAME_DATA];
      oSection.DisplayName := FActiveExtractor.Attributes[ATTR_NAME_DISPLAYNAME];
      oSection.Key := FActiveExtractor.Attributes[ATTR_NAME_KEY];
      oSection.ReadOnly := ReadTristateAttribute(FActiveExtractor.Attributes[ATTR_NAME_READONLY]);
      oSection.DisplayType := TWPWorkingDocumentSectionDisplayType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_DISPLAYTYPE], NAMES_WPWorkingDocumentSectionDISPLAYTYPE, ord(sdtNone)));
      oSection.Deletable := StringToBoolean(FActiveExtractor.Attributes[ATTR_NAME_DELETABLE]);
      oSection.IsField := StringToBoolean(FActiveExtractor.Attributes[ATTR_NAME_ISFIELD]); 

      ReadStyleAttributes(oSection);

      oDocument.Pieces.Add(oSection.Link);
    Finally
      oSection.Free;
    End;
  End;

  ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemParagraph, WPNativeDocumentReaderAllowedItemSection, WPNativeDocumentReaderAllowedItemTable, WPNativeDocumentReaderAllowedItemBreak]);

  If Not SuppressSections Then
  Begin
    oStop := TWPWorkingDocumentStopPiece.Create(stSection);
    Try
      oStop.AssignStyle(oSection);
      oDocument.Pieces.Add(oStop.Link);
    Finally
      oStop.Free;
    End;
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_SECTION);
End;


Procedure TWPNativeDocumentReader.ReadContent(oDocument : TWPWorkingDocument; aItems : TWPNativeDocumentReaderAllowedItems);
Var
  sTag : String;
  aItem : TWPNativeDocumentReaderAllowedItem;
  iIndex : Integer;
Begin

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
  Begin
    sTag := StringStrip(FActiveExtractor.PeekXml, '/');

    iIndex := StringArrayIndexOfSensitive(NAMES_NativeDocumentREADERALLOWEDITEM, sTag);

    If iIndex < 0 Then
      RaiseError('ReadContent', 'Unhandled content - found ' + sTag + '.');

    aItem := TWPNativeDocumentReaderAllowedItem(iIndex);

    If Not (aItem In aItems) Then
      RaiseError('ReadContent', 'Unexpected content - found ' + sTag + ' expecting ' + AllowedItemsToString(aItems));

    Case aItem Of
      WPNativeDocumentReaderAllowedItemDocument : ReadDocument(oDocument);
      WPNativeDocumentReaderAllowedItemSection : ReadSection(oDocument);
      WPNativeDocumentReaderAllowedItemParagraph : ReadParagraph(oDocument);
      WPNativeDocumentReaderAllowedItemField : ReadField(oDocument);
      WPNativeDocumentReaderAllowedItemLineBreak : ReadLineBreak(oDocument);
      WPNativeDocumentReaderAllowedItemText : ReadText(oDocument);
      WPNativeDocumentReaderAllowedItemStyle : ReadStyle;
      WPNativeDocumentReaderAllowedItemTable : ReadTable(oDocument);
      WPNativeDocumentReaderAllowedItemRow : ReadTableRow(oDocument, Nil);
      WPNativeDocumentReaderAllowedItemCell : ReadTableCell(oDocument);
      WPNativeDocumentReaderAllowedItemImage : ReadImage(oDocument);
      WPNativeDocumentReaderAllowedItemBreak : ReadBreak(oDocument);
    Else
      RaiseError('ReadContent', 'Unhandled content - found ' + sTag + '.');
    End;
  End;
End;


Procedure TWPNativeDocumentReader.ReadDocument(oDocument : TWPWorkingDocument);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_DOCUMENT);

  If IsFragment Then
  Begin
    ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemStyle, WPNativeDocumentReaderAllowedItemDocument, WPNativeDocumentReaderAllowedItemTable, WPNativeDocumentReaderAllowedItemLineBreak,
      WPNativeDocumentReaderAllowedItemSection, WPNativeDocumentReaderAllowedItemParagraph, WPNativeDocumentReaderAllowedItemField,
      WPNativeDocumentReaderAllowedItemText, WPNativeDocumentReaderAllowedItemImage, WPNativeDocumentReaderAllowedItemBreak]);
  End
  Else
  Begin
    ReadContent(oDocument, [WPNativeDocumentReaderAllowedItemSection, WPNativeDocumentReaderAllowedItemParagraph,
      WPNativeDocumentReaderAllowedItemTable, WPNativeDocumentReaderAllowedItemBreak]);
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_DOCUMENT);
End;


Procedure TWPNativeDocumentReader.ReadMap(oMap : TWPImageMap);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_MAP);
  While FActiveExtractor.PeekIsOpenTag(TAG_NAME_AREA) Do
    ReadMapArea(oMap);
  FActiveExtractor.ConsumeClose(TAG_NAME_MAP);
End;


Procedure TWPNativeDocumentReader.ReadMapArea(oMap : TWPImageMap);
Var
  oArea : TWPImageMapArea;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_AREA);
  oArea := TWPImageMapArea.Create;
  Try
    ReadHotspot(oArea);
    While Not FActiveExtractor.PeekIsClose Do
      ReadMapCoordinate(oArea);
    oMap.Areas.Add(oArea.Link);
  Finally
    oArea.Free;
  End;
  FActiveExtractor.ConsumeClose(TAG_NAME_AREA);
End;


Procedure TWPNativeDocumentReader.ReadMapCoordinate(oArea : TWPImageMapArea);
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_COORD + '/');
  oArea.Coordinates.Add(StringToInteger32(FActiveExtractor.Attributes[ATTR_NAME_X]), StringToInteger32(FActiveExtractor.Attributes[ATTR_NAME_Y]));
End;


Procedure TWPNativeDocumentReader.ReadHotspot(oPiece: TWPWorkingDocumentPiece);
Begin
  If FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_URL) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_KEY) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_TITLE) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_LINKCOLOUR) Or
     FActiveExtractor.Attributes.ExistsByKey(ATTR_NAME_HOVERCOLOUR) Then
    Begin
    oPiece.HasHotspot := True;
    ReadHotspot(oPiece.Hotspot);
    End;
End;


Procedure TWPNativeDocumentReader.ReadHotspot(oHotspot : TWPHotspot);
Begin
  oHotspot.URL := FActiveExtractor.Attributes[ATTR_NAME_URL];
  oHotspot.Title := FActiveExtractor.Attributes[ATTR_NAME_TITLE];
  oHotspot.Key := FActiveExtractor.Attributes[ATTR_NAME_KEY];
  oHotspot.LinkColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_LINKCOLOUR], DEF_COLOUR);
  oHotspot.HoverColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_HOVERCOLOUR], DEF_COLOUR);
End;


function TWPNativeDocumentReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FActiveExtractor.sizeInBytes);
  inc(result, (FVersion.length * sizeof(char)) + 12);
end;

Destructor TWPNativeDocumentWriter.Destroy;
Begin
  FFormatter.Free;

  Inherited;
End;

Procedure TWPNativeDocumentWriter.SetFormatter(Const Value: TFslXMLFormatter);
Begin
  FFormatter.Free;
  FFormatter := Value;
End;


Procedure TWPNativeDocumentWriter.WriteDocument(oDocument : TWPDocument);
Begin
  FFormatter.Attributes.Add(ATTR_NAME_XMLNS, NAMESPACE_WP_NATIVE);
  FFormatter.Attributes.Add(ATTR_NAME_VERSION, VERSION_NATIVE_THREE);
  FFormatter.ProduceOpen(TAG_NAME_STREAM);
  WriteStyles(oDocument.Styles);

  FFormatter.ProduceOpen(TAG_NAME_DOCUMENT);
  WriteBlocks(oDocument.Blocks);
  FFormatter.ProduceClose(TAG_NAME_DOCUMENT);

  FFormatter.ProduceClose(TAG_NAME_STREAM);
End;

Procedure TWPNativeDocumentWriter.WriteBlocks(oBlocks : TWPDocumentBlocks);
Var
  iLoop : Integer;
  oBlock : TWPDocumentBlock;
Begin
  For iLoop := 0 To oBlocks.Count - 1 Do
  Begin
    oBlock := oBLocks[iLoop];
    If oBlock Is TWPDocumentTable Then
      WriteTable(TWPDocumentTable(oBlock))
    Else If oBlock Is TWPDocumentSection Then
      WriteSection(TWPDocumentSection(oBlock))
    Else If oBlock Is TWPDocumentBreak Then
      WriteBreak(TWPDocumentBreak(oBlock))
    Else If oBlock Is TWPDocumentParagraph Then
      WriteParagraph(TWPDocumentParagraph(oBlock))
  End;
End;



Procedure TWPNativeDocumentWriter.WriteSection(oSection : TWPDocumentSection);
Begin
  If (oSection.Namespace <> '') Then
    FFormatter.Attributes.Add(ATTR_NAME_NAMESPACE, oSection.Namespace);
  If (oSection.Name <> '') Then
    FFormatter.Attributes.Add(ATTR_NAME_NAME, oSection.Name);
  If (oSection.RawData <> Nil) Then
    FFormatter.Attributes.Add(ATTR_NAME_DATA, oSection.RawData.AsText);
  FFormatter.Attributes.Add(ATTR_NAME_DELETABLE, BooleanToString(oSection.Deletable));
  FFormatter.Attributes.Add(ATTR_NAME_ISFIELD, BooleanToString(oSection.IsField));
  If oSection.Key <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_KEY, oSection.Key);
  If oSection.Title <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_DISPLAYNAME, oSection.Title);
  If oSection.ReadOnly <> ReadOnlyDefault Then
    FFormatter.Attributes.Add(ATTR_NAME_READONLY, Lowercase(TWPDOCUMENTOBJECT_READONLY_VALUE[oSection.ReadOnly]));
  If oSection.Display <> DisplayNone Then
    FFormatter.Attributes.Add(ATTR_NAME_DISPLAYTYPE, NAMES_WPDocumentSectionDISPLAYTYPE[oSection.Display]);
  FFormatter.ProduceOpen(TAG_NAME_SECTION);

  WriteBlocks(oSection.Blocks);
  FFormatter.ProduceClose(TAG_NAME_SECTION);
End;


Procedure TWPNativeDocumentWriter.WriteBreak(oBreak : TWPDocumentBreak);
Begin
  FFormatter.Attributes.Add(ATTR_NAME_BREAKTYPE, WPDocumentBreakTYPE_NAMES[oBreak.BreakType]);
  FFormatter.Attributes.Add(ATTR_NAME_ALIGNMENT, NAMES_WPSALIGNMENT[oBreak.Alignment]);
  If oBreak.Width <> 1 Then
    FFormatter.Attributes.Add(ATTR_NAME_WIDTH, RealToString(oBreak.Width));
  If oBreak.PenWidth <> 0 Then
  Begin
    FFormatter.Attributes.Add(ATTR_NAME_PENCOLOUR, ColourToXMLColourString(oBreak.PenColour));
    FFormatter.Attributes.Add(ATTR_NAME_PENWIDTH, IntegerToString(oBreak.PenWidth));
    FFormatter.Attributes.Add(ATTR_NAME_PENSTYLE, ADVPENSTYLE_CODES[oBreak.PenStyle]);
    FFormatter.Attributes.Add(ATTR_NAME_PENENDSTYLE, ADVPENENDSTYLE_CODES[oBreak.EndStyle]);
  End;
  FFormatter.ProduceTag(TAG_NAME_BREAK);
End;

Procedure TWPNativeDocumentWriter.WriteTableItemAttributes(oTableItem : TWPDocumentTableItem);
Begin
  WriteBorder(TAG_NAME_LEFTBORDER, oTableItem.LeftBorder);
  WriteBorder(TAG_NAME_RIGHTBORDER, oTableItem.RightBorder);
  WriteBorder(TAG_NAME_TOPBORDER, oTableItem.TopBorder);
  WriteBorder(TAG_NAME_BOTTOMBORDER, oTableItem.BottomBorder);
End;


Procedure TWPNativeDocumentWriter.WriteTable(oTable : TWPDocumentTable);
Var
  iLoop : Integer;
Begin
  If oTable.ReadOnly <> ReadOnlyDefault Then
    FFormatter.Attributes.Add(ATTR_NAME_READONLY, Lowercase(TWPDOCUMENTOBJECT_READONLY_VALUE[oTable.ReadOnly]));
  If oTable.BorderPolicy <> BorderPolicyNone Then
    FFormatter.Attributes.Add(ATTR_NAME_BORDERPOLICY, CODES_TWPDocumentTableBORDERPOLICY[oTable.BorderPolicy]);
  If oTable.Background <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_BACKGROUND, ColourToXMLColourString(oTable.Background));
  If oTable.HorizontalMargin <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_HORIZONTAL_MARGIN, IntegerToString(oTable.HorizontalMargin));
  If oTable.VerticalMargin <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_VERTICAL_MARGIN, IntegerToString(oTable.VerticalMargin));
  FFormatter.Attributes.Add(ATTR_NAME_EXPAND_LAST, BooleanToString(oTable.ExpandLastColumn));
  FFormatter.ProduceOpen(TAG_NAME_TABLE);
  WriteTableItemAttributes(oTable);
  WriteBorder(TAG_NAME_CENTERHORIZONTALBORDER, oTable.CenterHorizontalBorder);
  WriteBorder(TAG_NAME_CENTERVERTICALBORDER, oTable.CenterVerticalBorder);

  For iLoop := 0 To oTable.Rows.Count - 1 Do
    WriteTableRow(oTable.Rows[iLoop]);

  FFormatter.ProduceClose(TAG_NAME_TABLE);
End;


Procedure TWPNativeDocumentWriter.WriteTableRow(oTableRow : TWPDocumentTableRow);
Var
  iLoop : Integer;
Begin
  SetHotspotAttributes(oTableRow);
  If oTableRow.ReadOnly <> ReadOnlyDefault Then
    FFormatter.Attributes.Add(ATTR_NAME_READONLY, Lowercase(TWPDOCUMENTOBJECT_READONLY_VALUE[oTableRow.ReadOnly]));
  If oTableRow.Header Then
    FFormatter.Attributes.Add(ATTR_NAME_HEADER, BooleanToString(oTableRow.Header));
  If oTableRow.BreakBefore Then
    FFormatter.Attributes.Add(ATTR_NAME_BREAK_BEFORE, BooleanToString(oTableRow.BreakBefore));
  If oTableRow.LowerPaddingSize > 0 Then
    FFormatter.Attributes.Add(ATTR_NAME_LOWER_PADDING_SIZE, IntegerToString(oTableRow.LowerPaddingSize));
  If oTableRow.LowerPaddingColour <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_LOWER_PADDING_COLOUR, ColourToXMLColourString(oTableRow.LowerPaddingColour));
  If oTableRow.Background <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_BACKGROUND, ColourToXMLColourString(oTableRow.Background));
  FFormatter.ProduceOpen(TAG_NAME_TABLE_ROW);

  For iLoop := 0 To oTableRow.Cells.Count - 1 Do
    WriteTableCell(oTableRow.Cells[iLoop]);
  For iLoop := 0 To oTableRow.Rows.Count - 1 Do
    WriteTableRow(oTableRow.Rows[iLoop]);

  FFormatter.ProduceClose(TAG_NAME_TABLE_ROW);
End;

Procedure TWPNativeDocumentWriter.WriteTableCell(oTableCell : TWPDocumentTableCell);
Var
  iLoop : Integer;
Begin
  SetHotspotAttributes(oTableCell);
  If oTableCell.ReadOnly <> ReadOnlyDefault Then
    FFormatter.Attributes.Add(ATTR_NAME_READONLY, Lowercase(TWPDOCUMENTOBJECT_READONLY_VALUE[oTableCell.ReadOnly]));
  If oTableCell.Span <> 1 Then
    FFormatter.Attributes.Add(ATTR_NAME_SPAN, IntegerToString(oTableCell.Span));
  If oTableCell.Width <> 0 Then
    FFormatter.Attributes.Add(ATTR_NAME_WIDTH, RealToString(oTableCell.Width));
  If oTableCell.Background <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_BACKGROUND, ColourToXMLColourString(oTableCell.Background));
  If oTableCell.MarginLeft <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_MARGIN_LEFT, IntegerToString(oTableCell.MarginLeft));
  If oTableCell.MarginRight <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_MARGIN_RIGHT, IntegerToString(oTableCell.MarginRight));
  If oTableCell.MarginTop <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_MARGIN_TOP, IntegerToString(oTableCell.MarginTop));
  If oTableCell.MarginBottom <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_MARGIN_BOTTOM, IntegerToString(oTableCell.MarginBottom));
  If oTableCell.VerticalAlignment <> VerticalAlignmentTop Then
    FFormatter.Attributes.Add(ATTR_NAME_VERTICALALIGNMENT, NAMES_WORDPROCESSORVERTICALALIGNMENT[oTableCell.VerticalAlignment]);
  FFormatter.ProduceOpen(TAG_NAME_TABLE_CELL);
  WriteTableItemAttributes(oTableCell);

  For iLoop := 0 To oTableCell.Paragraphs.Count - 1 Do
    WriteParagraph(oTableCell.Paragraphs[iLoop]);
  FFormatter.ProduceClose(TAG_NAME_TABLE_CELL);
End;

Procedure TWPNativeDocumentWriter.WriteParagraph(oParagraph : TWPDocumentParagraph);
Begin
  SetParagraphAttributes(oParagraph.Format);
  SetStyleAttributes(oParagraph.Style, oParagraph.Font);
  FFormatter.ProduceOpen(TAG_NAME_PARAGRAPH);

  WriteContents(oParagraph.Contents);

  FFormatter.ProduceClose(TAG_NAME_PARAGRAPH);
End;

Procedure TWPNativeDocumentWriter.WriteContents(oContents : TWPDocumentContents);
Var
  iLoop : Integer;
  oContent : TWPDocumentContent;
Begin
  For iLoop := 0 To oContents.Count - 1 Do
  Begin
    oContent := oContents[iLoop];
    If oContent Is TWPDocumentText Then
      WriteText(TWPDocumentText(oContent))
    Else If oContent Is TWPDocumentImage Then
      WriteImage(TWPDocumentImage(oContent))
    Else If oContent Is TWPDocumentLineBreak Then
      WriteLineBreak(TWPDocumentLineBreak(oContent))
    Else If oContent Is TWPDocumentField Then
      WriteField(TWPDocumentField(oContent))
  End;
End;


Procedure TWPNativeDocumentWriter.WriteField(oField : TWPDocumentField);
Begin
  If oField.Namespace <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_NAMESPACE, oField.Namespace);
  If oField.Name <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_NAME, oField.Name);
  If oField.Deletable Then
    FFormatter.Attributes.Add(ATTR_NAME_DELETABLE, BooleanToString(oField.Deletable));
  If oField.RawData <> Nil Then
  Begin
    FFormatter.Attributes.Add(ATTR_NAME_DATA, oField.RawData.AsText);
    If oField.DataValue['Mask'] <> ''  Then
      FFormatter.Attributes.Add(ATTR_NAME_MASK, oField.DataValue['Mask']);
  End;

  FFormatter.Attributes.Add(ATTR_NAME_FIXEDFORMAT, TWPDOCUMENTOBJECT_FIXED_FORMAT[oField.FixedFormat]);
  SetHotspotAttributes(oField);
  If oField.ReadOnly <> ReadOnlyDefault Then
    FFormatter.Attributes.Add(ATTR_NAME_READONLY, Lowercase(TWPDOCUMENTOBJECT_READONLY_VALUE[oField.ReadOnly]));
  SetStyleAttributes(oField.Style, oField.Font);
  FFormatter.ProduceOpen(TAG_NAME_FIELD);
  WriteContents(oField.Contents);
  FFormatter.ProduceClose(TAG_NAME_FIELD);
End;


Procedure TWPNativeDocumentWriter.WriteLineBreak(oLineBreak : TWPDocumentLineBreak);
Begin
  SetStyleAttributes(oLineBreak.Style, oLineBreak.Font);
  FFormatter.ProduceTag(TAG_NAME_LINEBREAK);
End;

Procedure TWPNativeDocumentWriter.WriteText(oText : TWPDocumentText);
Begin
  SetStyleAttributes(oText.Style, oText.Font);
  FFormatter.ProduceText(TAG_NAME_TEXT, oText.Value);
End;


Procedure TWPNativeDocumentWriter.WriteImage(oImage : TWPDocumentImage);
Var
  oBuffer : TFslBuffer;
  oSelectionBuffer : TFslBuffer;
  sExt : String;
Begin
  Assert(CheckCondition(Assigned(oImage.Image), 'WriteImage', 'image.image is nil'));

  oSelectionBuffer := Nil;
  oBuffer := TFslBuffer.Create;
  Try
    sExt := SaveImageToBuffer(oImage, oImage.Image, oBuffer);
    If oImage.HasSelectionImage Then
      oSelectionBuffer := TFslBuffer.Create;
    Try
      If oImage.HasSelectionImage Then
        SaveImageToBuffer(oImage, oImage.SelectionImage, oBuffer);

      WriteImageSource(oImage, oBuffer, oSelectionBuffer, sExt);
    Finally
      oSelectionBuffer.Free;
    End;
  Finally
    oBuffer.Free;
  End;
End;

Procedure TWPNativeDocumentWriter.WriteImageSource(oImage : TWPDocumentImage; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
Begin
  SetImageAttributes(oImage);
  FFormatter.Attributes.Add(ATTR_NAME_NAME, oImage.Name);
  FFormatter.Attributes.Add(ATTR_NAME_FORMAT, sExt);
  FFormatter.ProduceOpen(TAG_NAME_IMAGE);
  FFormatter.ProduceText(TAG_NAME_DATA, EncodeBase64(oBuffer.AsBytes));
  If Assigned(oSelectionBuffer) Then
    FFormatter.ProduceText(TAG_NAME_SELECTION, EncodeBase64(oBuffer.AsBytes));
  If oImage.Map <> Nil Then
    WriteImageMap(oImage.Map);
  FFormatter.ProduceClose(TAG_NAME_IMAGE);
End;




{
Constructor TWPNativeDocumentWriter.Create;
Begin
  Inherited;

  FInternalFormatter := TFslXMLFormatter.Create;
  FInternalFormatter.HasWhitespace := True;
  FExternalFormatter := Nil;
End;



Procedure TWPNativeDocumentWriter.Initialise;
Begin
  Inherited;

  If Assigned(FExternalFormatter) Then
  Begin
    FActiveFormatter := FExternalFormatter;
  End
  Else
  Begin
    FActiveFormatter := FInternalFormatter;
    FFormatter.Clear;
    FFormatter.Stream := Stream.Link;

    FFormatter.Attributes.Add(ATTR_NAME_XMLNS, NAMESPACE_WP_NATIVE);
    FFormatter.Attributes.Add(ATTR_NAME_VERSION, VERSION_NATIVE_TWO);
    FFormatter.ProduceOpen(TAG_NAME_STREAM);
  End;

  FTextOpen := False;

  If EmbedStyles Then
    WriteStyles;
End;


Procedure TWPNativeDocumentWriter.Finalise;
Begin
  If FTextOpen Then
    CloseText;

  If FActiveFormatter = FInternalFormatter Then
  Begin
    FFormatter.Stream := Nil;
  End;

  Inherited;
End;












Procedure TWPNativeDocumentWriter.WriteImageReference(oImage : TWPDocumentImage; oBuffer, oSelectionBuffer : TFslBuffer; Const sExt : String);
Var
  sName : String;
  sSelectionName : String;
Begin
  Assert(CheckCondition(Assigned(OnSaveImage), 'WriteImageReference', 'OnSaveImage is not assigned'));
  sName := oImage.Name;
  OnSaveImage(Self, oBuffer, sExt, sName);
  If Assigned(oSelectionBuffer) Then
  Begin
    sSelectionName := sName+'-selection';
    OnSaveImage(Self, oSelectionBuffer, sExt, sSelectionName);
  End;
  If sName <> '' Then
    Begin
    oImage.Name := sName;
    SetImageAttributes(oImage);
    FFormatter.Attributes.Add(ATTR_NAME_IMAGEREF, sName);
    If Assigned(oSelectionBuffer) Then
      FFormatter.Attributes.Add(ATTR_NAME_IMAGEREF_SELECTION, sSelectionName);
    If oImage.hasImageMap Then
    Begin
      FFormatter.ProduceOpen(TAG_NAME_IMAGE);
      WriteImageMap(oImage.ImageMap);
      FFormatter.ProduceClose(TAG_NAME_IMAGE);
    End
    Else
      FFormatter.ProduceTag(TAG_NAME_IMAGE);
    End;
End;




Function TWPNativeDocumentWriter.GetFormatter: TFslXMLFormatter;
Begin
  Result := FExternalFormatter;
End;




Function TWPNativeDocumentWriter.SupportsNestedRows: Boolean;
Begin
  Result := True;
End;

}

Procedure TWPNativeDocumentWriter.SetParagraphAttributes(oFormat : TWPSParagraphDetails);
Begin
  If oFormat.Align <> WordProcessorParagraphAlignmentUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_ALIGN, NAMES_WPSPARAGRAPHALIGNMENT[oFormat.Align]);
  If (oFormat.LeftIndent <> 0) And (oFormat.LeftIndent <> DEF_WORD) Then
    FFormatter.Attributes.Add(ATTR_NAME_LEFTINDENT, IntegerToString(oFormat.LeftIndent));
  If (oFormat.RightIndent <> 0) And (oFormat.RightIndent <> DEF_WORD) Then
    FFormatter.Attributes.Add(ATTR_NAME_RIGHTINDENT, IntegerToString(oFormat.RightIndent));
  If oFormat.BulletType <> tbUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_BULLETTYPE, NAMES_WPSPARAGRAPHBULLETTYPE[oFormat.BulletType]);
  If oFormat.ListType <> WPSParagraphListTypeUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_LISTTYPE, NAMES_WPSPARAGRAPHLISTTYPE[oFormat.ListType]);
  If oFormat.NumberType <> tnUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_NUMBERTYPE, NAMES_WPSPARAGRAPHNUMBERTYPE[oFormat.NumberType]);
  If oFormat.NumberFormat <> nwUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_NUMBERFORMAT, NAMES_WPSPARAGRAPHNUMBERFORMAT[oFormat.NumberFormat]);
  If oFormat.FixedNumber <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_FIXEDNUMBER, IntegerToString(oFormat.FixedNumber));
  If oFormat.MarginBottom <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_MARGIN_BOTTOM, IntegerToString(oFormat.MarginBottom));
End;


Procedure TWPNativeDocumentWriter.SetStyleAttributes(Const sStyle : String; oFont : TWPSFontDetails);
Begin
  If sStyle <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_STYLE, sStyle);
  If (oFont <> Nil) Then
    SetFontAttributes(oFont);
End;

Procedure TWPNativeDocumentWriter.SetFontAttributes(oFont : TWPSFontDetails);
Begin
  If oFont.Name <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_FONTNAME, oFont.Name);
  If oFont.Size <> DEF_WORD Then
    FFormatter.Attributes.Add(ATTR_NAME_SIZE, IntegerToString(oFont.Size));
  If oFont.Bold <> tsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_BOLD, NAMES_WPSTRISTATE[oFont.Bold]);
  If oFont.Italic <> tsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_ITALIC, NAMES_WPSTRISTATE[oFont.Italic]);
  If oFont.Underline <> tsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_UNDERLINE, NAMES_WPSTRISTATE[oFont.Underline]);
  If oFont.Strikethrough <> tsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_STRIKETHROUGH, NAMES_WPSTRISTATE[oFont.Strikethrough]);
  If oFont.State <> fsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_FONTSTATE, NAMES_WPSFONTSTATE[oFont.State]);

  If oFont.Foreground <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_FOREGROUND, ColourToXMLColourString(oFont.Foreground));
  If oFont.Background <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_BACKGROUND, ColourToXMLColourString(oFont.Background));
  If oFont.Capitalization <> fcsUnknown Then
    FFormatter.Attributes.Add(ATTR_NAME_CAPITALIZATION, NAMES_WPSCAPSSTATE[oFont.Capitalization]);
End;


Procedure TWPNativeDocumentWriter.SetHotspotAttributes(o : TWPDocumentObject);
Begin
  If (o.HasHotspot) Then
    SetHotspotAttributes(o.Hotspot);
End;

Procedure TWPNativeDocumentWriter.SetHotspotAttributes(oHotspot : TWPHotspot);
Begin
  If oHotspot.URL <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_URL, oHotspot.URL);
  If oHotspot.Title <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_TITLE, oHotspot.Title);
  If oHotspot.Key <> '' Then
    FFormatter.Attributes.Add(ATTR_NAME_KEY, oHotspot.Key);
  If oHotspot.LinkColour <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_LINKCOLOUR, ColourToXMLColourString(oHotspot.LinkColour));
  If oHotspot.HoverColour <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_HOVERCOLOUR, ColourToXMLColourString(oHotspot.HoverColour));
End;


Procedure TWPNativeDocumentWriter.WriteStyles(oStyles : TWPStyles);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oStyles.count - 1 Do
    WriteStyle(oStyles[iLoop]);
End;


Procedure TWPNativeDocumentWriter.WriteStyle(oStyle: TWPStyle);
Begin
  FFormatter.Attributes.Add(ATTR_NAME_STYLE, oStyle.Name);
  SetFontAttributes(oStyle.Font);
  SetParagraphAttributes(oStyle.Paragraph);
  FFormatter.ProduceTag(TAG_NAME_STYLE);
End;


Procedure TWPNativeDocumentWriter.WriteBorder(Const sName : String; oBorder : TWPBorder);
Var
  oMem : TFslMemoryStream;
Begin
  If oBorder.Defined Then
    Begin
    FFormatter.Attributes.Add(ATTR_NAME_DEFINED, BooleanToString(oBorder.Defined));
    FFormatter.Attributes.Add(ATTR_NAME_FANCY, BooleanToString(oBorder.Fancy));
    If oBorder.Colour <> DEF_COLOUR Then
      FFormatter.Attributes.Add(ATTR_NAME_COLOUR, ColourToXMLColourString(oBorder.Colour));
    If oBorder.OuterColour <> DEF_COLOUR Then
      FFormatter.Attributes.Add(ATTR_NAME_OUTERCOLOUR, ColourToXMLColourString(oBorder.OuterColour));
    If oBorder.OuterColour2 <> DEF_COLOUR Then
      FFormatter.Attributes.Add(ATTR_NAME_OUTERCOLOUR2, ColourToXMLColourString(oBorder.OuterColour2));
    If oBorder.Width <> 0 Then
      FFormatter.Attributes.Add(ATTR_NAME_WIDTH, IntegerToString(oBorder.Width));
    If oBorder.Style <> apsSolid Then
      FFormatter.Attributes.Add(ATTR_NAME_STYLE, ADVPENSTYLE_CODES[oBorder.Style]);
    If oBorder.LowOuterlimit <> DEF_WORD Then
      FFormatter.Attributes.Add(ATTR_NAME_LOW_OUTER, IntegerToString(oBorder.LowOuterlimit));
    If oBorder.HighOuterLimit <> DEF_WORD Then
      FFormatter.Attributes.Add(ATTR_NAME_HIGH_OUTER, IntegerToString(oBorder.HighOuterLimit));

    If oBorder.BrushImage <> Nil Then
    Begin
      FFormatter.ProduceOpen(sName);
      oMem := TFslMemoryStream.Create;
      Try
        oBorder.BrushImage.SaveToStream(oMem);
        FFormatter.ProduceText(TAG_NAME_BRUSH, EncodeBase64(oMem.Buffer.AsBytes));
      Finally
        oMem.Free;
      End;
      FFormatter.ProduceClose(sName);
    End
    Else
      FFormatter.ProduceTag(sName);
    End;
End;

Procedure SavePNGToStream(oImage: TFslVCLGraphic; oStream: TFslStream);
Var
  oPNG : TPngObject;
  oAdaptor : TVCLStream;
Begin
  oPNG := TPngObject.Create;
  Try
    oPNG.Assign(oImage.Handle);

    oAdaptor := TVCLStream.Create;
    Try
      oAdaptor.Stream := oStream.Link;

      oPNG.SaveToStream(oAdaptor);
    Finally
      oAdaptor.Free;
    End;
  Finally
    oPng.Free;
  End;
End;


Function TWPNativeDocumentWriter.SaveImageToBuffer(oImage : TWPDocumentImage; oImg : TFslGraphic; oBuffer : TFslBuffer) : String;
Var
  oMemory : TFslMemoryStream;
Begin
  oMemory := TFslMemoryStream.Create;
  Try
    oMemory.Buffer := oBuffer.Link;
    oMemory.Expand := True;

    if not (oImg is TFslVCLGraphic) then
      raise EWPException.create('Not done yet')
    else If TFslVCLGraphic(oImg).Handle Is TJPEGImage Then
    Begin
      oImg.SaveToStream(oMemory);
      Result := 'jpg';
    End
    Else
    Begin
      SavePNGToStream(TFslVCLGraphic(oImg), oMemory);
      Result := 'png';
    End;
  Finally
    oMemory.Free;
  End;
End;

Procedure TWPNativeDocumentWriter.SetImageAttributes(oImage : TWPDocumentImage);
Begin
  SetStyleAttributes(oImage.Style, oImage.Font);
  SetHotspotAttributes(oImage);
  If oImage.BorderWidth <> 0 Then
  Begin
    FFormatter.Attributes.Add(ATTR_NAME_BORDERWIDTH, IntegerToString(oImage.BorderWidth));
    If oImage.BorderColour <> DEF_COLOUR Then
      FFormatter.Attributes.Add(ATTR_NAME_BORDERCOLOR, ColourToXMLColourString(oImage.BorderColour));
  End;
  FFormatter.Attributes.Add(ATTR_NAME_HEIGHT, IntegerToString(oImage.ImageHeight));
  FFormatter.Attributes.Add(ATTR_NAME_WIDTH, IntegerToString(oImage.ImageWidth));
  FFormatter.Attributes.Add(ATTR_NAME_VERTICALALIGNMENT, NAMES_IMAGEVERTICALALIGNMENT[oImage.VerticalAlignment]);
  FFormatter.Attributes.Add(ATTR_NAME_SIZEPOLICY, NAMES_IMAGESIZEPOLICY[oImage.SizePolicy]);
  If oImage.TransparentColour <> DEF_COLOUR Then
    FFormatter.Attributes.Add(ATTR_NAME_TRANSPARENTCOLOR, ColourToXMLColourString(oImage.TransparentColour));
End;

Procedure TWPNativeDocumentWriter.WriteImageMap(oMap : TWPImageMap);
Var
  iLoop : Integer;
Begin
  FFormatter.ProduceOpen(TAG_NAME_MAP);
  For iLoop := 0 To oMap.Areas.count - 1 Do
    WriteImageMapArea(oMap.Areas[iLoop]);
  FFormatter.ProduceClose(TAG_NAME_MAP);
End;


Procedure TWPNativeDocumentWriter.WriteImageMapArea(oArea : TWPImageMapArea);
Var
  iLoop : Integer;
Begin
  SetHotspotAttributes(oArea);
  FFormatter.ProduceOpen(TAG_NAME_AREA);
  For iLoop := 0 To oArea.Coordinates.count - 1 Do
    WriteImageMapCoordinate(oArea.Coordinates[iLoop]);
  FFormatter.ProduceClose(TAG_NAME_AREA);
End;


Procedure TWPNativeDocumentWriter.WriteImageMapCoordinate(oCoordinate : TWPCoordinate);
Begin
  FFormatter.Attributes.Add(ATTR_NAME_X, IntegerToString(oCoordinate.X));
  FFormatter.Attributes.Add(ATTR_NAME_Y, IntegerToString(oCoordinate.Y));
  FFormatter.ProduceTAG(TAG_NAME_COORD);
End;

function TWPNativeDocumentWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFormatter.sizeInBytes);
end;

procedure TWPNativeReader.ReadAdornment(oAdornments: TWPDocumentImageAdornments);
var
  oAdornment : TWPDocumentImageAdornment;
begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_ADORNMENT);
  oAdornment := TWPDocumentImageAdornment.Create;
  Try
    oAdornment.PenColour := XMLColourStringToColourOrDefault(FActiveExtractor.Attributes[ATTR_NAME_PENCOLOUR], clBlack);
    oAdornment.PenWidth := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_PENWIDTH], 1);
    oAdornment.PenStyle := TFslPenStyle(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_PENSTYLE], ADVPENSTYLE_CODES, ord(apsSolid)));
    oAdornment.Caption := FActiveExtractor.Attributes[ATTR_NAME_CAPTION];
    oAdornment.AdornmentType := TWPDocumentImageAdornmentType(ReadEnumeratedAttribute(FActiveExtractor.Attributes[ATTR_NAME_ADORN_TYPE], IMAGE_ADORNMENT_TYPE_CODES, ord(iatLine)));
    oAdornment.CaptionPoint.X := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_CAPTION_X], 10);
    oAdornment.CaptionPoint.Y := StrToIntDef(FActiveExtractor.Attributes[ATTR_NAME_CAPTION_Y], 10);
    ReadFontAttributes(oAdornment.Font);
    While Not FActiveExtractor.PeekIsClose Do
      ReadMapCoordinate(oAdornment.Coordinates);

    oAdornments.Add(oAdornment.Link);
  Finally
    oAdornment.Free;
  End;
  FActiveExtractor.ConsumeClose(TAG_NAME_ADORNMENT);
end;

procedure TWPNativeWriter.WriteImageAdornment(oAdornment: TWPDocumentImageAdornment);
var
  iLoop : Integer;
Begin
  FActiveFormatter.Attributes.Add(ATTR_NAME_PENCOLOUR, ColourToXMLColourString(oAdornment.PenColour));
  FActiveFormatter.Attributes.Add(ATTR_NAME_PENWIDTH, IntegerToString(oAdornment.PenWidth));
  FActiveFormatter.Attributes.Add(ATTR_NAME_PENSTYLE, ADVPENSTYLE_CODES[oAdornment.PenStyle]);
  FActiveFormatter.Attributes.Add(ATTR_NAME_CAPTION, oAdornment.Caption);
  FActiveFormatter.Attributes.Add(ATTR_NAME_CAPTION_X, IntegerToString(oAdornment.CaptionPoint.X));
  FActiveFormatter.Attributes.Add(ATTR_NAME_CAPTION_Y, IntegerToString(oAdornment.CaptionPoint.Y));
  FActiveFormatter.Attributes.Add(ATTR_NAME_ADORN_TYPE, IMAGE_ADORNMENT_TYPE_CODES[oAdornment.AdornmentType]);
  SetFontAttributes(oAdornment.Font);

  FActiveFormatter.ProduceOpen(TAG_NAME_ADORNMENT);
  For iLoop := 0 To oAdornment.Coordinates.count - 1 Do
    WriteImageMapCoordinate(oAdornment.Coordinates[iLoop]);
  FActiveFormatter.ProduceClose(TAG_NAME_ADORNMENT);
end;


procedure TWPNativeReader.ReadAnnotations(oDocument : TWPWorkingDocument);
Begin
  FAnnotationMap.Clear;
  FActiveExtractor.ConsumeOpen(TAG_NAME_ANNOTATIONS);

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
  Begin
    FActiveExtractor.ConsumeOpen(TAG_NAME_ANNOTATION + '/');
    oDocument.AllAnnotations.Add(FActiveExtractor.Attributes[ATTR_NAME_NAMESPACE], FActiveExtractor.Attributes[ATTR_NAME_VALUE]);
    FAnnotationMap.Add(FActiveExtractor.Attributes[ATTR_NAME_ID], oDocument.AllAnnotations.Count);
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_ANNOTATIONS);
end;

procedure TWPNativeReader.ReadAttachments(oDocument : TWPWorkingDocument);
var
  oAttach : TWPWorkingAttachment;
  oBuffer : TFslBuffer;
Begin
  FActiveExtractor.ConsumeOpen(TAG_NAME_ATTACHMENTS);

  While FActiveExtractor.More And FActiveExtractor.PeekIsOpen Do
  Begin
    FActiveExtractor.ConsumeOpen(TAG_NAME_ATTACHMENT);
    oAttach := TWPWorkingAttachment.Create;
    try
      oAttach.Id := FActiveExtractor.Attributes[ATTR_NAME_ID];
      oAttach.MimeType := FActiveExtractor.Attributes[ATTR_NAME_MIMETYPE];
      oAttach.Extension := FActiveExtractor.Attributes[ATTR_NAME_EXTENSION];
      if FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF] <> '' Then
      begin
        oBuffer := Nil;
        try
          OnLoadImage(Self, Context, FActiveExtractor.Attributes[ATTR_NAME_IMAGEREF], oBuffer);
          oAttach.LoadFromBuffer(oBuffer);
        finally
          oBuffer.Free;
        end
      end
      else
      oAttach.LoadFromString(ZDecompressStr(DecodeBase64(FActiveExtractor.ConsumeTextBody)));
      oDocument.Attachments.Add(oAttach.Link);
    finally
      oAttach.Free;
    end;
    FActiveExtractor.ConsumeClose(TAG_NAME_ATTACHMENT);
  End;

  FActiveExtractor.ConsumeClose(TAG_NAME_ATTACHMENTS);
end;

function TWPNativeReader.ResolveAnnotation(sAnnotation: String): Integer;
begin
  if (sAnnotation = '') or (sAnnotation = '0') then
    result := 0
  Else
    result := FAnnotationMap.Matches[sAnnotation];
end;

procedure TWPSnapshotReader.ExpectedName(oElement: TMXmlElement; const sName: String);
begin
  if oElement = nil Then
    RaiseError('ExpectedName', 'Unable to find XML element '+sName);
  if (oElement.Name <> sName) Then
    RaiseError('ExpectedName', 'Found XML Element '+oElement.Name+' expecting '+sName);
end;

Procedure TWPSnapshotReader.Read(oDocument : TWPWorkingDocument);
Var
  oDom : TMXmlDocument;
Begin
  Inherited;

  oDom := TMXmlParser.parse(Stream, [xpResolveNamespaces]);
  try
    ReadSnapshot(oDom.docElement, oDocument);
  Finally
    oDom.Free;
  End;
  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;



procedure TWPSnapshotReader.ReadSnapshot(oElement: TMXmlElement; oDocument: TWPWorkingDocument);
begin
  ExpectedName(oElement, 'snapshot');
  ReadStyles(oElement.element('Configured'));
  ReadDocument(oElement.element('document'), oDocument);
end;

procedure TWPSnapshotReader.ReadStyles(oElement: TMXmlElement);
var
  oChild : TMXmlElement;
  oStyle : TWPStyle;
begin
  ExpectedName(oElement, 'Configured');
  oChild := oElement.firstElement;
  While oChild <> Nil Do
  Begin
    Expectedname(oChild, 'Style');
    oStyle := TWPStyle.Create;
    Try
      ReadStyle(oChild, oStyle);
      Styles.Add(oStyle.Link);
    Finally
      oStyle.Free;
    End;
    oChild := oChild.nextElement;
  End;
end;

procedure TWPSnapshotReader.ReadStyle(oElement : TMXmlElement; oStyle : TWPStyle);
var
  oFormat : TMXmlElement;
begin
  oStyle.Name := oElement.Attribute['Name'];
  oFormat := oElement.element('format');
  ExpectedName(oFormat, 'format');
  oStyle.Font.Name := Attribute(oFormat, 'Name');
  oStyle.Font.Size := AttributeInt(oFormat, 'Size');
  oStyle.Font.Foreground := AttributeColour(oFormat, 'Foreground');
  oStyle.Font.Background := AttributeColour(oFormat, 'Background');
  oStyle.Font.Bold := AttributeTriState(oFormat, 'Bold');
  oStyle.Font.State := AttributeWPSFontState(oFormat, 'State');
  oStyle.Font.Italic := AttributeTriState(oFormat, 'Italic');
  oStyle.Font.Underline := AttributeTriState(oFormat, 'Underline');
  oStyle.Font.Strikethrough := AttributeTriState(oFormat, 'Strikethrough');
  oStyle.Font.Capitalization := AttributeWPSCapsState(oFormat, 'Capitalization');
  oFormat := oElement.element('para-format');
  ExpectedName(oFormat, 'para-format');
  oStyle.Paragraph.Align := AttributeWordProcessorParagraphAlignment(oFormat, 'Align');
  oStyle.Paragraph.BulletType := AttributeWPSParagraphBulletType(oFormat, 'BulletType');
  oStyle.Paragraph.NumberType := AttributeWPSParagraphNumberType(oFormat, 'NumberType');
  oStyle.Paragraph.ListType := AttributeWPSParagraphListType(oFormat, 'ListType');
  oStyle.Paragraph.FixedNumber := AttributeInt(oFormat, 'FixedNumber');
  oStyle.Paragraph.NumberFormat := AttributeWPSParagraphNumberFormat(oFormat, 'NumberFormat');
  oStyle.Paragraph.LeftIndent := AttributeInt(oFormat, 'LeftIndent');
  oStyle.Paragraph.RightIndent := AttributeInt(oFormat, 'RightIndent');
  oStyle.Paragraph.MarginBottom := AttributeInt(oFormat, 'MarginBottom');
end;

procedure TWPSnapshotReader.ReadDocument(oElement: TMXmlElement; oDocument: TWPWorkingDocument);
Var
  oChild : TMXmlElement;
begin
  ExpectedName(oElement, 'document');

  oChild := oElement.firstElement;
  While oChild <> Nil Do
  Begin
    If (oChild.Name = 'text') Then
      ReadPieceText(oChild, oDocument)
    Else If (oChild.Name = 'Image') Then
      ReadPieceImage(oChild, oDocument)
    Else If (oChild.Name = 'field-start') Then
      ReadPieceFieldStart(oChild, oDocument)
    Else If (oChild.Name = 'field-stop') Then
      ReadPieceFieldStop(oChild, oDocument)
    Else If (oChild.Name = 'line-break') Then
      ReadPieceLineBreak(oChild, oDocument)
    Else If (oChild.Name = 'break') Then
      ReadPieceBreak(oChild, oDocument)
    Else If (oChild.Name = 'para') Then
      ReadPiecePara(oChild, oDocument)
    Else If (oChild.Name = 'table') Then
      ReadPieceTableStart(oChild, oDocument)
    Else If (oChild.Name = 'table-row') Then
      ReadPieceRowStart(oChild, oDocument)
    Else If (oChild.Name = 'table-cell') Then
      ReadPieceCellStart(oChild, oDocument)
    Else If (oChild.Name = 'stop') Then
      ReadPieceStop(oChild, oDocument)
    Else If (oChild.Name = 'section-start') Then
      ReadPieceSectionStart(oChild, oDocument)
    Else
      RaiseError('ReadDocument', 'unknown element '+oChild.Name);
    oChild := oChild.nextElement;
  End;
end;


Function TWPSnapshotReader.Attribute(oElement : TMXmlElement; Const sName : String):String;
Begin
  if oElement.Attribute[sName] <> '' Then
    result := oElement.Attribute[sName]
  Else
    RaiseError('Attribute', 'Element '+oElement.Name+' does not have an attribute '+sName);
End;

Function TWPSnapshotReader.AttributeInt(oElement : TMXmlElement; Const sName : String):Integer;
Begin
  result := StringToInteger32(Attribute(oElement, sName));
End;

Function TWPSnapshotReader.AttributeReal(oElement : TMXmlElement; Const sName : String):Real;
Begin
  result := StringToReal(Attribute(oElement, sName));
End;

Function TWPSnapshotReader.AttributeBool(oElement : TMXmlElement; Const sName : String):Boolean;
Begin
  result := StringToBoolean(Attribute(oElement, sName));
End;

Function TWPSnapshotReader.AttributeColour(oElement : TMXmlElement; Const sName : String):TColour;
Begin
  result := XMLColourStringToColour(Attribute(oElement, sName));
End;

Function StringArrayToCommaString(Const sNames : Array of String) : String;
var
  iLoop : Integer;
Begin
  Result := sNames[0];
  For iLoop := 1 to High(sNames) Do
    result := result +', '+sNames[iLoop];
End;

Function TWPSnapshotReader.ReadEnum(Const sValue : String; Const sNames : Array of String) : Integer;
Begin
  result := StringArrayIndexOfSensitive(sNames, sValue);
  if result = -1 Then
    RaiseError('ReadEnum', 'Value '+sValue+' is not a valid option (expected one of '+StringArrayToCommaString(sNames)+')');

End;

Function TWPSnapshotReader.AttributeTriState(oElement : TMXmlElement; Const sName : String):TWPSTriState;
Begin
  Result := TWPSTriState(ReadEnum(Attribute(oElement, sName), NAMES_WPSTRISTATE));
End;

Function TWPSnapshotReader.AttributeWPSFontState(oElement : TMXmlElement; Const sName : String):  TWPSFontState;
Begin
  Result := TWPSFontState(ReadEnum(Attribute(oElement, sName), NAMES_WPSFONTSTATE));
End;

Function TWPSnapshotReader.AttributeWPSCapsState(oElement : TMXmlElement; Const sName : String):  TWPSCapsState;
Begin
  Result := TWPSCapsState(ReadEnum(Attribute(oElement, sName), NAMES_WPSCAPSSTATE));
End;

Function TWPSnapshotReader.AttributeWPSParagraphBulletType(oElement : TMXmlElement; Const sName : String):  TWPSParagraphBulletType;
Begin
  Result := TWPSParagraphBulletType(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHBULLETTYPE));
End;

Function TWPSnapshotReader.AttributeWPSParagraphNumberType(oElement : TMXmlElement; Const sName : String):  TWPSParagraphNumberType;
Begin
  Result := TWPSParagraphNumberType(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHNUMBERTYPE));
End;

Function TWPSnapshotReader.AttributeWPSParagraphNumberFormat(oElement : TMXmlElement; Const sName : String):  TWPSParagraphNumberFormat;
Begin
  Result := TWPSParagraphNumberFormat(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHNUMBERFORMAT));
End;

Function TWPSnapshotReader.AttributeWordProcessorAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorAlignment;
Begin
  Result := TWordProcessorAlignment(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHALIGNMENT));
End;

Function TWPSnapshotReader.AttributeWordProcessorVerticalAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorVerticalAlignment;
Begin
  Result := TWordProcessorVerticalAlignment(ReadEnum(Attribute(oElement, sName), NAMES_WORDPROCESSORVERTICALALIGNMENT));
End;

Function TWPSnapshotReader.AttributeWordProcessorParagraphAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorParagraphAlignment;
Begin
  Result := TWordProcessorParagraphAlignment(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHALIGNMENT));
End;

Function TWPSnapshotReader.AttributeWordProcessorImageVerticalAlignment(oElement : TMXmlElement; Const sName : String):TWordProcessorImageVerticalAlignment;
Begin
  Result := TWordProcessorImageVerticalAlignment(ReadEnum(Attribute(oElement, sName), NAMES_WORDPROCESSORIMAGEVERTICALALIGNMENT));
End;

Function TWPSnapshotReader.AttributeWPSParagraphListType(oElement : TMXmlElement; Const sName : String):TWPSParagraphListType;
Begin
  Result := TWPSParagraphListType(ReadEnum(Attribute(oElement, sName), NAMES_WPSPARAGRAPHLISTTYPE));
End;


Function TWPSnapshotReader.AttributeWPWorkingDocumentBreakPieceType(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentBreakPieceType;
Begin
  Result := TWPWorkingDocumentBreakPieceType(ReadEnum(Attribute(oElement, sName), WPWorkingDocumentBreakPIECETYPE_NAMES));
End;

Function TWPSnapshotReader.AttributeFslPenStyle(oElement : TMXmlElement; Const sName : String):TFslPenStyle;
Begin
  Result := TFslPenStyle(ReadEnum(Attribute(oElement, sName), ADVPENSTYLE_CODES));
End;

Function TWPSnapshotReader.AttributeFslPenEndStyle(oElement : TMXmlElement; Const sName : String):TFslPenEndStyle;
Begin
  Result := TFslPenEndStyle(ReadEnum(Attribute(oElement, sName), ADVPENENDSTYLE_CODES));
End;

Function TWPSnapshotReader.AttributeWPWorkingDocumentTableBorderPolicy(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentTableBorderPolicy;
Begin
  Result := TWPWorkingDocumentTableBorderPolicy(ReadEnum(Attribute(oElement, sName), CODES_TWPWorkingDocumentTableBORDERPOLICY));
End;

Function TWPSnapshotReader.AttributeWPWorkingDocumentStopType(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentStopType;
Begin
  Result := TWPWorkingDocumentStopType(ReadEnum(Attribute(oElement, sName), NAMES_WorkingDocumentStopType));
End;

Function TWPSnapshotReader.AttributeWPWorkingDocumentSectionDisplayTyp(oElement : TMXmlElement; Const sName : String):TWPWorkingDocumentSectionDisplayType;
Begin
  Result := TWPWorkingDocumentSectionDisplayType(ReadEnum(Attribute(oElement, sName), NAMES_WPWorkingDocumentSectionDISPLAYTYPE));
End;




Procedure TWPSnapshotReader.ReadPieceText(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentTextPiece;
Begin
  oPiece := TWPWorkingDocumentTextPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oPiece.Content := Attribute(oElement, 'Content');
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPSnapshotReader.ReadPieceImage(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentImagePiece;
Begin
  oPiece := TWPWorkingDocumentImagePiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oPiece.Border := AttributeInt(oElement, 'Border');
    oPiece.Height := AttributeInt(oElement, 'Height');
    oPiece.Width := AttributeInt(oElement, 'Width');
    oPiece.BorderColour := AttributeColour(oElement, 'BorderColour');
    oPiece.Name := Attribute(oElement, 'Name');
    oPiece.TransparentColour := AttributeColour(oElement, 'TransparentColour');
    oPiece.VerticalAlignment := AttributeWordProcessorImageVerticalAlignment(oElement, 'VerticalAlignment');
    // TODO: what about the actual image?
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceFieldStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentFieldStartPiece;
Begin
  oPiece := TWPWorkingDocumentFieldStartPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oPiece.FixedFormat := ReadFixedFormatAttribute(Attribute(oElement, 'FixedFormat'));
    oPiece.Deletable := AttributeBool(oElement, 'Deletable');
    oPiece.Namespace := Attribute(oElement, 'Namespace');
    oPiece.Name := Attribute(oElement, 'Name');
    oPiece.ReadOnly := AttributeTriState(oElement, 'ReadOnly');
    // TODO: raw data:  ProduceData(oPiece.RawData);
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceFieldStop(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentFieldStopPiece;
Begin
  oPiece := TWPWorkingDocumentFieldStopPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceLineBreak(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentLineBreakPiece;
Begin
  oPiece := TWPWorkingDocumentLineBreakPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceBreak(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentBreakPiece;
Begin
  oPiece := TWPWorkingDocumentBreakPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.BreakType := AttributeWPWorkingDocumentBreakPieceType(oElement, 'BreakType');
    oPiece.Alignment := AttributeWordProcessorAlignment(oElement, 'Alignment');
    oPiece.Width := AttributeReal(oElement, 'Width');
    oPiece.PenColour := AttributeColour(oElement, 'PenColour');
    oPiece.PenWidth := AttributeInt(oElement, 'PenWidth');
    oPiece.PenStyle := AttributeFslPenStyle(oElement, 'PenStyle');
    oPiece.EndStyle := AttributeFslPenEndStyle(oElement, 'EndStyle');
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPSnapshotReader.ReadPiecePara(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentParaPiece;
Begin
  oPiece := TWPWorkingDocumentParaPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.ListNumber := AttributeInt(oElement, 'ListNumber');
    oPiece.WorkingRightIndent := AttributeInt(oElement, 'WorkingRightIndent');
    oPiece.WorkingLeftIndent := AttributeInt(oElement, 'WorkingLeftIndent');
    oPiece.SpeechMagicDouble := AttributeBool(oElement, 'SpeechMagicDouble');
    ReadParaFormat(oElement.element('para-format'), oPiece);
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceTableStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentTableStartPiece;
Begin
  oPiece := TWPWorkingDocumentTableStartPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.BorderPolicy := AttributeWPWorkingDocumentTableBorderPolicy(oElement, 'BorderPolicy');
    oPiece.HorizontalMargin := AttributeInt(oElement, 'HorizontalMargin');
    oPiece.VerticalMargin := AttributeInt(oElement, 'VerticalMargin');
    oPiece.Background := AttributeColour(oElement, 'Background');
    oPiece.ColumnCount := AttributeInt(oElement, 'ColumnCount');
    oPiece.Jagged := AttributeBool(oElement, 'Jagged');
    ReadBorder(oElement, 'CenterHorizontalBorder', oPiece.CenterHorizontalBorder);
    ReadBorder(oElement, 'CenterVerticalBorder', oPiece.CenterVerticalBorder);
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceRowStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentTableRowStartPiece;
Begin
  oPiece := TWPWorkingDocumentTableRowStartPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.Header := AttributeBool(oElement, 'Header');
    oPiece.Background := AttributeColour(oElement, 'Background');
    oPiece.LowerPaddingSize := AttributeInt(oElement, 'LowerPaddingSize');
    oPiece.LowerPaddingColour := AttributeColour(oElement, 'LowerPaddingColour');
    oPiece.Depth := AttributeInt(oElement, 'Depth');
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceCellStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentTableCellStartPiece;
Begin
  oPiece := TWPWorkingDocumentTableCellStartPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.Span := AttributeInt(oElement, 'Span');
    oPiece.Width := AttributeReal(oElement, 'Width');
    oPiece.Background := AttributeColour(oElement, 'Background');
    oPiece.MarginLeft := AttributeInt(oElement, 'MarginLeft');
    oPiece.MarginTop := AttributeInt(oElement, 'MarginTop');
    oPiece.MarginRight := AttributeInt(oElement, 'MarginRight');
    oPiece.MarginBottom := AttributeInt(oElement, 'MarginBottom');
    oPiece.VerticalAlignment := AttributeWordProcessorVerticalAlignment(oElement, 'VerticalAlignment');
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;

Procedure TWPSnapshotReader.ReadPieceStop(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentStopPiece;
Begin
  oPiece := TWPWorkingDocumentStopPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    oPiece.StopType := AttributeWPWorkingDocumentStopType(oElement, 'StopType');
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPSnapshotReader.ReadPieceSectionStart(oElement : TMXmlElement; oDocument : TWPWorkingDocument);
Var
  oPiece : TWPWorkingDocumentSectionStartPiece;
Begin
  oPiece := TWPWorkingDocumentSectionStartPiece.Create;
  Try
    ReadPieceDetails(oElement, oPiece);
    ReadContainerDetails(oElement, oPiece);
    oPiece.DisplayName := Attribute(oElement, 'DisplayName');
    oPiece.Namespace := Attribute(oElement, 'Namespace');
    oPiece.Name := Attribute(oElement, 'Name');
    oPiece.DisplayType := AttributeWPWorkingDocumentSectionDisplayTyp(oElement, 'DisplayType');
    oPiece.Deletable := AttributeBool(oElement, 'Deletable');
    oPiece.IsField := AttributeBool(oElement, 'IsField');
    oPiece.Key := Attribute(oElement, 'Key');
    // TODO: raw data
    oDocument.Pieces.Add(oPiece.Link);
  Finally
    oPiece.Free;
  End;
End;


Procedure TWPSnapshotReader.ReadContainerDetails(oElement : TMXmlElement; oPiece : TWPWorkingDocumentContainerPiece);
Begin
  oPiece.ReadOnly := AttributeTriState(oElement, 'ReadOnly');
End;

Procedure TWPSnapshotReader.ReadPieceDetails(oElement : TMXmlElement; oPiece : TWPWorkingDocumentPiece);
var
  oFormat : TMXmlElement;
  oSpot : TMXmlElement;
Begin
  oPiece.Style := Attribute(oElement, 'Style');
  oPiece.IsReadOnly := AttributeBool(oElement, 'IsReadOnly');

  oFormat := oElement.element('format');
  Expectedname(oFormat, 'format');
  oPiece.Font.Name := Attribute(oFormat, 'Name');
  oPiece.Font.Size := AttributeInt(oFormat, 'Size');
  oPiece.Font.Foreground := AttributeColour(oFormat, 'Foreground');
  oPiece.Font.Background := AttributeColour(oFormat, 'Background');
  oPiece.Font.Bold := AttributeTriState(oFormat, 'Bold');
  oPiece.Font.State := AttributeWPSFontState(oFormat, 'State');
  oPiece.Font.Italic := AttributeTriState(oFormat, 'Italic');
  oPiece.Font.Underline := AttributeTriState(oFormat, 'Underline');
  oPiece.Font.Strikethrough := AttributeTriState(oFormat, 'Strikethrough');
  oPiece.Font.Capitalization := AttributeWPSCapsState(oFormat, 'Capitalization');

  oSpot := oElement.element('hotspot');
  if (oSpot <> Nil) Then
  Begin
    oPiece.HasHotspot := True;
    oPiece.Hotspot.URL := Attribute(oElement, 'URL');
    oPiece.Hotspot.Key := Attribute(oElement, 'Key');
    oPiece.Hotspot.LinkColour := AttributeColour(oElement, 'LinkColour');
    oPiece.Hotspot.HoverColour := AttributeColour(oElement, 'HoverColour');
    oPiece.Hotspot.Title := Attribute(oElement, 'Title');
    oPiece.Hotspot.LinkUnderline := AttributeBool(oElement, 'LinkUnderline');
  End;
End;


Procedure TWPSnapshotReader.ReadParaFormat(oElement : TMXmlElement; oPiece : TWPWorkingDocumentParaPiece);
Begin
  ExpectedName(oElement, 'para-format');

  oPiece.Format.Align := AttributeWordProcessorParagraphAlignment(oElement, 'Align');
  oPiece.Format.BulletType := AttributeWPSParagraphBulletType(oElement, 'BulletType');
  oPiece.Format.NumberType := AttributeWPSParagraphNumberType(oElement, 'NumberType');
  oPiece.Format.ListType := AttributeWPSParagraphListType(oElement, 'ListType');
  oPiece.Format.FixedNumber := AttributeInt(oElement, 'FixedNumber');
  oPiece.Format.NumberFormat := AttributeWPSParagraphNumberFormat(oElement, 'NumberFormat');
  oPiece.Format.LeftIndent := AttributeInt(oElement, 'LeftIndent');
  oPiece.Format.RightIndent := AttributeInt(oElement, 'RightIndent');
  oPiece.Format.MarginBottom := AttributeInt(oElement, 'MarginBottom');
End;


Procedure TWPSnapshotReader.ReadBorder(oElement : TMXmlElement; Const sName : String; oBorder : TWPBorder);
Var
  oChild : TMXmlElement;
Begin
  oChild := oElement.element(sName);
  ExpectedName(oChild, sName);
  oBorder.Defined := AttributeBool(oChild, 'Defined');
  oBorder.Colour := AttributeColour(oChild, 'Colour');
  oBorder.Width := AttributeInt(oChild, 'Width');
  oBorder.Style := AttributeFslPenStyle(oChild, 'Style');
  oBorder.Fancy := AttributeBool(oChild, 'Fancy');
  oBorder.OuterColour := AttributeColour(oChild, 'OuterColour');
  oBorder.OuterColour2 := AttributeColour(oChild, 'OuterColour2');
  oBorder.LowOuterlimit := AttributeInt(oChild, 'LowOuterlimit');
  oBorder.HighOuterLimit := AttributeInt(oChild, 'HighOuterLimit');
End;

{ TWPSnapshotWriter }

Procedure TWPSnapshotWriter.ProducePiece(oPiece : TWPWorkingDocumentPiece);
Begin
  Case oPiece.PieceType Of
    ptText : ProducePieceText(TWPWorkingDocumentTextPiece(oPiece));
    ptImage : ProducePieceImage(TWPWorkingDocumentImagePiece(oPiece));
    ptFieldStart : ProducePieceFieldStart(TWPWorkingDocumentFieldStartPiece(oPiece));
    ptFieldStop : ProducePieceFieldStop(TWPWorkingDocumentFieldStopPiece(oPiece));
    ptLineBreak : ProducePieceLineBreak(TWPWorkingDocumentLineBreakPiece(oPiece));
    ptBreak : ProducePieceBreak(TWPWorkingDocumentBreakPiece(oPiece));
    ptPara : ProducePiecePara(TWPWorkingDocumentParaPiece(oPiece));
    ptTableStart : ProducePieceTableStart(TWPWorkingDocumentTableStartPiece(oPiece));
    ptRowStart : ProducePieceRowStart(TWPWorkingDocumentTableRowStartPiece(oPiece));
    ptCellStart : ProducePieceCellStart(TWPWorkingDocumentTableCellStartPiece(oPiece));
    ptTableStop,
    ptRowStop,
    ptSectionStop,
    ptCellStop : ProducePieceStop(TWPWorkingDocumentStopPiece(oPiece));
    ptSectionStart : ProducePieceSectionStart(TWPWorkingDocumentSectionStartPiece(oPiece));
  Else
    RaiseError('ProducePiece', 'Unknown Piece Type');
  End;
End;

Procedure TWPSnapshotWriter.ProducePieceText(oPiece : TWPWorkingDocumentTextPiece);
Begin
  Attributes.Match['Content'] := oPiece.Content;
  Attributes.Match['SpellState'] := WPSPELLCHECKINGSTATE_NAMES[oPiece.SpellState];
  ProducePieceStart('text', oPiece);
  ProducePieceClose('text', oPiece);
End;


Procedure TWPSnapshotWriter.ProducePieceImage(oPiece : TWPWorkingDocumentImagePiece);
Begin
  Attributes.Match['Border'] := IntegerToString(oPiece.Border);
  Attributes.Match['Height'] := IntegerToString(oPiece.Height);
  Attributes.Match['Width'] := IntegerToString(oPiece.Width);
  Attributes.Match['BorderColour'] := ColourToString(oPiece.BorderColour);
  Attributes.Match['Name'] := oPiece.Name;
  Attributes.Match['TransparentColour'] := ColourToString(oPiece.TransparentColour);
  Attributes.Match['VerticalAlignment'] := NAMES_IMAGEVERTICALALIGNMENT[oPiece.VerticalAlignment];
  ProducePieceStart('image', oPiece);
{
    FImage : TFslVCLGraphic;
    FSelectionImage : TFslVCLGraphic;
    FWorkingImage : TFslVCLGraphic;
    FImageMap : TWPImageMap;
}

  ProducePieceClose('image', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceFieldStart(oPiece : TWPWorkingDocumentFieldStartPiece);
Begin
  Attributes.Match['FixedFormat'] := TWPDOCUMENTOBJECT_FIXED_FORMAT[oPiece.FixedFormat];
  Attributes.Match['Deletable'] := BooleanToString(oPiece.Deletable);
  Attributes.Match['Namespace'] := oPiece.Namespace;
  Attributes.Match['Name'] := oPiece.Name;
  Attributes.Match['ReadOnly'] := NAMES_WPSTRISTATE[oPiece.ReadOnly];
  Attributes.Match['DefinitionProvider'] := BooleanToString(oPiece.DefinitionProvider <> Nil);
  Attributes.Match['InError'] := BooleanToString(oPiece.InError);
  Attributes.Match['CheckedIndex'] := IntegerToString(oPiece.CheckedIndex);
  Attributes.Match['Checkables'] := oPiece.Checkables.AsCSV;
  ProducePieceStart('field-start', oPiece);
  ProduceData(oPiece.RawData);
  //  oPiece.DocField
  ProducePieceClose('field-start', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceFieldStop(oPiece : TWPWorkingDocumentFieldStopPiece);
Begin
  ProducePieceStart('field-stop', oPiece);
  ProducePieceClose('field-stop', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceLineBreak(oPiece : TWPWorkingDocumentLineBreakPiece);
Begin
  ProducePieceStart('line-break', oPiece);
  ProducePieceClose('line-break', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceBreak(oPiece : TWPWorkingDocumentBreakPiece);
Begin
  Attributes.Match['BreakType'] := WPWorkingDocumentBreakPIECETYPE_NAMES[oPiece.BreakType];
  Attributes.Match['Alignment'] := NAMES_WPSALIGNMENT[oPiece.Alignment];
  Attributes.Match['Width'] := RealToString(oPiece.Width);
  Attributes.Match['PenColour'] := ColourToString(oPiece.PenColour);
  Attributes.Match['PenWidth'] := IntegerToString(oPiece.PenWidth);
  Attributes.Match['PenStyle'] := ADVPENSTYLE_CODES[oPiece.PenStyle];
  Attributes.Match['EndStyle '] := ADVPENENDSTYLE_CODES[oPiece.EndStyle];
  ProduceContainerPieceStart('break', oPiece);
  ProducePieceClose('break', oPiece);
End;


Procedure TWPSnapshotWriter.ProducePiecePara(oPiece : TWPWorkingDocumentParaPiece);
Begin
  Attributes.Match['ListNumber'] := IntegerToString(oPiece.ListNumber);
  Attributes.Match['WorkingRightIndent'] := IntegerToString(oPiece.WorkingRightIndent);
  Attributes.Match['WorkingLeftIndent'] := IntegerToString(oPiece.WorkingLeftIndent);
  Attributes.Match['SpeechMagicDouble'] := BooleanToString(oPiece.SpeechMagicDouble);

  ProduceContainerPieceStart('para', oPiece);
  ProduceParaFormat(oPiece.Format);
  ProducePieceClose('para', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceTableStart(oPiece : TWPWorkingDocumentTableStartPiece);
Var
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['BorderPolicy'] := CODES_TWPWorkingDocumentTableBORDERPOLICY[oPiece.BorderPolicy];
  Attributes.Match['HorizontalMargin'] := IntegerToString(oPiece.HorizontalMargin);
  Attributes.Match['VerticalMargin'] := IntegerToString(oPiece.VerticalMargin);
  Attributes.Match['Background'] := ColourToString(oPiece.Background);
  Attributes.Match['ColumnCount'] := IntegerToString(oPiece.ColumnCount);
  Attributes.Match['Jagged'] := BooleanToString(oPiece.Jagged);
  Attributes.Match['ContentDirty'] := BooleanToString(oPiece.ContentDirty);
  Attributes.Match['StructureDirty'] := BooleanToString(oPiece.StructureDirty);
  ProduceTableItemPieceStart('table', oPiece);
  ProduceBorder('CenterHorizontalBorder', oPiece.CenterHorizontalBorder);
  ProduceBorder('CenterVerticalBorder', oPiece.CenterVerticalBorder);
  sText := '';
  For iLoop := 0 To oPiece.Rows.Count - 1 Do
    sText := sText + GetId(oPiece.Rows[iLoop])+',';
  ProduceText('rows', sText);
  ProducePieceClose('table', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceRowStart(oPiece : TWPWorkingDocumentTableRowStartPiece);
Var
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['Header'] := BooleanToString(oPiece.Header);
  Attributes.Match['BreakBefore'] := BooleanToString(oPiece.BreakBefore);
  Attributes.Match['Background'] := ColourToString(oPiece.Background);
  Attributes.Match['LowerPaddingSize'] := IntegerToString(oPiece.LowerPaddingSize);
  Attributes.Match['LowerPaddingColour'] := ColourToString(oPiece.LowerPaddingColour);
  Attributes.Match['State'] := NAMES_WPWorkingDocumentTableItemState[oPiece.State];
  Attributes.Match['Depth'] := IntegerToString(oPiece.Depth);

  Attributes.Match['Owner'] := GetId(oPiece.Owner);
  if oPiece.HasTable Then
    Attributes.Match['Table'] := GetId(oPiece.Table);
  Attributes.Match['TablePrev'] := GetId(oPiece.TablePrev);
  Attributes.Match['TablePrevLevel'] := GetId(oPiece.TablePrevLevel);
  Attributes.Match['TableNext'] := GetId(oPiece.TableNext);
  Attributes.Match['TableNextLevel'] := GetId(oPiece.TableNextLevel);

  ProduceContainerPieceStart('table-row', oPiece);
  sText := '';
  For iLoop := 0 To oPiece.Cells.Count - 1 Do
    sText := sText + GetId(oPiece.Cells[iLoop])+',';
  ProduceText('cells', sText);
  ProducePieceClose('table-row', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceCellStart(oPiece : TWPWorkingDocumentTableCellStartPiece);
Begin
  Attributes.Match['Span'] := IntegerToString(oPiece.Span);
  Attributes.Match['Width'] := RealToString(oPiece.Width);
  Attributes.Match['Background'] := ColourToString(oPiece.Background);
  Attributes.Match['MarginLeft'] :=  IntegerToString(oPiece.MarginLeft);
  Attributes.Match['MarginTop'] := IntegerToString(oPiece.MarginTop);
  Attributes.Match['MarginRight'] := IntegerToString(oPiece.MarginRight);
  Attributes.Match['MarginBottom'] := IntegerToString(oPiece.MarginBottom);
  Attributes.Match['VerticalAlignment'] := NAMES_WORDPROCESSORVERTICALALIGNMENT[oPiece.VerticalAlignment];
  Attributes.Match['State'] := NAMES_WPWorkingDocumentTableItemState[oPiece.State];
  Attributes.Match['WidthMinimum'] := IntegerToString(oPiece.WidthMinimum);
  Attributes.Match['WidthMaximum'] := IntegerToString(oPiece.WidthMaximum);
  Attributes.Match['WidthCurrent'] := IntegerToString(oPiece.WidthCurrent);
  Attributes.Match['MaxLeftParaSpace'] := IntegerToString(oPiece.MaxLeftParaSpace);
  Attributes.Match['MaxRightParaSpace'] := IntegerToString(oPiece.MaxRightParaSpace);
  if oPiece.HasRow Then
    Attributes.Match['Row'] := GetId(oPiece.Row);
  ProduceTableItemPieceStart('table-cell', oPiece);
  ProducePieceClose('table-cell', oPiece);
End;

Procedure TWPSnapshotWriter.ProducePieceStop(oPiece : TWPWorkingDocumentStopPiece);
Begin
  Attributes.Match['StopType'] := NAMES_WorkingDocumentStopType[oPiece.StopType];
  ProducePieceStart('stop', oPiece);
  ProducePieceClose('stop', oPiece);
End;


Procedure TWPSnapshotWriter.ProducePieceSectionStart(oPiece : TWPWorkingDocumentSectionStartPiece);
Begin
  Attributes.Match['DisplayName'] := oPiece.DisplayName;
  Attributes.Match['Namespace'] := oPiece.Namespace;
  Attributes.Match['Name'] := oPiece.Name;
  Attributes.Match['DisplayType'] := NAMES_WPWorkingDocumentSectionDISPLAYTYPE[oPiece.DisplayType];
  Attributes.Match['Deletable'] := BooleanToString(oPiece.Deletable);
  Attributes.Match['IsField'] := BooleanToString(oPiece.IsField);
  Attributes.Match['Key'] := oPiece.Key;
  Attributes.Match['DefinitionProvider'] := BooleanToString(oPiece.DefinitionProvider <> Nil);
  ProduceContainerPieceStart('section-start', oPiece);
  ProduceData(oPiece.RawData);
  ProducePieceClose('section-start', oPiece);
End;


Procedure TWPSnapshotWriter.ProduceDocument(oDocument: TWPWorkingDocument);
Var
  iLoop : Integer;
Begin
  Attributes.Match['FieldCount'] := IntegerToString(oDocument.FieldCount);
  Attributes.Match['MinLength'] := IntegerToString(oDocument.MinLength);
  Attributes.Match['CharCount'] := IntegerToString(oDocument.CharCount);
  Attributes.Match['VoiceCharCount'] := IntegerToString(oDocument.VoiceCharCount);
  Attributes.Match['RegenerationNeeded'] := BooleanToString(oDocument.RegenerationNeeded);

  ProduceOpen('document');

  For iLoop := 0 To oDocument.Pieces.Count - 1 Do
    ProducePiece(oDocument.Pieces[iLoop]);
  ProduceClose('document');
End;

Procedure TWPSnapshotWriter.Start(Const sCause : String);
Begin
  ProduceHeader;
  Attributes.Match['Time'] := fsl_utilities.DateTimeFormat(LocalDateTime, 'yyyymmddhhnnss');
  Attributes.Match['cause'] := sCause;
  ProduceOpen('snapshot');

  Attributes.Match['system-name'] := SystemName;
  Attributes.Match['system-platform'] := SystemPlatform;
  Attributes.Match['system-architecture'] := SystemArchitecture;
  Attributes.Match['system-resolution'] := SystemResolution;
  Attributes.Match['system-bootStatus'] := SystemBootStatus;
  Attributes.Match['system-information'] := SystemInformation;
  Attributes.Match['system-path'] := SystemPath;
  Attributes.Match['system-temp'] := SystemTemp;
  Attributes.Match['system-timezone'] := SystemTimezone;
  Attributes.Match['system-language'] := SystemLanguage;
  Attributes.Match['system-processors'] := IntegerToString(SystemProcessors);
  Attributes.Match['system-pageSize'] := IntegerToString(SystemPageSize);
  Attributes.Match['process-name'] := ProcessName;

  Attributes.Match['memory-length'] := IntegerToString(SystemMemory.Length);
  Attributes.Match['memory-totalLoad'] := IntegerToString(SystemMemory.TotalLoad);
  Attributes.Match['memory-totalPhysical'] := IntegerToString(SystemMemory.TotalPhysical);
  Attributes.Match['memory-availablePhysical'] := IntegerToString(SystemMemory.AvailablePhysical);
  Attributes.Match['memory-totalPageFile'] := IntegerToString(SystemMemory.TotalPageFile);
  Attributes.Match['memory-availablePageFile'] := IntegerToString(SystemMemory.AvailablePageFile);
  Attributes.Match['memory-totalVirtual'] := IntegerToString(SystemMemory.TotalVirtual);
  Attributes.Match['memory-availableVirtual'] := IntegerToString(SystemMemory.AvailableVirtual);
  ProduceTag('system');

  Attributes.Match['host-ip'] := IPToString(HostIP);
  Attributes.Match['host-name'] := HostName;
  Attributes.Match['logon-name'] := LogonName;
  Attributes.Match['local-computer-name'] := LocalComputerName;
  Attributes.Match['remote-computer-name'] := RemoteComputerName;
  Attributes.Match['is-remote-session'] := BooleanToString(IsRemoteSession);
  Attributes.Match['domain-name'] := DomainName;
  Attributes.Match['domain-logon-name'] := DomainLogonName;
  Attributes.Match['proxy-server'] := ProxyServer;
  ProduceTag('network');
End;

Procedure TWPSnapshotWriter.Stop;
Begin
  ProduceClose('snapshot');
End;


Procedure TWPSnapshotWriter.ProduceBorder(Const sName: String; oBorder: TWPBorder);
Begin
  Attributes.Match['Defined'] := BooleanToString(oBorder.Defined);
  Attributes.Match['Colour'] := ColourToString(oBorder.Colour);
  Attributes.Match['Width'] := IntegerToString(oBorder.Width);
  Attributes.Match['Style'] := ADVPENSTYLE_CODES[oBorder.Style];
  Attributes.Match['Fancy'] := BooleanToString(oBorder.Fancy);
  Attributes.Match['OuterColour'] := ColourToString(oBorder.OuterColour);
  Attributes.Match['OuterColour2'] := ColourToString(oBorder.OuterColour2);
  Attributes.Match['LowOuterlimit'] := IntegerToString(oBorder.LowOuterlimit);
  Attributes.Match['HighOuterLimit'] := IntegerToString(oBorder.HighOuterLimit);
  ProduceTag(sName);
  // Attributes.Match['BrushImage'] := TFslBitmapGraphiToString(oBorder.BrushImage);
End;

Procedure TWPSnapshotWriter.ProduceContainerPieceStart(Const sName: String; oPiece: TWPWorkingDocumentContainerPiece);
Begin
  Attributes.Match['ReadOnly'] := NAMES_WPSTRISTATE[oPiece.ReadOnly];
  ProducePieceStart(sName, oPiece);
  If oPiece.HasContainer Then
    ProduceContainer(oPiece.Container);
End;

Procedure TWPSnapshotWriter.ProduceData(oData: TFslStringMatch);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oData.Count - 1 Do
    ProduceText(oData.KeyByIndex[iLoop], oData.ValueByIndex[iLoop]);
End;


Procedure TWPSnapshotWriter.ProduceParaFormat(oFormat: TWPSParagraphDetails);
Begin
  Attributes.Match['Align'] := NAMES_WPSPARAGRAPHALIGNMENT[oFormat.Align];
  Attributes.Match['BulletType'] := NAMES_WPSPARAGRAPHBULLETTYPE[oFormat.BulletType];
  Attributes.Match['NumberType'] := NAMES_WPSPARAGRAPHNUMBERTYPE[oFormat.NumberType];
  Attributes.Match['ListType'] := NAMES_WPSPARAGRAPHLISTTYPE[oFormat.ListType];
  Attributes.Match['FixedNumber'] := IntegerToString(oFormat.FixedNumber);
  Attributes.Match['NumberFormat'] := NAMES_WPSPARAGRAPHNUMBERFORMAT[oFormat.NumberFormat];
  Attributes.Match['LeftIndent'] := IntegerToString(oFormat.LeftIndent);
  Attributes.Match['RightIndent'] := IntegerToString(oFormat.RightIndent);
  Attributes.Match['MarginBottom'] := IntegerToString(oFormat.MarginBottom);
  ProduceTag('para-format');
End;

Procedure TWPSnapshotWriter.ProducePieceClose(Const sName: String; oPiece: TWPWorkingDocumentPiece);
Begin
  ProduceClose(sName);
End;

Procedure TWPSnapshotWriter.ProducePieceStart(Const sName: String; oPiece: TWPWorkingDocumentPiece);
Var
  iLoop : Integer;
Begin
  Attributes.Match['Style'] := oPiece.Style;
  Attributes.Match['IsReadOnly'] := BooleanToString(oPiece.IsReadOnly);
  Attributes.Match['id'] := GetId(oPiece);
  Attributes.Match['Prev'] := GetId(oPiece.Prev);
  Attributes.Match['Next'] := GetId(oPiece.Next);


  ProduceOpen(sName);
  For iLoop := 0 To oPiece.Maps.Count - 1 Do
    ProduceMap(oPiece.Maps[iLoop]);
  ProduceMetrics(oPiece.Metrics);
  ProduceFormat(oPiece.Font);
  If oPiece.HasHotspot Then
    ProduceHotspot(oPiece.Hotspot);
End;

Procedure TWPSnapshotWriter.ProduceTableItemPieceStart(Const sName: String; oPiece: TWPWorkingDocumentTableItemPiece);
Begin
  ProduceContainerPieceStart(sName, oPiece);
  ProduceBorder('LeftBorder', oPiece.LeftBorder);
  ProduceBorder('RightBorder', oPiece.RightBorder);
  ProduceBorder('TopBorder', oPiece.TopBorder);
  ProduceBorder('BottomBorder', oPiece.BottomBorder);
  If oPiece.HasWorkingLeftBorder Then
    ProduceBorder('WorkingLeftBorder', oPiece.WorkingLeftBorder);
  If oPiece.HasWorkingRightBorder Then
    ProduceBorder('WorkingRightBorder', oPiece.WorkingRightBorder);
  If oPiece.HasWorkingTopBorder Then
    ProduceBorder('WorkingTopBorder', oPiece.WorkingTopBorder);
  If oPiece.HasWorkingBottomBorder Then
    ProduceBorder('WorkingBottomBorder', oPiece.WorkingBottomBorder);
End;


Procedure TWPSnapshotWriter.ProduceMap(oMap : TWPMapItem);
Begin
  Attributes.Match['OffsetLength'] := IntegerToString(oMap.OffsetLength);
  Attributes.Match['OffsetStart'] := IntegerToString(oMap.OffsetStart);
  Attributes.Match['Descent'] := IntegerToString(oMap.Descent);
  Attributes.Match['BreakState'] := NAMES_WPMAPITEMBREAKSTATE[oMap.BreakState];
  Attributes.Match['Selection'] := NAMES_WPMAPSELECTION[oMap.Selection];
  Attributes.Match['Nose'] := IntegerToString(oMap.Nose);
  Attributes.Match['ForeHotspot'] := GetId(oMap.ForeHotspot);
  ProduceMapObjectOpen('map', oMap);
  ProduceClose('map');
End;

Procedure TWPSnapshotWriter.ProduceMetrics(oMetrics : TWPMetrics);
Var
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['CharCount'] := IntegerToString(oMetrics.CharCount);
  Attributes.Match['VoiceCharCount'] := IntegerToString(oMetrics.VoiceCharCount);
  Attributes.Match['OffsetCount'] := IntegerToString(oMetrics.OffsetCount);
  Attributes.Match['Position'] := IntegerToString(oMetrics.Position);
  Attributes.Match['VoicePosition'] := IntegerToString(oMetrics.VoicePosition);
  sText := '';
  For iLoop := 0 To oMetrics.OffsetCount - 1 Do
    sText := sText + IntegerToString(oMetrics.Offset[iLoop])+' ';
  Attributes.Match['Offsets'] := sText;
  Attributes.Match['Height'] := IntegerToString(oMetrics.Height);
  Attributes.Match['Descent'] := IntegerToString(oMetrics.Descent);
  Attributes.Match['Valid'] := BooleanToString(oMetrics.Valid);
  ProduceTag('metrics');
End;

Procedure TWPSnapshotWriter.ProduceFormat(oFont : TWPSFontDetails);
Begin
  Attributes.Match['Name'] := oFont.Name;
  Attributes.Match['Size'] := IntegerToString(oFont.Size);
  Attributes.Match['Foreground'] := ColourToString(oFont.Foreground);
  Attributes.Match['Background'] := ColourToString(oFont.Background);
  Attributes.Match['Bold'] := NAMES_WPSTRISTATE[oFont.Bold];
  Attributes.Match['State'] := NAMES_WPSFONTSTATE[oFont.State];
  Attributes.Match['Italic'] := NAMES_WPSTRISTATE[oFont.Italic];
  Attributes.Match['Underline'] := NAMES_WPSTRISTATE[oFont.Underline];
  Attributes.Match['Strikethrough'] := NAMES_WPSTRISTATE[oFont.Strikethrough];
  Attributes.Match['Capitalization'] := NAMES_WPSCAPSSTATE[oFont.Capitalization];
  ProduceTag('format');
End;

Procedure TWPSnapshotWriter.ProduceHotspot(oHotspot : TWPHotspot);
Begin
  Attributes.Match['id'] := GetId(oHotspot);
  Attributes.Match['URL'] := oHotspot.URL;
  Attributes.Match['Key'] := oHotspot.Key;
  Attributes.Match['LinkColour'] := ColourToString(oHotspot.LinkColour);
  Attributes.Match['HoverColour'] := ColourToString(oHotspot.HoverColour);
  Attributes.Match['Title'] := oHotspot.Title;
  Attributes.Match['LinkUnderline'] := BooleanToString(oHotspot.LinkUnderline);
  ProduceTag('hotspot');
End;


Constructor TWPSnapshotWriter.Create;
Begin
  Inherited;
  FIds := TObjectList.Create;
  FIds.OwnsObjects := False;
End;

Destructor TWPSnapshotWriter.Destroy;
Begin
  FIds.Free;
  Inherited;
End;

Function TWPSnapshotWriter.GetId(oObject: TObject): String;
Var
  iIndex : Integer;
Begin
  iIndex := FIds.IndexOf(oObject);
  If iIndex = -1 Then
  Begin
    Result := IntegerToString(FIds.Count);
    FIds.Add(oObject);
  End
  Else
    Result := IntegerToString(iIndex);
End;

Procedure TWPSnapshotWriter.ProduceRow(oMap: TWPMapRow);
Var
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['Line'] := IntegerToString(oMap.Line);
  Attributes.Match['ColumnOffset'] := IntegerToString(oMap.ColumnOffset);
  Attributes.Match['BaseLine'] := IntegerToString(oMap.BaseLine);

  Attributes.Match['PaintedFirstLeft'] := IntegerToString(oMap.PaintedFirstLeft);
  Attributes.Match['PaintedLastRight'] := IntegerToString(oMap.PaintedLastRight);
  Attributes.Match['FirstLeft'] := IntegerToString(oMap.FirstLeft);
  Attributes.Match['LastRight'] := IntegerToString(oMap.LastRight);
  Attributes.Match['ColCount'] := IntegerToString(oMap.ColCount);
  Attributes.Match['LastOffset'] := IntegerToString(oMap.GetLastOffset);
  Attributes.Match['PrintLeftOffset'] := IntegerToString(oMap.PrintLeftOffset);
  Attributes.Match['PrintTopOffset'] := IntegerToString(oMap.PrintTopOffset);

  ProduceMapObjectOpen('row', oMap);
  ProduceText('text', oMap.GetText);
  sText := '';
  For iLoop := 0 To oMap.Items.Count - 1 Do
    sText := sText + GetId(oMap.Items[iLoop])+',';
  ProduceText('items', sText);
  ProduceClose('row');
End;

Procedure TWPSnapshotWriter.ProduceContainer(oMap: TWPMapContainer);
Var
  iLoop : Integer;
  sText : String;
Begin
  Attributes.Match['MarginLeft'] := IntegerToString(oMap.MarginLeft);
  Attributes.Match['MarginRight'] := IntegerToString(oMap.MarginRight);
  Attributes.Match['MarginTop'] := IntegerToString(oMap.MarginTop);
  Attributes.Match['MarginBottom'] := IntegerToString(oMap.MarginBottom);
  Attributes.Match['ChildrenHorizontal'] := BooleanToString(oMap.ChildrenHorizontal);
  Attributes.Match['PrintLeftOffset'] := IntegerToString(oMap.PrintLeftOffset);
  Attributes.Match['PrintTopOffset'] := IntegerToString(oMap.PrintTopOffset);
  Attributes.Match['Primary'] := BooleanToString(oMap.Primary);
  Attributes.Match['Cursor'] := IntegerToString(oMap.Cursor);

  ProduceMapObjectOpen('container', oMap);

  sText := '';
  For iLoop := 0 To oMap.Children.Count - 1 Do
    sText := sText + GetId(oMap.Children[iLoop])+',';
  ProduceText('children', sText);

  For iLoop := 0 To oMap.Rows.Count - 1 Do
    ProduceRow(oMap.Rows[iLoop]);


  ProduceClose('container');
End;

Procedure TWPSnapshotWriter.ProduceMapObjectOpen(Const sName : String; oMap : TWPMapObject);
Begin
  Attributes.Match['id'] := GetId(oMap);
  If oMap.HasParent Then
    Attributes.Match['Parent'] := GetId(oMap.Parent);
  Attributes.Match['Painted'] := BooleanToString(oMap.Painted);
  Attributes.Match['Width'] := IntegerToString(oMap.Width);
  Attributes.Match['Height'] := IntegerToString(oMap.Height);
  Attributes.Match['Top'] := IntegerToString(oMap.Top);
  Attributes.Match['Left'] := IntegerToString(oMap.Left);
  if oMap.HasPiece Then
    Attributes.Match['Piece'] := GetId(oMap.Piece)
  Else
    Attributes.Match['Piece'] := 'nil';
  Attributes.Match['Background'] := ColourToString(oMap.Background);
  Attributes.Match['ClipLeft'] := IntegerToString(oMap.ClipLeft);
  Attributes.Match['ClipTop'] := IntegerToString(oMap.ClipTop);
  Attributes.Match['ClipRight'] := IntegerToString(oMap.ClipRight);
  Attributes.Match['ClipBottom'] := IntegerToString(oMap.ClipBottom);
  ProduceOpen(sName);
  If oMap.BackHotspot <> Nil Then
    ProduceHotspot(oMap.BackHotspot);
End;




Procedure TWPSnapshotWriter.ProduceStyles(Const sName : String; oStyles : TWPStyles);
Var
  iLoop : Integer;
Begin
  ProduceOpen(sName);
  For iLoop := 0 To oStyles.Count - 1 Do
    ProduceStyle(oStyles[iLoop]);
  ProduceClose(sName);
End;



Procedure TWPSnapshotWriter.ProduceStyle(oStyle: TWPStyle);
Begin
  Attributes.Match['Name'] := oStyle.Name;
  ProduceOpen('Style');
  ProduceFormat(oStyle.Font);
  ProduceParaFormat(oStyle.Paragraph);
  ProduceClose('Style');
End;



function TWPSnapshotWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FFilename.length * sizeof(char)) + 12);
end;

End.


