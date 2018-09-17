Unit FHIR.WP.Format;

{
Copyright (c) 2001-2013, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
  Windows, SysUtils, Vcl.Graphics, Vcl.Imaging.pngimage,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream, FHIR.Support.Graphics,
  FHIR.Dicom.Dictionary,
  FHIR.WP.Types, FHIR.WP.Document, FHIR.WP.Imaging, FHIR.WP.Working; 


Type
  TWPFormat = (wpfUnknown, wpfNative, wpfText, wpfHTML, wpfRTF, wpfMHT, wpfODT, wpfSnapshot, wpfCDA);

Const
  WPFORMAT_NAMES : Array [TWPFormat] Of String = ('??', 'Native', 'Text', 'HTML', 'RTF', 'MHT', 'ODT', 'Snapshot', 'CDA');
  WPFORMAT_DISPLAYS : Array [TWPFormat] Of String = ('??', 'Internal (XML)', 'Plain Text (ANSI)', 'HTML (xhtml 1.0)', 'Microsoft Rich Text Format', 'Microsoft HTML Package (Multi-part Mime)', 'OpenDocument Standard', 'Snapshot (debug dump format)', 'CDA (Clinical Document Architecture)');
  WPFORMAT_UNIVERSAL = [Low(TWPFormat)..High(TWPFormat)];

Const
  ifUnknown = FHIR.WP.Imaging.ifUnknown;
  ifJPEG = FHIR.WP.Imaging.ifJPEG;
  ifPNG = FHIR.WP.Imaging.ifPNG;
  ifBMP = FHIR.WP.Imaging.ifBMP;

Type
  TWPReader = Class;
  TWPWriter = Class;

  TWPFormatConvertorOption = (foNoHtmlBody,
                              foTemplateMode); // for console editor

  TWPFormatConvertorOptions = Set Of TWPFormatConvertorOption;

  TWPFormatConvertor = Class (TFslObject)
    Private
      FOptions : TWPFormatConvertorOptions;

      FSourceDocument : TWPDocument;
      FSourceStream : TFslStream;
      FSourceFormat : TWPFormat;

      FDestinationDocument : TWPDocument;
      FDestinationStream : TFslStream;
      FDestinationFormat : TWPFormat;

      Function GetSourceStream: TFslStream;
      Procedure SetSourceStream(Const Value : TFslStream);
      Function GetSourceDocument: TWPDocument;
      Procedure SetSourceDocument(Const Value : TWPDocument);

      Function GetDestinationStream: TFslStream;
      Procedure SetDestinationStream(Const Value : TFslStream);
      Function GetDestinationDocument: TWPDocument;
      Procedure SetDestinationDocument(Const Value : TWPDocument);

      Procedure Read(oDocument : TWPWorkingDocument; oStyles : TWPStyles);
      Procedure Write(oDocument : TWPWorkingDocument; oStyles : TWPStyles);
    Protected
      Function MakeReader : TWPReader;
      Function MakeWriter : TWPWriter;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Class Procedure Convert(oSourceStream : TFslStream; aSourceFormat : TWPFormat; oDestinationStream : TFslStream; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []);  Overload;
      Class Procedure Convert(oSourceDocument : TWPDocument; oDestinationStream : TFslStream; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []);  Overload;
      Class Procedure Convert(oSourceStream : TFslStream; aSourceFormat : TWPFormat; oDestinationDocument : TWPDocument; aOptions : TWPFormatConvertorOptions = []);  Overload;

{$IFDEF VER130}
      Class Function Convert(Const sSource : String; aSourceFormat, aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []) : String; Overload;
      Class Procedure Convert(Const sSource : String; aSourceFormat : TWPFormat; oDestination : TWPDocument; aOptions : TWPFormatConvertorOptions = []); Overload;
      Class Function Convert(oSource : TWPDocument; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []): String; Overload;
{$ELSE}
      Class Function Convert(Const aSource : TBytes; aSourceFormat, aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []) : TBytes; Overload;
      Class Procedure Convert(Const aSource : TBytes; aSourceFormat : TWPFormat; oDestination : TWPDocument; aOptions : TWPFormatConvertorOptions = []); Overload;
      Class Function Convert(oSource : TWPDocument; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []): TBytes; Overload;
{$ENDIF}


      Procedure Convert; Overload; Virtual;

      Function HasSourceDocument : Boolean; Overload; Virtual;
      Function HasSourceStream : Boolean; Overload; Virtual;
      Function HasDestinationDocument : Boolean; Overload; Virtual;
      Function HasDestinationStream : Boolean; Overload; Virtual;

      Property Options : TWPFormatConvertorOptions Read FOptions Write FOptions;
      Property SourceDocument : TWPDocument Read GetSourceDocument Write SetSourceDocument;
      Property SourceStream : TFslStream Read GetSourceStream Write SetSourceStream;
      Property SourceFormat : TWPFormat Read FSourceFormat Write FSourceFormat;

      Property DestinationDocument : TWPDocument Read GetDestinationDocument Write SetDestinationDocument;
      Property DestinationStream : TFslStream Read GetDestinationStream Write SetDestinationStream;
      Property DestinationFormat : TWPFormat Read FDestinationFormat Write FDestinationFormat;
  End;




  TWPLoadImageEvent = Procedure (oSender : TObject; Const Context : String; Const sName : String; Out oBuffer : TFslBuffer) Of Object;

  TWPReader = Class (TFslObject)
  Private
    FOnLoadImage : TWPLoadImageEvent;
    FStream : TFslStream;
    FStyles : TWPStyles;
    FExtendExisting : Boolean;
    FFont : TWPSFontDetails;
    FStyle : String;
    FIsFragment : Boolean;
    FSuppressSections : Boolean;
    FSplitter : TWPWordIterator;
    FSpeechMagicDouble : Boolean;
    FMustCloseWithPara : Boolean;
    FContext : String;
    FDicomDictionary: TDicomDictionary;

    Procedure SetStyles(Const Value: TWPStyles);

    Function GetStyles: TWPStyles;
    Function GetStyle: String;

    Function GetFont: TWPSFontDetails;
    Procedure SetFont(Const Value: TWPSFontDetails);

    Function GetStream: TFslStream;
    Procedure SetStream(Const Value: TFslStream);
    procedure SetDicomDictionary(const Value: TDicomDictionary);

  Protected
    Procedure LoadImage(oImage : TWPWorkingDocumentImagePiece; oBuffer : TFslBuffer; aFormat : TWPImageFormat; bSelection : Boolean); Overload; Virtual;
    Procedure CreateUnloadedImage(oImage : TWPWorkingDocumentImagePiece); Overload; Virtual;

    Procedure CheckForEmpty(oDocument : TWPWorkingDocument);
    Procedure DoneReading(oDocument : TWPWorkingDocument);

  Public
    constructor Create; Override;
    destructor Destroy; Override;

    Procedure Read(oDocument : TWPWorkingDocument); Overload; Virtual;
    Procedure Read(oDocument : TWPDocument); Overload;

    Function HasStyles : Boolean;
    Function HasFont : Boolean;
    Function HasStyle : Boolean;

    // font/style to apply to content with no styles
    Property Stream : TFslStream Read GetStream Write SetStream;
    Property Styles : TWPStyles Read GetStyles Write SetStyles;
    Property Font : TWPSFontDetails Read GetFont Write SetFont;
    Property Style : String Read GetStyle Write FStyle;
    Property ExtendExisting : Boolean Read FExtendExisting Write FExtendExisting;
    Property OnLoadImage : TWPLoadImageEvent Read FOnLoadImage Write FOnLoadImage;
    Property IsFragment : Boolean Read FIsFragment Write FIsFragment;
    Property SuppressSections : Boolean Read FSuppressSections Write FSuppressSections;
    Property Splitter : TWPWordIterator Read FSplitter;
    Property SpeechMagicDouble : Boolean Read FSpeechMagicDouble Write FSpeechMagicDouble;
    Property MustCloseWithPara : Boolean read FMustCloseWithPara write FMustCloseWithPara;
    Property Context : String read FContext write FContext;
    Property DicomDictionary : TDicomDictionary read FDicomDictionary write SetDicomDictionary;
  End;

  TWPReaderClass = Class Of TWPReader;


  // used by formatters when the image cannot be saved inline
  // sName will be populated if the image already existed when read with the name that it had
  TWPSaveImageEvent = Procedure (oSender : TObject; context : TFslObject; oBuffer : TFslBuffer; Const sExtension : String; Var sName : String) Of Object;

  TWPWriter = Class (TFslObject)
    Private
      FStyles : TWPStyles;
      FStream : TFslStream;
      FEmbedStyles : Boolean;

      FOnSaveImage : TWPSaveImageEvent;
      FImageContext : TFslObject;

      Function GetStyles : TWPStyles;
      Procedure SetStyles(Const Value: TWPStyles);
      Function GetStream : TFslStream;
      Procedure SetStream(Const Value: TFslStream);
    procedure SetImageContext(const Value: TFslObject);

    Protected

      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Overload; Virtual;
      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Overload; Virtual;
      Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Overload; Virtual;
      Procedure WriteFieldStart(oField : TWPWorkingDocumentFieldStartPiece); Overload; Virtual;
      Procedure WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece); Overload; Virtual;
      Procedure WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece); Overload; Virtual;
      Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Virtual;
      Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Overload; Virtual;
      Procedure WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece); Overload; Virtual;
      Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Virtual; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece); Overload; Virtual;
      Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : boolean); Overload; Virtual; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece); Overload; Virtual;
      Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Virtual; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteSectionStart(oSection : TWPWorkingDocumentSectionStartPiece); Overload; Virtual;
      Procedure WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Virtual; // note that start is passed, not stop, since stop carries nothing
      Procedure WriteDocumentStart(oDocument : TWPWorkingDocument); Overload; Virtual;
      Procedure WriteDocumentStop(oDocument : TWPWorkingDocument); Overload; Virtual;

      Procedure IterateDocument(oIterator : TWPPieceIterator; oDocument : TWPWorkingDocument); Overload; Virtual;
      Procedure IterateTable(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateTableRow(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateTableCell(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateSectionInner(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Virtual;
      Procedure IterateSection(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateBreak(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Virtual;
      Procedure IterateParagraphContents(oIterator : TWPPieceIterator); Overload; Virtual;
      Procedure IterateField(oIterator : TWPPieceIterator); Overload; Virtual;


      Function SupportsNestedRows : Boolean; Overload; Virtual;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Write(oDocument : TWPDocument); Overload; Virtual;
      Procedure Write(oDocument : TWPWorkingDocument); Overload; Virtual;

      Class Procedure SavePNGToStream(oImage : TFslVCLGraphic; oStream : TFslStream); Overload; Virtual;

      Property Stream : TFslStream Read GetStream Write SetStream;
      Property Styles : TWPStyles Read GetStyles Write SetStyles;
      Property EmbedStyles : Boolean Read FEmbedStyles Write FEmbedStyles;
      Property OnSaveImage : TWPSaveImageEvent Read FOnSaveImage Write FOnSaveImage;
      property ImageContext : TFslObject read FImageContext write SetImageContext;

  End;

  TWPWriterClass = Class Of TWPWriter;

Function ReaderForFormat(aFormat : TWPFormat) : TWPReaderClass;
Function ReaderForContentType(aContentType : TWPClipboardContentType) : TWPReaderClass;
Function WriterForFormat(aFormat : TWPFormat) : TWPWriterClass;

Function ReadFixedFormatAttribute(sValue : String) : TWPDocumentFieldFixedFormat;

Implementation

uses
  FHIR.WP.Text, FHIR.WP.Rtf, FHIR.WP.Odt, FHIR.WP.Html, FHIR.WP.Native;


{$IFDEF VER130}
Class Procedure TWPFormatConvertor.Convert(Const sSource : String; aSourceFormat : TWPFormat; oDestination : TWPDocument; aOptions : TWPFormatConvertorOptions = []);
{$ELSE}
Class Procedure TWPFormatConvertor.Convert(Const aSource : TBytes; aSourceFormat : TWPFormat; oDestination : TWPDocument; aOptions : TWPFormatConvertorOptions = []);
{$ENDIF}
Var
  oSourceStream : TFslStringStream;
Begin
  oSourceStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oSourceStream.Data := sSource;
{$ELSE}
    oSourceStream.Bytes := aSource;
{$ENDIF}

    Convert(oSourceStream, aSourceFormat, oDestination, aOptions);
  Finally
    oSourceStream.Free;
  End;
End;


{$IFDEF VER130}
Class Function TWPFormatConvertor.Convert(oSource : TWPDocument; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []): String;
{$ELSE}
Class Function TWPFormatConvertor.Convert(oSource : TWPDocument; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []): TBytes;
{$ENDIF}
Var
  oDestinationStream : TFslStringStream;
Begin
  oDestinationStream := TFslStringStream.Create;
  Try
    Convert(oSource, oDestinationStream, aDestinationFormat, aOptions);
{$IFDEF VER130}
    Result := oDestinationStream.Data;
{$ELSE}
    Result := oDestinationStream.Bytes;
{$ENDIF}
  Finally
    oDestinationStream.Free;
  End;
End;


{$IFDEF VER130}
Class Function TWPFormatConvertor.Convert(Const sSource : String; aSourceFormat, aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []) : String;
{$ELSE}
Class Function TWPFormatConvertor.Convert(Const aSource : TBytes; aSourceFormat, aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []) : TBytes;
{$ENDIF}
Var
  oSourceStream, oDestinationStream : TFslStringStream;
Begin
  oSourceStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oSourceStream.Data := sSource;
{$ELSE}
    oSourceStream.Bytes := aSource;
{$ENDIF}

    oDestinationStream := TFslStringStream.Create;
    Try
      Convert(oSourceStream, aSourceFormat, oDestinationStream, aDestinationFormat, aOptions);

{$IFDEF VER130}
      Result := oDestinationStream.Data;
{$ELSE}
      Result := oDestinationStream.Bytes;
{$ENDIF}
    Finally
      oDestinationStream.Free;
    End;
  Finally
    oSourceStream.Free;
  End;
End;


Class Procedure TWPFormatConvertor.Convert(oSourceStream : TFslStream; aSourceFormat : TWPFormat; oDestinationStream : TFslStream; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []);
Var
  oConvertor : TWPFormatConvertor;
Begin
  oConvertor := TWPFormatConvertor.Create;
  Try
    oConvertor.Options := aOptions;
    oConvertor.SourceStream := oSourceStream.Link;
    oConvertor.SourceFormat := aSourceFormat;
    oConvertor.DestinationStream := oDestinationStream.Link;
    oConvertor.DestinationFormat := aDestinationFormat;

    oConvertor.Convert;
  Finally
    oConvertor.Free;
  End;
End;


Class Procedure TWPFormatConvertor.Convert(oSourceDocument : TWPDocument; oDestinationStream : TFslStream; aDestinationFormat : TWPFormat; aOptions : TWPFormatConvertorOptions = []);
Var
  oConvertor : TWPFormatConvertor;
Begin
  oConvertor := TWPFormatConvertor.Create;
  Try
    oConvertor.Options := aOptions;
    oConvertor.SourceDocument := oSourceDocument.Link;
    oConvertor.DestinationStream := oDestinationStream.Link;
    oConvertor.DestinationFormat := aDestinationFormat;

    oConvertor.Convert;
  Finally
    oConvertor.Free;
  End;
End;


Class Procedure TWPFormatConvertor.Convert(oSourceStream : TFslStream; aSourceFormat : TWPFormat; oDestinationDocument : TWPDocument; aOptions : TWPFormatConvertorOptions = []);
Var
  oConvertor : TWPFormatConvertor;
Begin
  oConvertor := TWPFormatConvertor.Create;
  Try
    oConvertor.Options := aOptions;
    oConvertor.SourceStream := oSourceStream.Link;
    oConvertor.SourceFormat := aSourceFormat;
    oConvertor.DestinationDocument := oDestinationDocument.Link;

    oConvertor.Convert;
  Finally
    oConvertor.Free;
  End;
End;


Constructor TWPFormatConvertor.Create;
Begin
  Inherited;

  FSourceDocument := Nil;
  FSourceStream := Nil;
  FDestinationDocument := Nil;
  FDestinationStream := Nil;
End;


Destructor TWPFormatConvertor.Destroy;
Begin
  FSourceDocument.Free;
  FSourceStream.Free;
  FDestinationDocument.Free;
  FDestinationStream.Free;

  Inherited;
End;


Function TWPFormatConvertor.GetSourceDocument: TWPDocument;
Begin
  Assert(Invariants('GetSourceDocument', FSourceDocument, TWPDocument, 'FSourceDocument'));

  Result := FSourceDocument;
End;


Procedure TWPFormatConvertor.SetSourceDocument(Const Value : TWPDocument);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetSourceDocument', Value, TWPDocument, 'Value'));

  FSourceDocument.Free;
  FSourceDocument := Value;
End;


Function TWPFormatConvertor.GetSourceStream: TFslStream;
Begin
  Assert(Invariants('GetSourceStream', FSourceStream, TFslStream, 'FSourceStream'));

  Result := FSourceStream;
End;


Procedure TWPFormatConvertor.SetSourceStream(Const Value : TFslStream);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetSourceStream', Value, TFslStream, 'Value'));

  FSourceStream.Free;
  FSourceStream := Value;
End;


Function TWPFormatConvertor.GetDestinationDocument: TWPDocument;
Begin
  Assert(Invariants('GetDestinationDocument', FDestinationDocument, TWPDocument, 'FDestinationDocument'));

  Result := FDestinationDocument;
End;


Procedure TWPFormatConvertor.SetDestinationDocument(Const Value : TWPDocument);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetDestinationDocument', Value, TWPDocument, 'Value'));

  FDestinationDocument.Free;
  FDestinationDocument := Value;
End;


Function TWPFormatConvertor.GetDestinationStream: TFslStream;
Begin
  Assert(Invariants('GetDestinationStream', FDestinationStream, TFslStream, 'FDestinationStream'));

  Result := FDestinationStream;
End;


Procedure TWPFormatConvertor.SetDestinationStream(Const Value : TFslStream);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetDestinationStream', Value, TFslStream, 'Value'));

  FDestinationStream.Free;
  FDestinationStream := Value;
End;


Procedure TWPFormatConvertor.Read(oDocument : TWPWorkingDocument; oStyles : TWPStyles);
Var
  oReader : TWPReader;
  oTrans : TWPDocumentTranslator;
Begin
  If HasSourceDocument Then
  Begin
    oTrans := TWPDocumentTranslator.Create;
    Try
      oTrans.Document := SourceDocument.Link;
      oTrans.WorkingDocument := oDocument.Link;
      oTrans.WorkingStyles := oStyles.Link;
      oTrans.TranslateToWorking;
    Finally
      oTrans.Free;
    End;
  End
  Else
  Begin
    oReader := MakeReader;
    Try
      oReader.Stream := SourceStream.Link;
      oReader.Styles := oStyles.Link;

      oReader.Read(oDocument);
    Finally
      oReader.Free;
    End;
  End;
End;


Procedure TWPFormatConvertor.Write(oDocument : TWPWorkingDocument; oStyles : TWPStyles);
Var
  oWriter : TWPWriter;
  oTrans : TWPDocumentTranslator;
Begin
  If HasDestinationDocument Then
  Begin
    oTrans := TWPDocumentTranslator.Create;
    Try
      oTrans.Document := DestinationDocument.Link;
      oTrans.WorkingDocument := oDocument.Link;
      oTrans.WorkingStyles := oStyles.Link;
      oTrans.TranslateToDocument;
    Finally
      oTrans.Free;
    End;
  End
  Else
  Begin
    oWriter := MakeWriter;
    Try
      oWriter.Stream := DestinationStream.Link;
      oWriter.Styles := oStyles.Link;

      oWriter.Write(oDocument);
    Finally
      oWriter.Free;
    End;
  End;
End;

Procedure TWPFormatConvertor.Convert;
Var
  oDocument : TWPWorkingDocument;
  oStyles : TWPStyles;
Begin
  oDocument := TWPWorkingDocument.Create;
  Try
    oStyles := TWPStyles.Create;
    Try
      Read(oDocument, oStyles);

      oDocument.RegenerateMetrics(False, False);

      Write(oDocument, oStyles);
    Finally
      oStyles.Free;
    End;
  Finally
    oDocument.Free;
  End;
End;


Function ReaderForContentType(aContentType : TWPClipboardContentType) : TWPReaderClass;
Begin
  Case aContentType Of
    wcctText : Result := TWPTextReader;
    wcctFormattedText : Result := TWPTextReader;
    wcctRTF : Result := TWPRTFReader;
    wcctOdt : Result := TWPOdtReader;
    wcctHTML : Result := TWPHTMLReader;
    wcctNative : Result := TWPNativeReader;
  Else
    // wcctUnknown, wcctJPEG, wcctBitmap
    Raise EFslException.Create('Unable to Read '+WPCLIPBOARDCONTENTTYPE_NAMES[aContentType]);
  End;
End;


Function ReaderForFormat(aFormat : TWPFormat) : TWPReaderClass;
Begin
  Case aFormat Of
    wpfNative  : Result := TWPNativeReader;
    wpfText    : Result := TWPTextReader;
    wpfHTML    : Result := TWPHTMLReader;
    wpfRTF     : Result := TWPRTFReader;
    wpfOdt     : Result := TWPOdtReader;
  Else
    Result := Nil;
  End;
End;


Function WriterForFormat(aFormat : TWPFormat) : TWPWriterClass;
Begin
  Case aFormat Of
    wpfNative  : Result := TWPNativeWriter;
    wpfText    : Result := TWPTextWriter;
    wpfHTML    : Result := TWPHTMLWriter;
    wpfRTF     : Result := TWPRTFWriter;
    wpfOdt     : Result := TWPOdtWriter;
    wpfMHT     : Result := TWPMHTWriter;
  Else
    Result := Nil;
  End;
End;


Function TWPFormatConvertor.MakeReader : TWPReader;
Var
  aClass : TWPReaderClass;
Begin
  aClass := ReaderForFormat(FSourceFormat);
  If aClass = Nil Then
  Begin
    Result := Nil;
    RaiseError('MakeReader', 'Unknown Format');
  End
  Else
    Result := aClass.Create;
End;


Function TWPFormatConvertor.MakeWriter : TWPWriter;
Var
  aClass : TWPWriterClass;
Begin
  aClass := WriterForFormat(FDestinationFormat);
  If aClass = Nil Then
  Begin
    Result := Nil;
    RaiseError('MakeWriter', 'Unknown Format');
  End
  Else
    Result := aClass.Create;
  If (Result Is TWPHTMLWriter) And (foNoHtmlBody In Options) Then
    TWPHTMLWriter(Result).NoHTMLBody := True;
End;


Function TWPFormatConvertor.HasSourceDocument : Boolean;
Begin
  Result := Assigned(FSourceDocument);
End;


Function TWPFormatConvertor.HasSourceStream : Boolean;
Begin
  Result := Assigned(FSourceStream);
End;


Function TWPFormatConvertor.HasDestinationDocument : Boolean;
Begin
  Result := Assigned(FDestinationDocument);
End;


Function TWPFormatConvertor.HasDestinationStream : Boolean;
Begin
  Result := Assigned(FDestinationStream);
End;




Constructor TWPReader.Create;
Begin
  Inherited;
  FSplitter := TWPWordIterator.Create;

  FStyles := Nil;
  FStream := Nil;
  FFont := Nil;

  FMustCloseWithPara := True;
End;


Destructor TWPReader.Destroy;
Begin
  FDicomDictionary.Free;
  FSplitter.Free;
  FStyles.Free;
  FFont.Free;
  FStream.Free;

  Inherited;
End;


Procedure TWPReader.CheckForEmpty(oDocument : TWPWorkingDocument);
Var
  oPara : TWPWorkingDocumentParaPiece;
  oStyle : TWPStyle;
Begin
  If oDocument.Pieces.Count = 0 Then
  Begin
    oPara := TWPWorkingDocumentParaPiece.Create;
    Try
      oPara.SpeechMagicDouble := SpeechMagicDouble;
      oPara.Style := DEFAULT_STYLE_NAME;
      oStyle := Styles.DefaultStyle;

      If Assigned(oStyle) Then
      Begin
        oPara.Format.Assign(oStyle.Paragraph);
        oPara.Font.Assign(oStyle.Font);
      End;

      oDocument.Pieces.Add(oPara.Link);
    Finally
      oPara.Free;
    End;
  End;
End;


Procedure TWPReader.Read(oDocument : TWPWorkingDocument);
Begin
  If Not ExtendExisting Then
    oDocument.Pieces.Clear;
End;


Procedure TWPReader.CreateUnloadedImage(oImage : TWPWorkingDocumentImagePiece);
Var
  oImg : TFslBitmapGraphic;
  aRect : TRect;
Begin
  oImg := TFslBitmapGraphic.Create;
  Try
    oImg.Height := oImage.Height;
    oImg.Width := oImage.Width;
    oImg.Handle.Canvas.brush.Color := clOlive;
    aRect.Left := 0;
    aRect.Top := 0;
    aRect.Right := oImage.Width;
    aRect.Bottom := oImage.Height;

    oImg.Handle.Canvas.FillRect(Windows.TRect(aRect));

    oImage.Image := oImg.Link;
  Finally
    oImg.Free;
  End;
End;


Procedure TWPReader.LoadImage(oImage : TWPWorkingDocumentImagePiece; oBuffer : TFslBuffer; aFormat : TWPImageFormat; bSelection : Boolean);
Var
  oLoader : TWPImageLoader;
  oAttach : TWPWorkingAttachment;
Begin
  if (aFormat = ifPDF) or ((aFormat = ifUnknown) and SameText(ExtractFileExt(oImage.Name), '.pdf')) then
  begin
    oAttach := TWPWorkingAttachment.create;
    Try
      oAttach.LoadFromBuffer(oBuffer);
//      oImage.Image := TWPPDFGraphic.Create(oAttach.Link);
    Finally
      oAttach.Free;
    End;
  end
  else
  begin
    oLoader := TWPImageLoader.Create;
    Try
      oLoader.DicomDictionary := FDicomDictionary.Link;
      oLoader.Filename := oImage.Name;
      oLoader.Source := TFslMemoryStream.Create;
      TFslMemoryStream(oLoader.Source).Buffer := oBuffer.Link;
      oLoader.Format := aFormat;
      If bSelection Then
        oImage.SelectionImage := oLoader.Load
      Else
        oImage.Image := oLoader.Load;
    Finally
      oLoader.Free;
    End;
  end;
End;


Function TWPReader.GetStyles: TWPStyles;
Begin
  Assert(Invariants('GetStyles', FStyles, TWPStyles, 'FStyles'));

  Result := FStyles;
End;


Procedure TWPReader.SetStyles(Const Value: TWPStyles);
Begin
  FStyles.Free;
  FStyles := Value;
End;


Function TWPReader.HasStyles: Boolean;
Begin
  Result := Assigned(FStyles);
End;


Function TWPReader.GetFont: TWPSFontDetails;
Begin
  Assert(Invariants('GetFont', FFont, TWPSFontDetails, 'Font'));
  Result := FFont;
End;


Function TWPReader.GetStyle: String;
Begin
  Assert(CheckCondition(FStyle <> '', 'GetStyle', 'No Style specified'));
  Result := FStyle;
End;


procedure TWPReader.SetDicomDictionary(const Value: TDicomDictionary);
begin
  FDicomDictionary.Free;
  FDicomDictionary := Value;
end;

Procedure TWPReader.SetFont(Const Value: TWPSFontDetails);
Begin
  FFont.Free;
  FFont := Value;
End;


Function TWPReader.HasFont : Boolean;
Begin
  Result := Assigned(FFont);
End;


Function TWPReader.HasStyle : Boolean;
Begin
  Result := FStyle <> '';
End;


Procedure TWPReader.DoneReading(oDocument : TWPWorkingDocument);
Begin
  oDocument.RegenerateMetrics(True, False);
End;


Procedure TWPReader.Read(oDocument: TWPDocument);
Var
  oWorkingDocument : TWPWorkingDocument;
  oWPDocumentTranslator : TWPDocumentTranslator;
Begin
  oWorkingDocument := TWPWorkingDocument.Create;
  oWPDocumentTranslator := TWPDocumentTranslator.Create;
  Try
    // Read text into a TWPWorkingDocument
    Read(oWorkingDocument);

    // Convert the working document into a TWPDocument
    oWPDocumentTranslator.WorkingStyles := Styles.Link;
    oWPDocumentTranslator.WorkingDocument := oWorkingDocument.Link;
    oWPDocumentTranslator.Document := oDocument.Link;

    oWPDocumentTranslator.TranslateToDocument;
  Finally
    oWPDocumentTranslator.Free;
    oWorkingDocument.Free;
  End;
End;


Function TWPReader.GetStream: TFslStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslStream, 'FStream'));

  Result := FStream;
End;


Procedure TWPReader.SetStream(Const Value: TFslStream);
Begin
  FStream.Free;
  FStream := Value;
End;




Constructor TWPWriter.Create;
Begin
  Inherited;

  FStyles := Nil;
  FStream := Nil;
End;


Destructor TWPWriter.Destroy;
Begin
  FStyles.Free;
  FStream.Free;
  FImageContext.Free;

  Inherited;
End;


Procedure TWPWriter.Initialise;
Begin
End;


Procedure TWPWriter.Finalise;
Begin
End;


Procedure TWPWriter.Write(oDocument : TWPWorkingDocument);
Var
  oIterator : TWPPieceIterator;
Begin
  Initialise;
  if oDocument.Pieces[oDocument.Pieces.Count-1].PieceType = ptPara then
    oDocument.Pieces[oDocument.Pieces.Count-1].Font.Clear;

  oIterator := TWPPieceIterator.Create(oDocument.Link);
  Try
    oIterator.PieceTypes := ALL_PIECE_TYPES;
    oIterator.First;

    IterateDocument(oIterator, oDocument);
  Finally
    oIterator.Free;
  End;

  Finalise;
End;


Procedure TWPWriter.IterateDocument(oIterator : TWPPieceIterator; oDocument : TWPWorkingDocument);
Begin
  WriteDocumentStart(oDocument);

  While oIterator.More Do
  Begin
    If oIterator.Current.PieceType = ptSectionStart Then
      IterateSection(oIterator)
    Else If oIterator.Current.PieceType = ptTableStart Then
      IterateTable(oIterator)
    Else If oIterator.Current.PieceType = ptBreak Then
      IterateBreak(oIterator)
    Else
      IterateParagraph(oIterator, Nil);
  End;
  
  WriteDocumentStop(oDocument);
End;


Procedure TWPWriter.IterateBreak(oIterator : TWPPieceIterator);
Begin
  WriteBreak(TWPWorkingDocumentBreakPiece(oIterator.Current));
  
  oIterator.Next;
End;


Procedure TWPWriter.IterateSectionInner(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  While oIterator.More And (oIterator.Current.PieceType <> ptSectionStop) Do
  Begin
    If oIterator.Current.PieceType = ptSectionStart Then
      IterateSection(oIterator)
    Else If oIterator.Current.PieceType = ptBreak Then
      IterateBreak(oIterator)
    Else If oIterator.Current.PieceType = ptTableStart Then
      IterateTable(oIterator)
    Else
      IterateParagraph(oIterator, oSection);
  End;
End;


Procedure TWPWriter.IterateSection(oIterator : TWPPieceIterator);
Var
  oSection : TWPWorkingDocumentSectionStartPiece;
Begin
  oSection := TWPWorkingDocumentSectionStartPiece(oIterator.Current);

  WriteSectionStart(oSection);

  oIterator.Next;

  IterateSectionInner(oIterator, oSection);

  If oIterator.Current.PieceType = ptSectionStop Then
    WriteSectionStop(oSection, TWPWorkingDocumentStopPiece(oIterator.Current))
  Else
    WriteSectionStop(oSection, Nil);

  If oIterator.More Then
    oIterator.Next;
End;


Procedure TWPWriter.IterateTable(oIterator : TWPPieceIterator);
Var
  oTable : TWPWorkingDocumentTableStartPiece;
Begin
  oTable := TWPWorkingDocumentTableStartPiece(oIterator.Current);

  oIterator.Next;

  WriteTableStart(oTable);

  While oIterator.More And (oIterator.Current.PieceType <> ptTableStop) Do
  Begin
    If oIterator.Current.PieceType = ptRowStart Then
      IterateTableRow(oIterator)
    Else
      RaiseError('IterateTable', 'Encountered a piece of type '+NAMES_WPPIECETYPE[oIterator.Current.PieceType]+' iterating a table');
  End;

  If oIterator.Current.PieceType = ptTableStop Then
    WriteTableStop(oTable, TWPWorkingDocumentStopPiece(oIterator.Current))
  Else
    WriteTableStop(oTable, Nil);

  If oIterator.More Then
    oIterator.Next;
End;


Procedure TWPWriter.IterateTableRow(oIterator : TWPPieceIterator);
Var
  oTableRow : TWPWorkingDocumentTableRowStartPiece;
  oStop : TWPWorkingDocumentStopPiece;
Begin
  oTableRow := TWPWorkingDocumentTableRowStartPiece(oIterator.Current);

  oIterator.Next;

  WriteTableRowStart(oTableRow);

  While oIterator.More And (oIterator.Current.PieceType <> ptRowStop) Do
  Begin
    If oIterator.Current.PieceType = ptCellStart Then
      IterateTableCell(oIterator)
    Else
      RaiseError('IterateTableRow', 'Encountered a piece of type '+NAMES_WPPIECETYPE[oIterator.Current.PieceType]+' iterating a TableRow');
  End;

  oStop := Nil;
  If oIterator.Current.PieceType = ptRowStop Then
    oStop := TWPWorkingDocumentStopPiece(oIterator.Current);
  If oIterator.More Then
    oIterator.Next;

  // now we decide - do we look ahead for nested rows before closing this row
  While oIterator.More And (oIterator.Current.PieceType = ptRowStart) And
      (TWPWorkingDocumentTableRowStartPiece(oIterator.Current).HasOwner) And (TWPWorkingDocumentTableRowStartPiece(oIterator.Current).Owner = oTableRow) Do
    IterateTableRow(oIterator);

  WriteTableRowStop(oTableRow, oStop, oIterator.Current.PieceType = ptTableStop);
End;


Procedure TWPWriter.IterateTableCell(oIterator : TWPPieceIterator);
Var
  oTableCell : TWPWorkingDocumentTableCellStartPiece;
Begin
  oTableCell := TWPWorkingDocumentTableCellStartPiece(oIterator.Current);

  oIterator.Next;

  WriteTableCellStart(oTableCell);

  While oIterator.More And (oIterator.Current.PieceType <> ptCellStop) Do
  Begin
    If oIterator.Current.PieceType = ptBreak Then
      IterateBreak(oIterator)
    Else
      IterateParagraph(oIterator, Nil);
  End;

  If oIterator.Current.PieceType = ptCellStop Then
    WriteTableCellStop(oTableCell, TWPWorkingDocumentStopPiece(oIterator.Current))
  Else
    WriteTableCellStop(oTableCell, Nil);

  If oIterator.More Then
    oIterator.Next;
End;


Procedure TWPWriter.IterateParagraphContents(oIterator : TWPPieceIterator);
Begin
  While oIterator.More And (oIterator.Current.PieceType <> ptPara) Do
  Begin
    Case oIterator.Current.PieceType Of
      ptText :
      Begin
        WriteText(TWPWorkingDocumentTextPiece(oIterator.Current));
      End;

      ptImage :
      Begin
        WriteImage(TWPWorkingDocumentImagePiece(oIterator.Current));
      End;

      ptFieldStart :
      Begin
        IterateField(oIterator);
      End;

      ptLineBreak :
      Begin
        WriteLineBreak(TWPWorkingDocumentLineBreakPiece(oIterator.Current));
      End;
    Else
      RaiseError('IterateParagraph', 'Encountered a piece of type '+NAMES_WPPIECETYPE[oIterator.Current.PieceType]+' iterating a paragraph');
    End;

    oIterator.Next;
  End;
End;

Procedure TWPWriter.IterateParagraph(oIterator : TWPPieceIterator; oSection : TWPWorkingDocumentSectionStartPiece);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := oIterator.CurrentParagraph(True);

  WriteParagraphStart(oPara);

  IterateParagraphContents(oIterator);

  WriteParagraphStop(oPara, (oIterator.Peek <> Nil) And (oIterator.Peek.PieceType = ptSectionStop), oSection);

  oIterator.Next;
End;


Procedure TWPWriter.IterateField(oIterator : TWPPieceIterator);
Var
  oField : TWPWorkingDocumentFieldStartPiece;
Begin
  oField := TWPWorkingDocumentFieldStartPiece(oIterator.Current);

  WriteFieldStart(oField);

  oIterator.Next;

  While oIterator.More And (oIterator.Current.PieceType <> ptFieldStop) Do
  Begin
    Case oIterator.Current.PieceType Of
      ptText :
      Begin
        WriteText(TWPWorkingDocumentTextPiece(oIterator.Current));
      End;

      ptImage :
      Begin
        WriteImage(TWPWorkingDocumentImagePiece(oIterator.Current));
      End;

      ptLineBreak :
      Begin
        WriteLineBreak(TWPWorkingDocumentLineBreakPiece(oIterator.Current));
      End;
    Else
      RaiseError('IterateField', 'Encountered a piece of type '+NAMES_WPPIECETYPE[oIterator.Current.PieceType]+' iterating a paragraph');
    End;

    oIterator.Next;
  End;

  WriteFieldStop(oField, TWPWorkingDocumentFieldStopPiece(oIterator.Current));
End;


Procedure TWPWriter.WriteLineBreak(oBreak: TWPWorkingDocumentLineBreakPiece);
Begin
End;


Procedure TWPWriter.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
End;


Procedure TWPWriter.WriteDocumentStart(oDocument: TWPWorkingDocument);
Begin
End;


Procedure TWPWriter.WriteDocumentStop(oDocument: TWPWorkingDocument);
Begin
End;


Procedure TWPWriter.WriteFieldStart(oField: TWPWorkingDocumentFieldStartPiece);
Begin
End;


Procedure TWPWriter.WriteFieldStop(oField : TWPWorkingDocumentFieldStartPiece; oStop : TWPWorkingDocumentFieldStopPiece);
Begin
End;


Procedure TWPWriter.WriteImage(oImage: TWPWorkingDocumentImagePiece);
Begin
End;


Procedure TWPWriter.WriteParagraphStart(oParagraph: TWPWorkingDocumentParaPiece);
Begin
End;


Procedure TWPWriter.WriteParagraphStop(oParagraph: TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
End;


Procedure TWPWriter.WriteSectionStart(oSection: TWPWorkingDocumentSectionStartPiece);
Begin
End;


Procedure TWPWriter.WriteSectionStop(oSection : TWPWorkingDocumentSectionStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
End;


Procedure TWPWriter.WriteText(oText: TWPWorkingDocumentTextPiece);
Begin
End;


Procedure TWPWriter.WriteTableStart(oTable : TWPWorkingDocumentTableStartPiece);
Begin
End;


Procedure TWPWriter.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
End;


Procedure TWPWriter.WriteTableRowStart(oTableRow : TWPWorkingDocumentTableRowStartPiece);
Begin
End;


Procedure TWPWriter.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : boolean);
Begin
End;


Procedure TWPWriter.WriteTableCellStart(oTableCell : TWPWorkingDocumentTableCellStartPiece);
Begin
End;


Procedure TWPWriter.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
End;


Class Procedure TWPWriter.SavePNGToStream(oImage: TFslVCLGraphic; oStream: TFslStream);
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


Procedure TWPWriter.Write(oDocument : TWPDocument);
Var
  oWorking : TWPWorkingDocument;
  oTranslator : TWPDocumentTranslator;
Begin
  oWorking := TWPWorkingDocument.Create;
  Try
    oTranslator := TWPDocumentTranslator.Create;
    Try
      oTranslator.Document := oDocument.Link;
      oTranslator.WorkingDocument := oWorking.Link;
      oTranslator.WorkingStyles := Styles.Link;
      oTranslator.TranslateToWorking;

      Write(oWorking);
    Finally
      oTranslator.Free;
    End;
  Finally
    oWorking.Free;
  End;
End;


Function TWPWriter.GetStyles : TWPStyles;
Begin
  Assert(Invariants('GetStyles', FStyles, TWPStyles, 'Styles'));
  Result := FStyles;
End;


Procedure TWPWriter.SetStyles(Const Value : TWPStyles);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetStyles', Value, TWPStyles, 'Value'));

  FStyles.Free;
  FStyles := Value;
End;


Function TWPWriter.GetStream : TFslStream;
Begin
  Assert(Invariants('GetStream', FStream, TFslStream, 'Stream'));

  Result := FStream;
End;


procedure TWPWriter.SetImageContext(const Value: TFslObject);
begin
  FImageContext.Free;
  FImageContext := Value;
end;

Procedure TWPWriter.SetStream(Const Value : TFslStream);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetStream', Value, TFslStream, 'Value'));

  FStream.Free;
  FStream := Value;
End;


Function TWPWriter.SupportsNestedRows: Boolean;
Begin
  Result := False;
End;


Function ReadFixedFormatAttribute(sValue : String) : TWPDocumentFieldFixedFormat;
begin
  if StringArrayIndexOfSensitive(TWPDOCUMENTOBJECT_FIXED_FORMAT, sValue) > -1 then
    result := TWPDocumentFieldFixedFormat(StringArrayIndexOfSensitive(TWPDOCUMENTOBJECT_FIXED_FORMAT, sValue))
  else if StrToBoolDef(sValue, False) Then
    result := fffFixed
  else
    result := fffAnyPart;
end;

End.
