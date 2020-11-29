unit wp_factory;

{
Copyright (c) 2010+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

interface

uses
  SysUtils, Classes,
  GraphicEx,
  fsl_base, fsl_stream, fsl_utilities,
  wp_graphics, wp_graphics_ex,
  wp_types, wp_document, wp_working, wp_format, wp_clipboard, wp_text, wp_odt, wp_html, wp_rtf, wp_native;

type

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

    function sizeInBytesV : cardinal; override;
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




Function ReaderForFormat(aFormat : TWPFormat) : TWPReaderClass;
Function ReaderForContentType(aContentType : TWPClipboardContentType) : TWPReaderClass;
Function WriterForFormat(aFormat : TWPFormat) : TWPWriterClass;


Function LoadGraphic(const oGraphic : TGraphic) : TFslVCLGraphic;


implementation



Function LoadGraphic(const oGraphic: TGraphic): TFslVCLGraphic;
Var
  oGraphicExGraphicClass : TGraphicExGraphicClass;
  oVCLStream : TVCLStream;
Begin
  Result := Nil;

  oVCLStream := TVCLStream.Create;
  Try
    oVCLStream.Stream := TFslMemoryStream.Create;
    oGraphic.SaveToStream(oVCLStream);
    oVCLStream.Position := 0;

    oGraphicExGraphicClass := GraphicEx.FileFormatList.GraphicFromContent(oVCLStream);

    If Assigned(oGraphicExGraphicClass) Then
    Begin
      If oGraphicExGraphicClass = TTIFFGraphic Then
        Result := TFslTIFFGraphic.Create
      Else If oGraphicExGraphicClass = TPNGGraphic Then
        Result := TFslPortableNetworkGraphic.Create
      Else If oGraphicExGraphicClass = TGIFGraphic Then
        Result := TFslGIFGraphic.Create
      Else
        Raise EFslException.Create('TFslVCLGraphic', 'NewFromGraphic', StringFormat('Image file format ''%s'' is not supported.', [oGraphicExGraphicClass.ClassName]));
    End
    Else
    Begin
      // TBitmap, TJpegImage cannot be looked up as they don't descend from TGraphicExGraphic.

      If TFslBitmapGraphic.CanLoad(oVCLStream) Then
        Result := TFslBitmapGraphic.Create
      Else {If TFslJpegImage.CanLoad(oVCLStream) Then}
        Result := TFslJPegGraphic.Create;
    End;

    oVCLStream.Position := 0;
    Result.LoadFromStream(oVCLStream.Stream);
  Finally
    oVCLStream.Free;
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




function TWPFormatConvertor.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSourceDocument.sizeInBytes);
  inc(result, FSourceStream.sizeInBytes);
  inc(result, FDestinationDocument.sizeInBytes);
  inc(result, FDestinationStream.sizeInBytes);
end;

end.
