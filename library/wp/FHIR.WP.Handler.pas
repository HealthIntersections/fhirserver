Unit FHIR.WP.Handler;

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
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_xml,

  wp_document, wp_working, wp_types,
  wp_format, wp_native, wp_html, wp_rtf, wp_text, wp_odt, FHIR.WP.Cda, FHIR.WP.V2Ft,
  FHIR.WP.Settings;


Type
  TWPDocumentEvent = Procedure (oSender : TObject; oDocument : TWPDocument) Of Object;
  TWPSaveImageEvent = wp_format.TWPSaveImageEvent;
  TWPLoadImageEvent = wp_format.TWPLoadImageEvent;

  TWPDocumentHandler = Class (TFslObject)
    Private
      FOnSaveImage : TWPSaveImageEvent;
      FOnLoadImage : TWPLoadImageEvent;
      FOnNewDocument: TWPDocumentEvent;
      FSettings : TWPSettings;

      Function GetAsHTML : String;
      Procedure SetAsHTML(sContent : String);
      Function GetAsRTF : String;
      Procedure SetAsRTF(sContent : String);
      Function GetAsHL7 : String;
      Procedure SetAsHL7(sContent : String);
      Function GetAsNative : String;
      Procedure SetAsNative(sContent : String);
      Function GetAsText : String;
      Procedure SetAsText(sContent : String);

      Procedure SetSettings(oSettings : TWPSettings);
      function GetAsLogicalText: String;
    Protected
      Function GetHost : TObject; Virtual;
      Function GetHostConfiguredStyles : TWPStyles; Virtual;
      Function GetHostWorkingStyles : TWPStyles; Virtual;
      Function GetHostDocument : TWPWorkingDocument; Virtual;
      Procedure SetHostDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles); Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      destructor Destroy; Override;

      Procedure LoadHTML(Const sFilename : String); Overload; Virtual;
      Procedure LoadHTML(oStream : TFslStream; Context : String = ''); Overload; Virtual;
      Procedure LoadHTML(oBuffer : TFslBuffer; Context : String = ''); Overload; Virtual;
      Procedure SaveHTML(Const sFilename : String); Overload; Virtual;
      Procedure SaveHTML(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveHTML(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Procedure SaveMHT(Const sFilename : String); Overload; Virtual;
      Procedure SaveMHT(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveMHT(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Procedure LoadRTF(Const sFilename : String); Overload; Virtual;
      Procedure LoadRTF(oStream : TFslStream); Overload; Virtual;
      Procedure LoadRTF(oBuffer : TFslBuffer); Overload; Virtual;
      Procedure SaveRTF(Const sFilename : String); Overload; Virtual;
      Procedure SaveRTF(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveRTF(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Procedure LoadCDA(Const sFilename : String); Overload; Virtual;
      Procedure LoadCDA(oStream : TFslStream); Overload; Virtual;
      Procedure LoadCDA(oBuffer : TFslBuffer); Overload; Virtual;
      Procedure SaveCDA(Const sFilename : String); Overload; Virtual;
      Procedure SaveCDA(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveCDA(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Procedure LoadODT(Const sFilename : String); Overload; Virtual;
      Procedure LoadODT(oStream : TFslStream); Overload; Virtual;
      Procedure LoadODT(oBuffer : TFslBuffer); Overload; Virtual;
      Procedure SaveODT(Const sFilename : String); Overload; Virtual;
      Procedure SaveODT(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveODT(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Procedure LoadHL7(Const sFilename : String); Overload; Virtual;
      Procedure LoadHL7(oStream : TFslStream); Overload; Virtual;
      Procedure LoadHL7(oBuffer : TFslBuffer); Overload; Virtual;
      Procedure SaveHL7(Const sFilename : String); Overload; Virtual;
      Procedure SaveHL7(oStream : TFslStream; Const sTitle : String); Overload; Virtual;
      Procedure SaveHL7(oBuffer : TFslBuffer; Const sTitle : String); Overload; Virtual;

      Function LoadByXmlType(Const sFilename : String) : TWPFormat; Overload; Virtual;
      function LoadNativeOrSnapshot(Const sFilename : String) : TWPFormat; Overload; Virtual;

      Procedure LoadNative(Const sFilename : String); Overload; Virtual;
      Procedure LoadNative(oStream : TFslStream; Context : String = ''); Overload; Virtual;
      Procedure LoadNative(oBuffer : TFslBuffer; Context : String = ''); Overload; Virtual;
      Procedure SaveNative(Const sFilename : String); Overload; Virtual;
      Procedure SaveNative(oStream : TFslStream; bImages : Boolean); Overload; Virtual;
      Procedure SaveNative(oBuffer : TFslBuffer; bImages : Boolean); Overload; Virtual;

      Procedure LoadText(Const sFilename : String); Overload; Virtual;
      Procedure LoadText(oStream : TFslStream); Overload; Virtual;
      Procedure LoadText(oBuffer : TFslBuffer); Overload; Virtual;
      Procedure SaveText(Const sFilename : String); Overload; Virtual;
      Procedure SaveText(oStream : TFslStream); Overload; Virtual;
      Procedure SaveText(oBuffer : TFslBuffer); Overload; Virtual;

      function SaveByExtension(Const sFilename : String) : TWPFormat; Overload; Virtual;
      function LoadByExtension(Const sFilename : String) : TWPFormat; Overload; Virtual;
      procedure SaveByFormat(Const sFilename : String; aFormat : TWPFormat); Overload; Virtual;
      procedure SaveByFormat(buffer : TFslBuffer; aFormat : TWPFormat; title : string); Overload; Virtual;
      procedure loadByFormat(Const sFilename : String; aFormat : TWPFormat); Overload; Virtual;

      Procedure LoadSnapshot(Const sFilename : String); Overload; Virtual;
      Procedure LoadSnapshot(oStream : TFslStream); Overload; Virtual;
      Procedure LoadSnapshot(oBuffer : TFslBuffer); Overload; Virtual;

      Property AsHTML : String Read GetAsHTML Write SetAsHTML;
      Property AsRTF : String Read GetAsRTF Write SetAsRTF;
      Property AsHL7 : String Read GetAsHL7 Write SetAsHL7;
      Property AsNative : String Read GetAsNative Write SetAsNative;
      Property AsText : String Read GetAsText Write SetAsText;
      Property AsLogicalText : String read GetAsLogicalText;

      Procedure NewDocument; Overload; Virtual; // load the default new document in to the word processor
      Procedure LoadDocument(Const oDocument : TWPDocument); Overload; Virtual; // load oDocument into the wordprocessor
      Procedure SaveDocument(Const oDocument : TWPDocument); Overload; Virtual; // save the currently loaded document into oDocument

      Property OnSaveImage : TWPSaveImageEvent Read FOnSaveImage Write FOnSaveImage;
      Property OnLoadImage : TWPLoadImageEvent Read FOnLoadImage Write FOnLoadImage;
      Property OnNewDocument : TWPDocumentEvent Read FOnNewDocument Write FOnNewDocument;

      Property Settings : TWPSettings Read FSettings Write SetSettings; 
  End;


Implementation


Procedure TWPDocumentHandler.LoadHTML(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadHTML(oFile, 'file://'+PathFolder(sFilename));
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadHTML(oStream : TFslStream; Context : String = '');
Var
  oReader : TWPHTMLReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPHTMLReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Context := Context;
    oReader.Read(oDocument);

    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadHTML(oBuffer: TFslBuffer; Context : String = '');
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;

    LoadHTML(oStream, Context);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHTML(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveHTML(oStream, sTitle);

    oBuffer.Format := 'text/html';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHTML(oStream: TFslStream; Const sTitle : String);
Var
  oHTMLer : TWPHTMLWriter;
Begin
  oHTMLer := TWPHTMLWriter.Create;
  Try
    oHTMLer.Stream := oStream.Link;
    oHTMLer.Styles := GetHostWorkingStyles.Link;
    oHTMLer.Title := sTitle;
    oHTMLer.EmbedStyles := True;
    oHTMLer.OnSaveImage := FOnSaveImage;
    oHTMLer.Write(GetHostDocument);
  Finally
    oHTMLer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHTML(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveHTML(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;


Procedure TWPDocumentHandler.SaveMHT(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveMHT(oStream, sTitle);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveMHT(oStream: TFslStream; Const sTitle : String);
Var
  oMHTer : TWPMHTWriter;
Begin
  oMHTer := TWPMHTWriter.Create;
  Try
    oMHTer.Stream := oStream.Link;
    oMHTer.Styles := GetHostWorkingStyles.Link;
    oMHTer.Title := sTitle;
    oMHTer.EmbedStyles := True;
    oMHTer.OnSaveImage := FOnSaveImage;
    oMHTer.Write(GetHostDocument);
  Finally
    oMHTer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveMHT(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveMHT(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadRTF(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadRTF(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadRTF(oStream : TFslStream);
Var
  oReader : TWPRTFReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPRTFReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Read(oDocument);
    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadRTF(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadRTF(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveRTF(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveRTF(oStream, sTitle);

    oBuffer.Format := 'application/rtf';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveRTF(oStream: TFslStream; Const sTitle : String);
Var
  oRTFer : TWPRTFWriter;
Begin
  oRTFer := TWPRTFWriter.Create;
  Try
    oRTFer.Stream := oStream.Link;
    oRTFer.Styles := GetHostWorkingStyles.Link;
    oRTFer.EmbedStyles := True;
    oRTFer.OnSaveImage := FOnSaveImage;
    oRTFer.Write(GetHostDocument);
  Finally
    oRTFer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveRTF(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveRTF(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadCDA(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadCDA(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadCDA(oStream : TFslStream);
Var
  oReader : TWPCdaReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPCdaReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Read(oDocument);
    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadCDA(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadCDA(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveCDA(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveCDA(oStream, sTitle);

    oBuffer.Format := 'application/cda';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveCDA(oStream: TFslStream; Const sTitle : String);
Var
  oCDAer : TWPCdaWriter;
Begin
  oCDAer := TWPCdaWriter.Create;
  Try
    oCDAer.Stream := oStream.Link;
    oCDAer.Styles := GetHostWorkingStyles.Link;
    oCDAer.EmbedStyles := True;
    oCDAer.OnSaveImage := FOnSaveImage;
    oCDAer.Write(GetHostDocument);
  Finally
    oCDAer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveCDA(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveCDA(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadODT(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadODT(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadODT(oStream : TFslStream);
Var
  oReader : TWPOdtReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPOdtReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Read(oDocument);
    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadODT(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadODT(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveODT(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveODT(oStream, sTitle);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveODT(oStream: TFslStream; Const sTitle : String);
Var
  oODTer : TWPOdtWriter;
Begin
  oODTer := TWPOdtWriter.Create;
  Try
    oODTer.Stream := oStream.Link;
    oODTer.Styles := GetHostWorkingStyles.Link;
    oODTer.EmbedStyles := True;
    oODTer.OnSaveImage := FOnSaveImage;
    oODTer.Write(GetHostDocument);
  Finally
    oODTer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveODT(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveODT(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadHL7(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadHL7(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadHL7(oStream : TFslStream);
Var
  oReader : TWPHL7FTReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPHL7FTReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Read(oDocument);
    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadHL7(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadHL7(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHL7(oBuffer: TFslBuffer; Const sTitle: String);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveHL7(oStream, sTitle);

    oBuffer.Format := 'application/hl7';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHL7(oStream: TFslStream; Const sTitle : String);
Var
  oHL7er : TWPHL7FTWriter;
Begin
  oHL7er := TWPHL7FTWriter.Create;
  Try
    oHL7er.Stream := oStream.Link;
    oHL7er.Styles := GetHostWorkingStyles.Link;
    oHL7er.EmbedStyles := True;
    oHL7er.OnSaveImage := FOnSaveImage;
    oHL7er.Width := 88; // actual limit is 90 for Medical Director. keep a couple of chars up our sleeve 
    oHL7er.Write(GetHostDocument);
  Finally
    oHL7er.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveHL7(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveHL7(oFile, PathTitle(sFileName));
  Finally
    oFile.Free;
  End;
End;


function TWPDocumentHandler.LoadNativeOrSnapshot(Const sFilename : String) : TWPFormat;
Var
  oDoc : TMXmlDocument;
  bSnap : Boolean;
  oFile : TFileStream;
Begin
  oDoc := TMXmlParser.parseFile(sFilename, []);
  Try
    bSnap := oDoc.docElement.Name = 'snapshot';
  Finally
    oDoc.Free;
  End;
  If bSnap Then
  begin
    result := wpfSnapshot;
    LoadSnapshot(sFilename)
  end
  Else
  begin
    result := wpfNative;
    LoadNative(sFilename);
  end;
End;


Procedure TWPDocumentHandler.LoadNative(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadNative(oFile, 'file://'+PathFolder(sFilename));
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadNative(oStream : TFslStream; Context : String = '');
Var
  oReader : TWPNativeReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  Try
    oReader := TWPNativeReader.Create;
    Try
      oReader.Stream := oStream.Link;
      oReader.Styles := GetHostConfiguredStyles.Clone;
      oReader.OnLoadImage := FOnLoadImage;
      oReader.Context := Context;
      oReader.Read(oDocument);
      SetHostDocument(oDocument, oReader.Styles);
    Finally
      oReader.Free;
    End;
  Finally
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadNative(oBuffer: TFslBuffer; Context : String = '');
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadNative(oStream, Context);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveNative(oBuffer: TFslBuffer; bImages : Boolean);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveNative(oStream, bImages);

    oBuffer.Format := 'application/xml';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveNative(oStream: TFslStream; bImages : Boolean);
Var
  oNativeer : TWPNativeWriter;
Begin
  oNativeer := TWPNativeWriter.Create;
  Try
    oNativeer.Stream := oStream.Link;
    oNativeer.Styles := GetHostWorkingStyles.Link;
    oNativeer.EmbedStyles := True;
    oNativeer.ImagesAsReferences := Not bImages;
    oNativeer.OnSaveImage := FOnSaveImage;
    oNativeer.Write(GetHostDocument);
  Finally
    oNativeer.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveNative(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveNative(oFile, True);
  Finally
    oFile.Free;
  End;
End;


Function TWPDocumentHandler.GetAsHTML: String;
Var
  oStream : TFslStringStream;
  chars: TArray<Char>;
Begin
  oStream := TFslStringStream.Create;
  Try
    SaveHTML(oStream, 'HTML');
    result := TEncoding.UTF8.GetString(oStream.Bytes);
  Finally
    oStream.Free;
  End;
End;

Function TWPDocumentHandler.GetAsRTF: String;
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
    SaveRTF(oStream, 'RTF');
    Result := string(oStream.Data);
  Finally
    oStream.Free;
  End;
End;

Function TWPDocumentHandler.GetAsHL7: String;
Var
  oStream : TFslStringStream;
{$IFNDEF VER130}
  chars: SysUtils.TCharArray;
{$ENDIF}
Begin
  oStream := TFslStringStream.Create;
  Try
    SaveHL7(oStream, 'HL7');
{$IFDEF VER130}
    Result := oStream.Data;
{$ELSE}
    chars := TEncoding.UTF8.GetChars(oStream.Bytes);
    SetString(Result, PChar(chars), Length(chars));
{$ENDIF}
  Finally
    oStream.Free;
  End;
End;

Function TWPDocumentHandler.GetAsNative: String;
Var
  oStream : TFslStringStream;
{$IFNDEF VER130}
  chars: SysUtils.TCharArray;
{$ENDIF}
Begin
  oStream := TFslStringStream.Create;
  Try
    SaveNative(oStream, True);
{$IFDEF VER130}
    Result := oStream.Data;
{$ELSE}
    chars := TEncoding.UTF8.GetChars(oStream.Bytes);
    SetString(Result, PChar(chars), Length(chars));
{$ENDIF}
  Finally
    oStream.Free;
  End;
End;

Function TWPDocumentHandler.GetAsText: String;
Var
  oStream : TFslStringStream;
{$IFNDEF VER130}
  chars: SysUtils.TCharArray;
{$ENDIF}
Begin
  oStream := TFslStringStream.Create;
  Try
    SaveText(oStream);
{$IFDEF VER130}
    Result := oStream.Data;
{$ELSE}
    chars := TEncoding.UTF8.GetChars(oStream.Bytes);
    SetString(Result, PChar(chars), Length(chars));
{$ENDIF}
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadText(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadText(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadText(oStream : TFslStream);
Var
  oReader : TWPTextReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPTextReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.Read(oDocument);

    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadText(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadText(oStream);
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveText(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    oStream.Size := 0;
    SaveText(oStream);

    oBuffer.Format := 'text/plain';
  Finally
    oStream.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveText(oStream: TFslStream);
Var
  oTexter : TWPTextWriter;
Begin
  oTexter := TWPTextWriter.Create;
  Try
    oTexter.Stream := oStream.Link;
    oTexter.Styles := GetHostWorkingStyles.Link;
    oTexter.Write(GetHostDocument);
  Finally
    oTexter.Free;
  End;
End;

Procedure TWPDocumentHandler.SaveText(Const sFilename: String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmCreate);
  Try
    SaveText(oFile);
  Finally
    oFile.Free;
  End;
End;


Procedure TWPDocumentHandler.SetAsHTML(sContent : String);
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oStream.Data := sContent;
{$ELSE}
    oStream.Bytes := TEncoding.UTF8.GetBytes(sContent);
{$ENDIF}
    LoadHTML(oStream);
  Finally
    oStream.Free;
  End;
End;


Procedure TWPDocumentHandler.SetAsRTF(sContent : String);
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
    oStream.Data := AnsiString(sContent);
    LoadRTF(oStream);
  Finally
    oStream.Free;
  End;
End;


Procedure TWPDocumentHandler.SetAsHL7(sContent : String);
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oStream.Data := sContent;
{$ELSE}
    oStream.Bytes := TEncoding.UTF8.GetBytes(sContent);
{$ENDIF}
    LoadHL7(oStream);
  Finally
    oStream.Free;
  End;
End;


Procedure TWPDocumentHandler.SetAsNative(sContent : String);
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oStream.Data := sContent;
{$ELSE}
    oStream.Bytes := TEncoding.UTF8.GetBytes(sContent);
{$ENDIF}
    LoadNative(oStream);
  Finally
    oStream.Free;
  End;
End;


Procedure TWPDocumentHandler.SetAsText(sContent : String);
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
{$IFDEF VER130}
    oStream.Data := sContent;
{$ELSE}
    oStream.Bytes := TEncoding.UTF8.GetBytes(sContent);
{$ENDIF}
    LoadText(oStream);
  Finally
    oStream.Free;
  End;
End;

function TWPDocumentHandler.SaveByExtension(Const sFilename : String) : TWPFormat;
Begin
  result := wpfUnknown;
  Case StringArrayIndexOfInsensitive(['.htm', '.html', '.txt', '.text', '.rtf', '.xml', '.mht', '.odt', '.kdoc', '.cda'], PathExtension(sFilename)) Of
    0, 1:
         begin
         SaveHTML(sFilename);
         result := wpfHTML;
         end;
    2, 3:
         begin
         SaveText(sFilename);
         result := wpfText;
         end;
    4:
         begin
         SaveRTF(sFilename);
         result := wpfRTF;
         end;
    5,8:
         begin
         SaveNative(sFilename);
         result := wpfNative;
         end;
    6:
         begin
         SaveMHT(sFilename);
         result := wpfMHT;
         end;
    7:
         begin
         SaveODT(sFilename);
         result := wpfODT;
         end;
    9:
         begin
         SaveCDA(sFileName);      // Temporarily save CDA if extension is .cda
         result := wpfCDA;
         end;
  Else
    RaiseError('SaveByExtension', 'Unable to determine file format for '+sFilename);
  End;
End;


function TWPDocumentHandler.LoadByExtension(Const sFilename : String) : TWPFormat;
Begin
  result := wpfUnknown;
  Case StringArrayIndexOfInsensitive(['.htm', '.html', '.txt', '.text', '.rtf', '.xml', '.kdoc', '.odt', '.cda', 'snapshot', '.zip', '.cde'], PathExtension(sFilename)) Of
    0, 1:
      begin
      LoadHTML(sFilename);
      result := wpfHTML;
      end;
    2, 3:
      begin
      LoadText(sFilename);
      result := wpfText;
      end;
    4:
      begin
      LoadRTF(sFilename);
      result := wpfRtf;
      end;
    5:
      begin
      result := LoadByXmlType(sFilename);
      end;
    6, 12:
      begin
      LoadNative(sFilename);
      result := wpfNative;
      end;
    7:
      begin
      LoadODT(sFilename);
      result := wpfODT;
      end;
    8, 11:
      begin
      LoadCDA(sFileName);      // Temporarily load CDA if extension is .cda
      result := wpfCDA;
      end;
    9:
      begin
      result := LoadNativeOrSnapshot(sFilename);
      end;
  Else
    RaiseError('LoadByExtension', 'Unable to determine file format for '+sFilename);
  End;
End;


function TWPDocumentHandler.LoadByXmlType(const sFilename: String): TWPFormat;
Var
  oDom : TMXmlDocument;
Begin
  oDom := TMXmlParser.parseFile(sFilename, [xpResolveNamespaces]);
  Try
    if oDom.docElement.Name = 'snapshot' then
    begin
      result := wpfSnapshot;
      LoadSnapshot(sFilename)
    end
    Else if oDom.docElement.Name = 'ClinicalDocument' then
    begin
      result := wpfCDA;
      LoadCDA(sFilename)
    end
    Else
    begin
      result := wpfNative;
      LoadNative(sFilename);
    end;
  Finally
    oDom.Free;
  End;
end;

Function TWPDocumentHandler.GetHost : TObject;
Begin
  Result := Nil;
  RaiseError('GetHost', 'Need to override '+ClassName+'.GetHost');
End;


Function TWPDocumentHandler.GetHostConfiguredStyles : TWPStyles;
Begin
  Result := Nil;
  RaiseError('GetHostConfiguredStyles', 'Need to override '+ClassName+'.GetHostConfiguredStyles');
End;


Function TWPDocumentHandler.GetHostWorkingStyles : TWPStyles;
Begin
  Result := Nil;
  RaiseError('GetHostWorkingStyles', 'Need to override '+ClassName+'.GetHostWorkingStyles');
End;


Function TWPDocumentHandler.GetHostDocument : TWPWorkingDocument;
Begin
  Result := Nil;
  RaiseError('GetHostDocument', 'Need to override '+ClassName+'.GetHostDocument');
End;


Procedure TWPDocumentHandler.SetHostDocument(Const oDocument : TWPWorkingDocument; oStyles : TWPStyles);
Begin
  RaiseError('SetHostDocument', 'Need to override '+ClassName+'.SetHostDocument');
End;


Procedure TWPDocumentHandler.LoadDocument(Const oDocument : TWPDocument);
Var
  oWorking : TWPWorkingDocument;
  oTranslator : TWPDocumentTranslator;
Begin
  {$IFOPT C+}
  TWPDocumentValidator.ValidateAssert(oDocument);
  {$ENDIF}
  oWorking := TWPWorkingDocument.Create;
  Try
    oTranslator := TWPDocumentTranslator.Create;
    Try
      oTranslator.WorkingDocument := oWorking.Link;
      oTranslator.WorkingStyles := GetHostConfiguredStyles.Clone;
      oTranslator.Document := oDocument.Link;
      oTranslator.TranslateToWorking;
      SetHostDocument(oWorking, oTranslator.WorkingStyles);
    Finally
      oTranslator.Free;
    End;
  Finally
    oWorking.Free;
  End;
End;


Procedure TWPDocumentHandler.SaveDocument(Const oDocument : TWPDocument);
Var
  oTranslator : TWPDocumentTranslator;
Begin
  oTranslator := TWPDocumentTranslator.Create;
  Try
    oTranslator.WorkingDocument := GetHostDocument.Link;
    oTranslator.WorkingStyles := GetHostWorkingStyles.Clone;
    oTranslator.Document := oDocument.Link;
    oTranslator.TranslateToDocument;
  Finally
    oTranslator.Free;
  End;
End;


Procedure TWPDocumentHandler.NewDocument;
Var
  oDocument : TWPDocument;
  oWorking : TWPWorkingDocument;
  oTranslator : TWPDocumentTranslator;
Begin
  oDocument := TWPDocument.Create;
  Try

    If Assigned(FOnNewDocument) Then
      FOnNewDocument(GetHost, oDocument);

    oWorking := TWPWorkingDocument.Create;
    Try
      oTranslator := TWPDocumentTranslator.Create;
      Try
        oTranslator.WorkingDocument := oWorking.Link;
        oTranslator.WorkingStyles := GetHostConfiguredStyles.Clone;
        oTranslator.Document := oDocument.Link;
        oTranslator.TranslateToWorking;
        SetHostDocument(oWorking, oTranslator.WorkingStyles);
      Finally
        oTranslator.Free;
      End;
    Finally
      oWorking.Free;
    End;
  Finally
    oDocument.Free;
  End;
End;


Procedure TWPDocumentHandler.LoadSnapshot(Const sFilename : String);
Var
  oFile : TFslFile;
Begin
  oFile := TFslFile.Create(sFilename, fmOpenRead + fmShareDenyWrite);
  Try
    LoadSnapshot(oFile);
  Finally
    oFile.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadSnapshot(oStream : TFslStream);
Var
  oReader : TWPSnapshotReader;
  oDocument : TWPWorkingDocument;
Begin
  oDocument := TWPWorkingDocument.Create;
  oReader := TWPSnapshotReader.Create;
  Try
    oReader.Stream := oStream.Link;
    oReader.Styles := GetHostConfiguredStyles.Clone;
    oReader.OnLoadImage := FOnLoadImage;
    oReader.Read(oDocument);
    SetHostDocument(oDocument, oReader.Styles);
  Finally
    oReader.Free;
    oDocument.Free;
  End;
End;

Procedure TWPDocumentHandler.LoadSnapshot(oBuffer: TFslBuffer);
Var
  oStream : TFslMemoryStream;
Begin
  oStream := TFslMemoryStream.Create;
  Try
    oStream.Buffer := oBuffer.Link;
    LoadSnapshot(oStream);
  Finally
    oStream.Free;
  End;
End;



Procedure TWPDocumentHandler.SetSettings(oSettings: TWPSettings);
Begin
  FSettings.Free;
  FSettings := oSettings;
End;

Destructor TWPDocumentHandler.Destroy;
Begin
  FSettings.Free;
  Inherited;
End;

function TWPDocumentHandler.GetAsLogicalText: String;
Var
  oBuilder : TFslStringBuilder;
  iLoop : integer;
  oDocument : TWPWorkingDocument;
Begin
  oBuilder := TFslStringBuilder.Create;
  Try
    oDocument := GetHostDocument;
    for iLoop := 0 to oDocument.Pieces.Count - 1 Do
      oBuilder.Append(oDocument.Pieces[iLoop].LogicalText);
    result := oBuilder.ToString;
  Finally
    oBuilder.Free;
  End;
end;

procedure TWPDocumentHandler.loadByFormat(const sFilename: String; aFormat: TWPFormat);
begin
  Case aFormat Of
    wpfHTML: LoadHTML(sFilename);
    wpfText: LoadText(sFilename);
    wpfRtf: LoadRTF(sFilename);
//    wpfMHT : Load...
    wpfNative: LoadNative(sFilename);
    wpfODT: LoadODT(sFilename);
    wpfCDA: LoadCDA(sFileName);      // Temporarily load CDA if extension is .cda
    wpfSnapshot : LoadSnapshot(sFilename);
  Else // wpfUnknown
    raiseError('LoadByExtension', 'Unspecified file format for '+sFilename);
  End;
end;

procedure TWPDocumentHandler.SaveByFormat(Const sFilename : String; aFormat : TWPFormat);
begin
  Case aFormat Of
    wpfHTML: SaveHTML(sFilename);
    wpfText: SaveText(sFilename);
    wpfRTF: SaveRTF(sFilename);
    wpfNative: SaveNative(sFilename);
    wpfMHT: SaveMHT(sFilename);
    wpfODT: SaveODT(sFilename);
    wpfCDA: SaveCDA(sFileName);      // Temporarily save CDA if extension is .cda
  Else // wpfUnknown
    raiseError('SaveByExtension', 'Unspecified file format');
  End;
end;

procedure TWPDocumentHandler.SaveByFormat(buffer : TFslBuffer; aFormat : TWPFormat; title : string);
begin
  Case aFormat Of
    wpfHTML: SaveHTML(buffer, title);
    wpfText: SaveText(buffer);
    wpfRTF: SaveRTF(buffer, title);
    wpfNative: SaveNative(buffer, true);
    wpfMHT: SaveMHT(buffer, title);
    wpfODT: SaveODT(buffer.Create, title);
    wpfCDA: SaveCDA(buffer, title);      // Temporarily save CDA if extension is .cda
  Else // wpfUnknown
    raiseError('SaveByExtension', 'Unspecified file format');
  End;

end;


function TWPDocumentHandler.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSettings.sizeInBytes);
end;

End.
