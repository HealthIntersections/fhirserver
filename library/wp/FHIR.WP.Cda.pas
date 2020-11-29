Unit FHIR.WP.Cda;

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
  SysUtils,
  fsl_stream, fsl_xml, fsl_utilities,
  wp_types, wp_document, wp_working, wp_format;

Const
  TAG_CDA_ROOT = 'ClinicalDocument';
  TAG_CDA_TYPEID = 'typeId';
  TAG_CDA_TEMPLATEID = 'templateId';
  TAG_CDA_ID = 'id';
  TAG_CDA_CODE = 'code';
  TAG_CDA_TITLE = 'title';
  TAG_CDA_EFFECTIVETIME = 'effectiveTime';
  TAG_CDA_CONFIDENTIALITY_CODE = 'confidentialityCode';
  TAG_CDA_LANGUAGE_CODE = 'languageCode';
  TAG_CDA_SET_ID = 'setId';
  TAG_CDA_VERSION_NUMBER = 'versionNumber';
  TAG_CDA_RECORD_TARGET = 'recordTarget';
  TAG_CDA_AUTHOR = 'author';
  TAG_CDA_LEGAL_AUTHENTICATOR = 'legalAuthenticator';
  TAG_CDA_RELATED_DOCUMENT = 'relatedDocument';
  TAG_CDA_COMPONENT_OF = 'componentOf';
  TAG_CDA_COMPONENT = 'component';

  TAG_CDA_STRUCTURED_BODY = 'structuredBody';
  TAG_CDA_TEXT = 'text';

Type

  TWPCdaReader = Class (TWPReader)
    Private
      FDoc : TMXmlDocument;

    Protected
      Procedure ReadCDA(oDocument : TWPWorkingDocument); Overload;
      Procedure ReadCDAHeaderElement(oDocument: TWPWorkingDocument; oElement : TMXmlElement); Overload;
      Procedure ReadCDABody(oDocument : TWPWorkingDocument; oElement : TMXmlElement); Overload;
      Procedure ReadStructureBody(oDocument : TWPWorkingDocument; oElement : TMXmlElement); Overload;

      Procedure AddText(oDocument: TWPWorkingDocument; Const sContent : String); Overload;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(oDocument : TWPWorkingDocument); Overload; Override;
  End;



Type
  TWPCdaWriter = Class (TWPWriter)
    Private
      FDoc : TMXmlDocument;

      FCurrent : TMXmlElement;     // the current element

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;

      Procedure WriteCDAHeader(oDocument : TWPWorkingDocument); Overload; Virtual;

      Procedure WriteDocumentStart(oDocument : TWPWorkingDocument); Override;
      Procedure WriteParagraphStart(oParagraph : TWPWorkingDocumentParaPiece); Override;
      Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Override;
      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Write(oDocument : TWPWorkingDocument); Override;
  End;

Implementation



{ TWPCdaReader }

Constructor TWPCdaReader.Create;
Begin
  Inherited;
End;

Destructor TWPCdaReader.Destroy;
Begin
  Inherited;
End;


Procedure TWPCdaReader.Read(oDocument: TWPWorkingDocument);
var
  options : TMXmlParserOptions;
Begin
  Inherited;
  options := [xpResolveNamespaces, xpDropComments];

  FDoc := TMXmlParser.parse(Stream, options);
  Try
    ReadCDA(oDocument);
  Finally
    FDoc.Free;
  End;

  CheckForEmpty(oDocument);
  DoneReading(oDocument);
End;


Procedure TWPCdaReader.ReadCDA(oDocument: TWPWorkingDocument);
Var
  iLoop : Integer;
  oElement : TMXmlElement;
Begin
  CheckCondition(StringEquals(FDoc.docElement.Name, TAG_CDA_ROOT) And StringEquals(FDoc.docElement.NamespaceURI, NAMESPACE_HL7V3), 'ReadDOM', 'Not CDA root');

  For oElement in FDoc.docElement.Children Do
  Begin
    CheckCondition(StringEquals(oElement.NamespaceURI, NAMESPACE_HL7V3),
              'ReadDOM', 'Node not in HL7V3 namespace ' + oElement.Name);

    If StringEquals(oElement.Name, TAG_CDA_COMPONENT) Then
      ReadCDABody(oDocument, oElement)
    Else
      ReadCDAHeaderElement(oDocument, oElement);
  End;

End;


Procedure TWPCdaReader.ReadCDAHeaderElement(oDocument: TWPWorkingDocument; oElement : TMXmlElement);
Begin
  // TODO: read header info
End;


Procedure TWPCdaReader.ReadCDABody(oDocument: TWPWorkingDocument; oElement : TMXmlElement);
Begin
  CheckCondition(StringEquals(oElement.Name, TAG_CDA_COMPONENT)
        And StringEquals(oElement.namespaceUri, NAMESPACE_HL7V3), 'ReadCDABody', 'Not CDA body');
  CheckCondition(oElement.Children.Count = 1, 'ReadCDABody', 'CDA body should have 1 structured body only');

  oElement := oElement.Children[0];
  CheckCondition(StringEquals(oElement.Name, TAG_CDA_STRUCTURED_BODY)
        And StringEquals(oElement.namespaceUri, NAMESPACE_HL7V3), 'ReadCDABody', 'Not CDA structured body');
  readStructureBody(oDocument, oElement);
End;


Procedure TWPCdaReader.ReadStructureBody(oDocument: TWPWorkingDocument; oElement : TMXmlElement);
Var
  iLoop: Integer;
  oParagraph : TWPWorkingDocumentParaPiece;
Begin
  Assert(CheckCondition(oElement <> Nil, 'ReadStructureBody', 'Invalid element'));

  // TODO: read non-text element, and attributes

  // extract text and add to document
  If oElement.HasText Then
  Begin
    Splitter.Init(oElement.Text);
    While Splitter.More Do
      AddText(oDocument, Splitter.Next);
  End;

  // recurse to read the children
  For iLoop := 0 To oElement.Children.Count - 1 Do
    ReadStructureBody(oDocument, oElement.Children[iLoop]);

  // terminate as a paragraph
  If StringEquals(oElement.Name, TAG_CDA_COMPONENT) Then
  Begin
    oParagraph := TWPWorkingDocumentParaPiece.Create;
    Try
      //oParagraph.SpeechMagicDouble := SpeechMagicDouble;
      //ReadStyleAttributes(oParagraph);
      //ReadParagraphAttributes(oParagraph.Format);
      oDocument.Pieces.Add(oParagraph.Link);
    Finally
      oParagraph.Free;
    End;
  End;
End;

Procedure TWPCdaReader.AddText(oDocument: TWPWorkingDocument; Const sContent : String);
Var
  oText : TWPWorkingDocumentTextPiece;
Begin
  oText := TWPWorkingDocumentTextPiece.Create;
  Try
    // ReadStyleAttributes(oText);

    oText.Content := sContent;
    oDocument.Pieces.Add(oText.Link);
  Finally
    oText.Free;
  End;
End;

function TWPCdaReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDoc.sizeInBytes);
end;

{ TWPCdaWriter }

Constructor TWPCdaWriter.Create;
Begin
  Inherited;

End;

Destructor TWPCdaWriter.Destroy;
Begin

  Inherited;
End;


Procedure TWPCdaWriter.Initialise;
Begin
  Inherited;
End;

Procedure TWPCdaWriter.Finalise;
Begin
  Inherited;

  FDoc.ToXml(Stream, true);
End;


Procedure TWPCdaWriter.WriteDocumentStart(oDocument: TWPWorkingDocument);
Var
  oRoot : TMXmlElement;
  oBody : TMXmlElement;
Begin
  Inherited;

  // create the root and define namespace
  FDoc := TMXmlDocument.Create(ntDocument);
  oRoot := FDoc.addElementNS(NAMESPACE_HL7V3, TAG_CDA_ROOT);
//  oRoot.setAttribute('xmlns:xsi', NAMESPACE_W3C_XSI);
//  oRoot.setAttribute('xsi:schemaLocation', 'urn:hl7-org:v3 CDA.xsd');

  WriteCDAHeader(oDocument);

  // Start the CDA body
  oBody := oRoot.addElement(TAG_CDA_COMPONENT);
  FCurrent := oBody.addElement(TAG_CDA_STRUCTURED_BODY);
End;


Procedure TWPCdaWriter.Write(oDocument: TWPWorkingDocument);
Begin
  try
    Inherited;
  Finally
    FDoc.Free;
    FDoc := Nil;
  End;
End;


Procedure TWPCdaWriter.WriteCDAHeader(oDocument: TWPWorkingDocument);
Begin
  // TODO: Create and write the CDA header
End;


Procedure TWPCdaWriter.WriteParagraphStart(oParagraph: TWPWorkingDocumentParaPiece);
Begin
  Inherited;

  // we create a component for each paragraph
  FCurrent := FCurrent.addElement(TAG_CDA_COMPONENT);
End;


Procedure TWPCdaWriter.WriteParagraphStop(oParagraph: TWPWorkingDocumentParaPiece;
        bNextIsSection: Boolean; oSection: TWPWorkingDocumentSectionStartPiece);
Begin
  Inherited;

  FCurrent := FCurrent.Parent;
End;


Procedure TWPCdaWriter.WriteText(oText: TWPWorkingDocumentTextPiece);
Var
  oTextNode : TMXmlElement;
Begin
  Inherited;

  oTextNode := FCurrent.addText(oText.Content);
End;

function TWPCdaWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDoc.sizeInBytes);
  inc(result, FCurrent.sizeInBytes);
end;

End.
