unit cda_documents;

{
Copyright (c) 2011+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)

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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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

{$I fhir.inc}

Interface

uses
  SysUtils, Classes,
  fsl_base, fsl_utilities, fsl_fpc, fsl_collections, fsl_stream, fsl_xml,
  cda_base, cda_types, cda_objects, cda_parser, cda_writer;

type
  TIdentity = record
    Space : String;
    Value : String;
  End;


Type
  TCDASourceFormat = (csfPlain, csfXDM);

  TCDAAttachment = class (TFslObject)
  private
    FName : String;
    FBuffer : TFslBuffer;
    FDateTime: TDateTime;
    function GetText: String;
    procedure SetText(const Value: String);
    procedure SetBuffer(const Value: TFslBuffer);
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    Property Buffer : TFslBuffer read FBuffer write SetBuffer;

    {
      The name of the file in the attachment. May include a full path such as \IHE_XDM\SET01\signature.xml.

      Note that relative references from the CDA document use the CDA path name
    }
    Property Name : String read FName write FName;

    {
      The contents of the attachment as text. This is only safe to use if the contents are text, not binary.
      (i.e. xml etc)
    }
    Property AsText : String read GetText write SetText;

    {
      Timestamp on the file, or 0 if none exists
    }
    Property DateTime : TDateTime read FDateTime write FDateTime;

    {
      fill the contents of the attachment from a file
    }
    Procedure LoadFromFile(filename : String);

    {
      save the contents of the attachment to a file
    }
    Procedure SaveToFile(filename : String);
  End;

  {
    A list of CDA Attachments
  }
  TCDAAttachmentList = class (TFslObjectList)
  private
    Function GetAttachments(iIndex : Integer) : TcdaAttachment;
    procedure SetAttachments(iIndex: Integer; const Value: TcdaAttachment);
  protected
    Function ItemClass : TFslObjectClass; Override;
  public
    Procedure InsertObject(iIndex : Integer; oObject : TcdaAttachment);
    Function Link : TcdaAttachmentList; Overload;
    Function Clone : TcdaAttachmentList; Overload;
    {
      Add a Attachment to the end of the list.
    }
    function Append : TcdaAttachment;
    {
      Add an already existing Attachment to the end of the list.
    }
    Procedure AddItem(value : TcdaAttachment);
    {
      See if an item is already in the list. returns -1 if not in the list
    }
    Function IndexOf(value : TcdaAttachment) : Integer;
    {
       Insert Attachment before the designated index (0 = first item)
    }
    function Insert(iIndex : Integer) : TcdaAttachment;
    {
       Insert an existing Attachment before the designated index (0 = first item)
    }
    Procedure InsertItem(iIndex : Integer; value : TcdaAttachment);
    {
       Get the iIndexth Attachment. (0 = first item)
    }
    Function Item(iIndex : Integer) : TcdaAttachment;
    {
       Set the iIndexth Attachment. (0 = first item)
    }
    Procedure SetItemByIndex(iIndex : Integer; value : TcdaAttachment);
    {
      The number of items in the collection
    }
    Function Count : Integer; Overload;
    {
      Remove the iIndexth item. The first item is index 0.
    }
    Procedure Remove(iIndex : Integer);
    {
      Remove All Items from the list
    }
    Procedure ClearItems;
    Property Attachments[iIndex : Integer] : TcdaAttachment read GetAttachments write SetAttachments; default;
  End;


  {
    Base CDA Document
  }
  TCDADocument = class (TFslObject)
  Private
    FSource : TBytes;
    FDom : TMXmlDocument;
    FRoot : TcdaClinicalDocument;
    FAttachments : TcdaAttachmentList;
    FSourceFormat : TCDASourceFormat;
    FCDAName : String;
    FIdentifiedObjects : TFslStringObjectMatch;

    procedure MineCDForSnomedCodes(oCD: Tv3CD; oCodes: TFslStringList);
    Procedure MineForSnomedCodes(oItem : Tv3Base; oCodes : TFslStringList);

    Function GetCode : TIdentity; overload;
    Function GetCode(oCd : Tv3CD): TIdentity;overload;
    Function GetId : TIdentity;
    Function GetMRN : TIdentity;
    procedure doParse(buffer : TFslBuffer);
    procedure SetRoot(const Value: TcdaClinicalDocument);
    Procedure Decode(Const sSource : TBytes); Overload;
    Procedure Decode(Const sSource : TStream); Overload;
    function GetCDAPatientID(const sOid: String): TIdentity;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create(Const sSource : TBytes); Overload;
    constructor Create(Const sSource : TStream); Overload;
    destructor Destroy; Override;
    function Link : TCDADocument; Overload;
    Function doXPath(Const Expression : String) : String; Overload;
    Function doXPath(oRoot : Tv3Base; Const Expression : String) : String; Overload;
    Procedure doXPath(Const Expression : String; oNodes : Tv3BaseList); Overload;
    Procedure doXPath(oRoot : Tv3Base; Const Expression : String; oNodes : Tv3BaseList); Overload;
    Function SnomedQuery(Const XPathExpression : String; oCodes : TFslStringList) : String; Overload;
    Property Dom : TMXmlDocument Read FDom;
    Property Source : TBytes read FSource;
    Property Code : TIdentity read GetCode;
    Property Id : TIdentity read GetId;
    Property MRN : TIdentity read GetMRN;
    Property IdentifiedObjects : TFslStringObjectMatch read FIdentifiedObjects;

    function title : string;
    function extractDocumentId(buffer : TFslBuffer) : String;
    function getPatientIHI : string;
    function patientName : string;
    function authorName : string;
    function authorTelecomSummary : String;
    function patientGender : String;
    function patientDOB : String;
    function getPatientMRN : String;
    function RenderPatientIdentifiers : String;

   {
      Select a set of objects by an XPath expression.

      The first parameter is the root for the XPath expression. To
      start at the root of the document, pass the CDA property, or nil.

      The second parameter is an XPath statement such as //v3:recordTarget/v3:patientRole/v3:id
      The namespace 'v3' is required on elements or the xpath won't work. Other namespaces that can be used
      in the XPath are xsi and the xml namespace itself.

      The return value is a list of the objects that match the selected XPath statement.
      The objects may be any of the Cda, structured text or data types. Only objects
      that have an element can be selected. Although you can execute xpath statements
      that match text, attributes, comments etc, these will never be able to be matched
      to any matching object, and nothing will be returned.

      For instance, executing the xpath above with the CDA document itself as the root
      will return the list of Tv3II identifiers for the patient.

      Note that the XPath statements operate on the original parsed XML, and the XML is not
      updated as the object model itself is changed. So if you add an identifier to the
      list of patient identifiers in a script, then execute the XPath above, the new
      identifier will not be returned. Further, deleting an identifier from the list,
      then rerunning the XPath statement, and trying to access the deleted object will
      cause unpredictable results (technically, the link from the XML back to the object
      model is not reference counted)

      Technical Note: The XPath expression is evaluated by the MSXML engine.
    }
    Function XPath(oRoot : TV3Base; sExpression : String) : Tv3BaseList;

    {
      generate XML for the document - intended to be used for debugging purposes
    }
    Function Encode(pretty : boolean = false) : TBytes;
  published
    {
      the root object of the CDA document
    }
    Property Root : TcdaClinicalDocument read FRoot write SetRoot;

    {
      Any attachments with names (including paths) for anything that is associated with the incoming CDA document (i.e. XDM package)
    }
    Property Attachments : TcdaAttachmentList read FAttachments;

    {
      The absolute name of the CDA document
    }
    Property CDAName : String read FCDAName write FCDAName;

    {
      the format of the source - plain CDA, or XDM
    }
    Property SourceFormat : TCDASourceFormat read FSourceFormat;

  End;

Const
  NAMES_TCDASourceFormat : array [TCDASourceFormat] of string = ('Plain', 'XDM');

Function SrcIsZip(s : Tbytes):Boolean;

Function ParseCDA(Const source : TStream; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
Function ParseCDA(Const source : TFslStream; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
Function ParseCDA(Const sSource : TBytes; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
Function ParseCDA(Const sSource : String; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
Function WriteCDA(Const oDoc : TcdaClinicalDocument; bIsPretty : Boolean = True) : TBytes;

implementation

Function SrcIsZip(s : TBytes):Boolean;
begin
  result := BytePos(AnsiStringAsBytes('PK'#03#04), s) = 0;
end;

{ TCDADocument }


constructor TCDADocument.Create(const sSource: TBytes);
begin
  Inherited Create;
  FAttachments := TcdaAttachmentList.create;
  Decode(sSource);
end;

procedure TCDADocument.Decode(const sSource: TBytes);
var
  buffer : TFslBuffer;
  zip : TFslZipPartList;
  reader : TFslZipReader;
  ss : TFslStringStream;
  i : integer;
  att : TCDAAttachment;
Begin
  if SrcIsZip(sSource) then
  begin
    FSourceFormat := csfXDM;
    ss := TFslStringStream.Create;
    try
      ss.Bytes := sSource;
      zip := TFslZipPartList.create;
      reader := TFslZipReader.Create;
      try
        reader.Stream := ss.Link;
        reader.Parts := zip.Link;
        reader.ReadZip;
        for i := 0 to zip.Count - 1 do
        begin
          if StringEndsWithInsensitive(zip[i].Name, 'CDA_ROOT.XML') then
          begin
            FCDAName := zip[i].Name;
            FSource := zip[i].AsBytes;
            doParse(zip[i]);
          end
          else if not StringEndsWith(zip[i].Name, '/') or (zip[i].Size > 0) then
          begin
            att := TCDAAttachment.Create;
            try
              att.Name := zip[i].Name;
              att.Buffer := zip[i].link;
              att.DateTime := zip[i].Timestamp;
              FAttachments.Add(att.link);
            finally
              att.Free;
            end;
          end;
        end;

      finally
        reader.free;
      end;
    finally
      ss.free;
    end;
  end
  else
  begin
    FSource := sSource;
    FSourceFormat := csfPlain;
    buffer :=  TFslBuffer.Create;
    try
      buffer.AsBytes := sSource;
      doParse(buffer);
    finally
      buffer.Free;
    end;
  End;
end;

procedure TCDADocument.doParse(buffer : TFslBuffer);
var
  oCda: TCDAParser;
  oXml : TMXmlParser;
begin
  oXml := TMXmlParser.Create;
  Try
    oCda := TCDAParser.Create;
    try
      oCda.Errors := false;
      oCda.StripWhitespace := true;
      oCda.Mapping := True;
      FDom := oXml.Parse(buffer.AsBytes, [xpResolveNamespaces]);
//      FDom.setProperty('SelectionNamespaces', 'xmlns:v3=''urn:hl7-org:v3''');
//      FDom.setProperty('SelectionLanguage', 'XPath');
      FIdentifiedObjects := oCda.IdentifiedObjects.Link;
      FRoot := oCda.Parse(FDom);
    finally
      oCda.Free;
    end;
  Finally
    oXml.Free;
  End;
end;


destructor TCDADocument.Destroy;
begin
  FAttachments.Free;
  FIdentifiedObjects.Free;
  FSource := Bytes([]);
  FDom := nil;
  FRoot.Free;
  FRoot := nil;
  inherited;
end;


function TCDADocument.Encode(pretty : boolean = false): TBytes;
var
  ss : TFslStringStream;
  zip : TFslZipPartList;
  writer : TFslZipWriter;
  i : integer;
begin
  if FSourceFormat = csfXDM then
  begin
    zip := TFslZipPartList.create;
    try
      zip.Add(FCDAName, WriteCDA(FRoot, pretty));
      for i := 0 to FAttachments.Count - 1 Do
        zip.Add(FAttachments[i].Name, FAttachments[i].Buffer.AsBytes);
      writer := TFslZipWriter.create;
      try
        writer.Parts := zip.Link;
        ss := TFslStringStream.Create;
        try
          writer.Stream := ss.Link;
          writer.WriteZip;
          result := ss.Bytes;
        finally
          ss.free;
        end;
      finally
        writer.free;
      end;
    finally
      zip.free;
    end;
  end
  else
    Result := WriteCDA(FRoot, pretty);
end;

Function TCDADocument.GetCode(oCd : Tv3CD): TIdentity;
Begin
  if oCd = nil Then
  Begin
    result.Space := '';
    result.Value := '';
  End
  Else
  Begin
    result.Space := oCd.codeSystem;
    result.Value := oCd.code;
  End;
End;

function TCDADocument.GetCode: TIdentity;
begin
  result := GetCode(FRoot.code);
end;


function TCDADocument.GetId: TIdentity;
var
  oId : Tv3II;
Begin
  oId := FRoot.id;
  if oId = nil Then
  begin
    Result.Space := '';
    Result.Value := '';
  End
  Else if oId.extension <> '' Then
  Begin
    result.Space:= oId.root;
    result.Value := oId.extension;
  End
  Else
  Begin
    result.Space := '';
    result.Value := oId.root;
  End;
end;

Function TCDADocument.GetCDAPatientID(Const sOid : String) : TIdentity;
Var
  iRec : Integer;
  iId : Integer;
Begin
  Result.Space  := '';
  Result.Value  := '';
  if sOid <> '' Then
    For iRec := 0 to FRoot.recordTarget.Count - 1 do
      if (FRoot.recordTarget[iRec].patientRole <> nil) Then
        For iId := 0 to FRoot.recordTarget[iRec].patientRole.id.Count - 1 Do
          if FRoot.recordTarget[iRec].patientRole.id[iId].root = sOid Then
          Begin
            If Result.Value <> '' Then
            Begin
              result.Space := '';
              result.Value := '';
              exit;
            End;
            Result.Space := FRoot.recordTarget[iRec].patientRole.id[iId].root;
            Result.Value := FRoot.recordTarget[iRec].patientRole.id[iId].extension;
          End;
  if (result.Value = '') And (FRoot.recordTarget <> nil) And (FRoot.recordTarget.Count = 1) And (FRoot.recordTarget[0].patientRole <> nil)
         And (FRoot.recordTarget[0].patientRole.id <> nil) And (FRoot.recordTarget[0].patientRole.id.Count = 1) Then
    if FRoot.recordTarget[0].patientRole.id[0].extension <> '' Then
    Begin
      Result.Space := FRoot.recordTarget[0].patientRole.id[0].root;
      Result.Value := FRoot.recordTarget[0].patientRole.id[0].extension
    End
    else
    Begin
      Result.Space := '';
      Result.Value := FRoot.recordTarget[0].patientRole.id[0].root;
    End;
End;

function TCDADocument.GetMRN: TIdentity;
begin
  result := GetCDAPatientID('');
end;

function TCDADocument.getPatientIHI: string;
var
  nodes : Tv3BaseList;
begin
  nodes := XPath(FRoot, '/ClinicalDocument/recordTarget/patientRole/patient/ext:asEntityIdentifier[@classCode=''IDENT'']/ext:id[@assigningAuthorityName=''IHI'']');
  try
    if nodes.Count = 0 then
      result := ''
    else if nodes.Count = 1 then
      result := Tv3II(nodes[0]).root
    else
      result := '??multiple matches';
  finally
    nodes.Free;
  end;
end;

function TCDADocument.getPatientMRN: String;
var
  i: Integer;
  nodes : Tv3BaseList;
begin
  nodes := XPath(FRoot, '/ClinicalDocument/recordTarget/patientRole/patient/ext:asEntityIdentifier[@classCode=''IDENT'']/ext:id[@assigningAuthorityName!=''IHI'']');
  try
    if nodes.Count = 0 then
      result := ''
    else if nodes.Count = 1 then
      if Tv3II(nodes[0]).extension <> '' then
        result := Tv3II(nodes[0]).extension
      else
        result := Tv3II(nodes[0]).root
    else for i := 0 to nodes.Count - 1 do
    begin
      result := '';
      if i > 0 then
        result := result + ', ';
      if Tv3II(nodes[i]).extension <> '' then
        result := result + Tv3II(nodes[i]).extension
      else
        result := result + Tv3II(nodes[i]).root;
    end;
  finally
    nodes.Free;
  end;
end;


//Function NodesToText(oNodes : IXMLDOMNodeList): String;
//Var
//  iLoop : integer;
//Begin
//  Result := '';
//  For iLoop := 0 to oNodes.length - 1 Do
//    Result := result + oNodes.item[iLoop].Text;
//End;
//
function TCDADocument.doXPath(const Expression: String): String;
Begin
  result := doXPath(FRoot, Expression);
End;

function TCDADocument.doXPath(oRoot : Tv3Base; const Expression: String): String;
//var
//  n : IXMLDOMNodeList;
begin
//  n := FDom.selectNodes(Expression);
//  Result := NodesToText(n);
end;

procedure TCDADocument.doXPath(const Expression: String; oNodes: Tv3BaseList);
Begin
//  doXPath(FRoot, Expression, oNodes);
End;

procedure TCDADocument.SetRoot(const Value: TcdaClinicalDocument);
begin
  FRoot.Free;
  FRoot := Value;
end;

Function TCDADocument.SnomedQuery(Const XPathExpression : String; oCodes : TFslStringList) : String;
var
  oList : Tv3BaseList;
  iLoop : integer;
begin
  oList := Tv3BaseList.Create(nil);
  Try
    doXpath(XPathExpression, oList);
    For iLoop := 0 to oList.Count -1 Do
      MineForSnomedCodes(oList[iLoop], oCodes);
  Finally
    oList.Free;
  End;
End;

function TCDADocument.authorTelecomSummary: String;
var
  j, i: Integer;
  tel : Tv3TEL;
  auth : TcdaAssignedAuthor;
begin
  result := '';
  for j := 0 to FRoot.author.count - 1 do
  begin
    auth := FRoot.author[j].assignedAuthor;
    if auth.assignedPerson <> nil then
    begin
      for i := 0 to auth.telecom.Count - 1 do
      begin
        tel := auth.telecom[i];
        result := result + ' / '+tel.render;
      end;
    end;
  end;
  result := copy(result, 4, $FFFF);
end;

function TCDADocument.title: string;
begin
  if assigned(FRoot.title) and (FRoot.title.value <> '') then
    result := FRoot.title.value
  else if assigned(FRoot.code) then
    result := FRoot.code.render
//  else if assigned(FRoot.templateId) then
//  begin
//
//  end
  else
    result := '??';
end;

procedure TCDADocument.MineCDForSnomedCodes(oCD: Tv3CD; oCodes: TFslStringList);
var
  iLoop : integer;
Begin
  if oCD.nullFlavor = nfNull Then
  Begin
    if oCD.codeSystem = OID_SNOMED Then
      oCodes.Add(oCD.code);
    if oCd.translation <> nil Then
      For iLoop := 0 to oCd.translation.Count - 1 Do
        MineCDForSnomedCodes(oCD.translation[iLoop], oCodes);
  End;
End;

procedure TCDADocument.MineForSnomedCodes(oItem: Tv3Base; oCodes: TFslStringList);
var
  oIter : Tv3DataTypePropertyIterator;
begin
  if oItem is Tv3CD Then
    MineCDForSnomedCodes(Tv3CD(oItem), oCodes)
  Else if oItem is TcdaBase Then
  Begin
    oIter := TcdaBase(oItem).createIterator(true);
    Try
      oIter.Reset;
      while oIter.More Do
      Begin
        MineForSnomedCodes(Tv3Base(oIter.Current.AsType(Tv3Base)), oCodes);
        oIter.Next;
      End;
    Finally
      oIter.Free;
    End;
  End;
end;

function TCDADocument.patientDOB: String;
var
  ts : Tv3TS;
begin
  ts := FRoot.recordTarget[0].patientRole.patient.birthTime;
  result := ts.render;
end;

function TCDADocument.patientGender: String;
var
  cd : Tv3CD;
begin
  cd := FRoot.recordTarget[0].patientRole.patient.administrativeGenderCode;
  result := cd.render;
end;

function TCDADocument.patientName: string;
var
  j: Integer;
  en : Tv3EN;
begin
  result := '';
  for j := 0 to FRoot.recordTarget[0].patientRole.patient.name.Count - 1 do
  begin
    en := FRoot.recordTarget[0].patientRole.patient.name[j];
    if (result = '') or (nuC in en.use) or (nuL in en.use) then
      result := en.render;
  end;
end;

function TCDADocument.authorName: string;
var
  j, i: Integer;
  en : Tv3EN;
  auth : TcdaAssignedAuthor;
begin
  result := '';
  for j := 0 to FRoot.author.count - 1 do
  begin
    auth := FRoot.author[j].assignedAuthor;
    if auth.assignedPerson <> nil then
    begin
      for i := 0 to auth.assignedPerson.name.Count - 1 do
      begin
        en := auth.assignedPerson.name[i];
        if (result = '') or (nuC in en.use) or (nuL in en.use) then
          result := en.render;
      end;
    end;
  end;
end;

function TCDADocument.RenderPatientIdentifiers: String;
var
  s : TFslStringBuilder;
  i, j : integer;
  rt : TcdaRecordTarget;
  ei : TcdaEntityIdentifier;
begin
  s := TFslStringBuilder.Create;
  try
    for i := 0 to FRoot.recordTarget.Count - 1 do
    begin
      rt := FRoot.recordTarget[i];
      for j := 0 to rt.patientRole.patient.asEntityIdentifier.Count - 1 do
      begin
        ei := rt.patientRole.patient.asEntityIdentifier[j];
        s.Append(', ');
        if (ei.id.identifierName = '') and (ei.code <> nil) then
          s.Append(ei.code.render+': ');
        s.Append(ei.id.render);
      end;
      for j := 0 to rt.patientRole.id.Count - 1 do
      begin
        s.Append(', ');
        s.Append(rt.patientRole.id[j].render);
      end;
    end;
    result := copy(s.toString(), 3, $FFFF);
  finally
    s.Free;
  end;
end;

Procedure TCDADocument.doXPath(oRoot : Tv3Base; Const Expression : String; oNodes : Tv3BaseList);
//var
//  oXmlNodes : IXMLDOMNodeList;
//  iLoop : integer;
//  oNode : Tv3Base;
//  sVal : String;
begin
//  oNodes.Clear;
//  if (oRoot.Element <> nil) Then
//  Begin
//    oXmlNodes := FRoot.Element.selectNodes(Expression);
//    for iLoop := 0 to oXmlNodes.length - 1 Do
//      if oXmlNodes.item[iLoop].nodeType = NODE_ELEMENT Then
//      Begin
//        sVal := (oXmlNodes.item[iLoop] as IXMLDomElement).getAttribute(MAGIC_ATTRIBUTE_NAME);
//        oNode := Tv3Base(StrToIntDef('$'+sVal, 0));
//        if oNode <> Nil Then
//          oNodes.Add(oNode.Link);
//      End;
//  End;
end;

function TCDADocument.XPath(oRoot: TV3Base; sExpression: String): Tv3BaseList;
begin
  result := Tv3BaseList.create(nil);
  try
    doXpath(oRoot, sExpression, result);
    result.Link;
  finally
    result.free;
  end;
end;

constructor TCDADocument.Create(const sSource: TStream);
begin
  Inherited Create;
  FAttachments := TcdaAttachmentList.create;
  Decode(sSource);
end;

procedure TCDADocument.Decode(const sSource: TStream);
var
  b : TBytes;
begin
  SetLength(b, sSource.Size - sSource.position);
  if Length(b) > 0 then
    sSource.Read(b[0], length(b));
  decode(b);
end;


function TCDADocument.Link: TCDADocument;
begin
  result := TCDADocument(Inherited Link);
end;

function TCDADocument.extractDocumentId(buffer: TFslBuffer): String;
var
  s : String;
  i, j : integer;
begin
  result := '?unable to determine cda id '+copy(buffer.asText, 1, 40);
  s := buffer.AsText;
  i := pos('ClinicalDocument', s);
  if (i > 0) then
  begin
    while (i < length(s) - 3) and (copy(s, i, 3) <> 'id ') do
      inc(i);
    if (i < length(s)-3) then
    begin
      j := i;
      while (j < length(s)) and (s[j] <> '>') do
        inc(j);
      if (j < length(s)) then
        result := 'CDA.id: '+s.Substring(i+3, j-1);
    end;
  end;
end;

function TCDADocument.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDom.sizeInBytes);
  inc(result, FRoot.sizeInBytes);
  inc(result, FAttachments.sizeInBytes);
  inc(result, (FCDAName.length * sizeof(char)) + 12);
  inc(result, FIdentifiedObjects.sizeInBytes);
end;

{ TCDAAttachment }

constructor TCDAAttachment.Create;
begin
  inherited;
  FBuffer := TFslBuffer.Create;
end;

destructor TCDAAttachment.Destroy;
begin
  FBuffer.Free;
  inherited;
end;

function TCDAAttachment.GetText: String;
begin
  result := FBuffer.AsText;
end;

procedure TCDAAttachment.LoadFromFile(filename: String);
begin
  FBuffer.LoadFromFileName(filename);
  FDateTime := FileGetModified(filename);
end;

procedure TCDAAttachment.SaveToFile(filename: String);
begin
  FBuffer.SaveToFileName(filename);
  if FDateTime <> 0 then
    FileSetModified(filename, FDateTime);
end;

procedure TCDAAttachment.SetBuffer(const Value: TFslBuffer);
begin
  Fbuffer.free;
  FBuffer := value;
end;

procedure TCDAAttachment.SetText(const Value: String);
begin
  FBuffer.AsText := value;
end;

function TCDAAttachment.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, FBuffer.sizeInBytes);
end;

{ TcdaAttachmentList }

Function TcdaAttachmentList.Link : TcdaAttachmentList;
Begin
  Result := TcdaAttachmentList(Inherited Link);
End;

Function TcdaAttachmentList.Clone : TcdaAttachmentList;
Begin
  result := TcdaAttachmentList(Inherited Clone);
End;

Function TcdaAttachmentList.GetAttachments(iIndex : Integer) : TcdaAttachment;
Begin
  Result := TcdaAttachment(ObjectByIndex[iIndex]);
End;

Function TcdaAttachmentList.Append : TcdaAttachment;
Begin
  Result := TcdaAttachment.Create;
  Try
    Add(result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAttachmentList.AddItem(value : TcdaAttachment);
Begin
  Add(value.Link);
End;

Function TcdaAttachmentList.IndexOf(value : TcdaAttachment) : Integer;
Begin
  result := IndexByReference(value);
End;

Function TcdaAttachmentList.Insert(iIndex : Integer) : TcdaAttachment;
Begin
  Result := TcdaAttachment.Create;
  Try
    Inherited Insert(iIndex, Result.Link);
  Finally
    Result.Free;
  End;
End;

Procedure TcdaAttachmentList.InsertItem(iIndex : Integer; value : TcdaAttachment);
begin
  Inherited Insert(iIndex, value);
End;

Function TcdaAttachmentList.Item(iIndex : Integer) : TcdaAttachment;
Begin
  Result := Attachments[iIndex];
End;

Procedure TcdaAttachmentList.SetItemByIndex(iIndex : Integer; value: TcdaAttachment);
Begin
  Attachments[iIndex] := value;
End;

Procedure TcdaAttachmentList.Remove(iIndex : Integer);
Begin
  DeleteByIndex(iIndex);
End;

Procedure TcdaAttachmentList.ClearItems;
Begin
  Clear;
End;

function TcdaAttachmentList.Count: Integer;
begin
  result := Inherited Count;
end;

Procedure TcdaAttachmentList.InsertObject(iIndex: Integer; oObject : TcdaAttachment);
begin
  Inherited Insert(iIndex, oObject);
end;

procedure TcdaAttachmentList.SetAttachments(iIndex: Integer; const Value: TcdaAttachment);
begin
  ObjectByIndex[iIndex] := value;
end;

function TcdaAttachmentList.ItemClass: TFslObjectClass;
begin
  Result := TcdaAttachment;
end;

Function ParseCDA(Const sSource : String; bErrors : Boolean = False) : TcdaClinicalDocument;
begin
  result := ParseCDA(StringAsBytes(sSource), bErrors);
end;

Function ParseCDA(Const sSource : Tbytes; bErrors : Boolean) : TcdaClinicalDocument;
var
  oStream : TBytesStream;
  oXml : TMXmlParser;
  oCda : TCDAParser;
  doc : TMXmlDocument;
Begin
  oStream := TBytesStream.Create(sSource);
  Try
    oXml := TMXmlParser.Create;
    Try
      oCda := TCDAParser.Create;
      Try
        oCda.Errors := bErrors;
        oCda.StripWhitespace := true;
        doc := oXml.Parse(oStream, [xpResolveNamespaces]);
        try
          result := oCda.Parse(doc);
        finally
          doc.Free;
        end;
      Finally
        oCda.Free;
      End;
    Finally
      oXml.Free;
    End;
  Finally
    oStream.Free;
  End;
End;

Function ParseCDA(Const source : TStream; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
var
  oXml : TMXmlParser;
  oCda : TCDAParser;
  doc : TMXmlDocument;
Begin
  oXml := TMXmlParser.Create;
  Try
    oCda := TCDAParser.Create;
    Try
      oCda.Errors := bErrors;
      oCda.StripWhitespace := true;
      doc := oXml.Parse(source, [xpResolveNamespaces]);
      try
        result := oCda.Parse(doc);
      finally
        doc.Free;
      end;
    Finally
      oCda.Free;
    End;
  Finally
    oXml.Free;
  End;
End;

Function ParseCDA(Const source : TFslStream; bErrors : Boolean = False) : TcdaClinicalDocument; Overload;
var
  oXml : TMXmlParser;
  oCda : TCDAParser;
  doc : TMXmlDocument;
Begin
  oXml := TMXmlParser.Create;
  Try
    oCda := TCDAParser.Create;
    Try
      oCda.Errors := bErrors;
      oCda.StripWhitespace := true;
      doc := oXml.Parse(source, [xpResolveNamespaces]);
      try
        result := oCda.Parse(doc);
      finally
        doc.Free;
      end;
    Finally
      oCda.Free;
    End;
  Finally
    oXml.Free;
  End;
End;


Function WriteCDA(Const oDoc : TcdaClinicalDocument; bIsPretty : Boolean) : TBytes;
var
  oStream : TBytesStream;
  oXml : TXmlBuilder;
  oCda : TCDAWriter;
Begin
  oStream := TBytesStream.Create;
  Try
    oXml := TFslXmlBuilder.Create;
    Try
      oXml.IsPretty := bIsPretty;
      oXml.Start;
      oCda := TCDAWriter.Create;
      Try
        oCda.Errors := False;
        oCda.WriteCDA(oXml, oDoc);
      Finally
        oCda.Free;
      End;
      oXml.Finish;
      oXml.Build(oStream);
    Finally
      oXml.Free;
    End;
    result := oStream.Bytes;
  Finally
    oStream.Free;
  End;
End;


End.
