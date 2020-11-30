unit v2_publish;

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

interface

uses
  SysUtils, Graphics,
  fsl_base, fsl_utilities, fsl_collections, fsl_stream,
  wp_document, FHIR.WP.Builder, wp_types, wp_format, wp_factory,
  v2_base, v2_dictionary, v2_objects;

Type
  THL7V2DocumentPublisher = class (TWPDocumentBuilder)
    Private
      FTitle: String;
      FTitleStyle : TWPStyle;
      procedure SetTitleStyle(const Value: TWPStyle);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      Constructor Create; Override;
      Destructor Destroy; Override;

      Property TitleStyle : TWPStyle Read FTitleStyle Write SetTitleStyle;

      Property Title : String read FTitle write FTitle;

      Function StartTable(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTable; Override;

      Function AddTitle(sText : String) : TWPDocumentParagraph; Overload; Virtual;
      Function StartRowFlip(iCount : Integer):TWPDocumentTableRow; Overload; Virtual;
      Function AddTableCellURL(Const sText, sLink : String; Const sPrefix : String = ''; Const sSuffix : String = '') : TWPDocumentTableCell; Overload; Virtual;
      Function AddError(Const sText : String): TWPDocumentText; Overload; Virtual;
      Function StartTable(Const sHeadings : Array of String) : TWPDocumentTable; Overload; Virtual;
      Function AddTableRow(Const sContent : Array of String; bBold : Boolean = false) : TWPDocumentTableRow; Overload; Virtual;
      Function AddTableRowFlip(Const aContent : Array of String; iCount : Integer; bBold : Boolean = false) : TWPDocumentTableRow; Overload; Virtual;
      Function Heading(Const sText : String) : TWPDocumentParagraph; Overload; Virtual;

      Function ParaURL(Const sText, sLink : String; Const sPrefix : String = ''; Const sSuffix : String = '') : TWPDocumentParagraph; Overload; Virtual;
  End;

  THL7V2HTMLPublisher = Class (TFslObject)
  Private
    FDict : THL7V2Dictionary;
    Procedure ProcessMap(Const sPath : String; oMap : TFslStringMatch);

    Procedure ViewTable(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewTables(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewSegment(oModel: THL7V2Model; oMap: TFslStringMatch; Const sPrefix: String; oBuilder : THL7V2DocumentPublisher);

    Function ViewDataType(oDataType : THL7V2ModelDataType; oMap: TFslStringMatch; Const sPrefix: String) : String;
    Procedure ViewDataElement(oElement : THL7V2ModelDataElement; oMap: TFslStringMatch; Const sPrefix: String; bId : Boolean; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewField(oField : THL7V2ModelField; oMap: TFslStringMatch; Const sPrefix: String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewSegments(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewElements(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);

    Procedure ViewComponent(oComponent : THL7V2ModelComponent; oMap: TFslStringMatch; Const sPrefix: String; bExpand : Boolean; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewStructure(oStructure : THL7V2ModelStructure; oMap: TFslStringMatch; Const sPrefix: String; bQuick, bStructureTableView : Boolean; oModel : THL7V2Model; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewStructures(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewEvent(oEvent : THL7V2ModelEvent; oMap: TFslStringMatch; Const sPrefix: String; oModel : THL7V2Model; oBuilder : THL7V2DocumentPublisher);
    Function  ViewMessageSummary(oEvent : THL7V2ModelEvent; oMap: TFslStringMatch; Const sPrefix: String): String;
    Procedure ViewEvents(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewSegmentMap(oGroup : THL7V2ModelSegmentGroup; oMap : TFslStringMatch; Const sPrefix : String; iLevel : Integer; Var iCount : Integer; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewMessageType(oModel : THL7V2Model; oStruc : THL7V2ModelMessageStructure; oMap : TFslStringMatch; Const sPrefix : String; bEvents, bTitle : Boolean; oBuilder : THL7V2DocumentPublisher);
    Procedure ViewMessageTypes(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure GetVersionHome(oModel : THL7V2Model; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);

    Function DocToHTML(oDocument : TWPDocument; Const sTitle: String): String;

    Function ShowContent(oCell : THL7V2Cell; bHtml : Boolean) : String;

    Function HTMLView(oObject : THL7V2BaseObject; bHtml, bFullView : Boolean; iOffset, iIndex : Integer; sPrefix : String) : String;
    Function HTMLViewMessage(oMsg : THL7V2Message; bHtml, bFullView : Boolean; iOffset : Integer) : String;
    Function HTMLViewSegment(oSegment : THL7V2Segment; bHtml, bFullView : Boolean; iOffset : Integer) : String;
    Function HTMLViewDataElement(oDataElement : THL7V2DataElement; bHtml, bFullView : Boolean; iOffset, iIndex : Integer) : String;
    Function HTMLViewLocalDataElement(oDataElement : THL7V2DataElement; bHtml, bFullView : Boolean; iOffset, iIndex: Integer): String;
    Function HTMLViewComponent(oComponent : THL7V2Component; bHtml, bFullView : Boolean; iOffset, iIndex : Integer; sPrefix : String) : String;

    Function FormatTextToHTML(Const sValue : String) : String;

    Procedure PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    Procedure PublishDictHomeInternal(Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
    procedure SetDict(const Value: THL7V2Dictionary);

  protected
    function sizeInBytesV : cardinal; override;
  Public
    destructor Destroy; override;

    Function PublishDict(oMap : TFslStringMatch; sPrefix : String; Var sTitle : String): String; Overload; Virtual;
    Function PublishDict(Const sPath : String; sPrefix : String; Var sTitle : String): String; Overload; Virtual;
    Procedure PublishDict(Const sPath : String; sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;
    Procedure PublishDict(oMap : TFslStringMatch; sPrefix : String; oBuilder : THL7V2DocumentPublisher); Overload; Virtual;

    Function PublishMsg(oMsg : THL7V2Message; bHtml, bFullView, b3 : Boolean) : String;
    property dict : THL7V2Dictionary read FDict write SetDict;
  End;

implementation

Function CellString(Const s: String): String;
Begin
  If s = '' Then
    Result := '&nbsp;'
  Else
    Result := s;
End;

Function HTMLPad(AIndent: Integer): String;
Var
  i: Integer;
Begin
  Result := '';
  For i := 1 To AIndent * 3 Do
    Result := Result + '&nbsp;';
End;

Function Indent(AIndent: Integer): String;
Var
  i: Integer;
Begin
  Result := '';
  For i := 1 To AIndent * 3 Do
    Result := Result + ' ';
End;

Function LeftPad(Const bHtml: Boolean; iIndex : Integer; sStr : String = '') : String;
Var
  iLoop : Integer;
  iLength : Integer;
Begin
  If BHtml Then
    Begin
    Result := sStr;
    If sStr = '' Then
      iLength := iIndex + 2
    Else
      iLength := iIndex - Length(sStr);
    For iLoop := 1 To iLength Do
      Result := '&nbsp;' + Result;
    End
  Else
    Result := StringPadLeft(sStr, ' ', iIndex);
End;

{ THL7V2DocumentPublisher }

constructor THL7V2DocumentPublisher.Create;
begin
  inherited;
  Document := TWPDocument.Create;
  Document.Styles := TWPStyles.Create;
  FTitleStyle := Document.Styles.Add('Title').Link;
  FTitleStyle.Font.Bold := tsTrue;
  FTitleStyle.Font.Size := 12;
end;

destructor THL7V2DocumentPublisher.Destroy;
begin
  FTitleStyle.Free;
  inherited;
end;

procedure THL7V2DocumentPublisher.SetTitleStyle(const Value: TWPStyle);
begin
  FTitleStyle.Free;
  FTitleStyle := Value;
end;

function THL7V2DocumentPublisher.AddError(const sText: String): TWPDocumentText;
begin
  StartParagraph;
  Result := AddText(sText, True, True, 11);
  EndParagraph;
end;

function THL7V2DocumentPublisher.Heading(const sText: String): TWPDocumentParagraph;
begin
  Result := StartParagraph;
  Result.Format.MarginBottom := 6;
  AddText(sText, TitleStyle);
  EndParagraph;
end;

function THL7V2DocumentPublisher.AddTitle(sText: String): TWPDocumentParagraph;
begin
  FTitle := sText;
  Result := Heading(sText);
end;

Function THL7V2DocumentPublisher.StartTable(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTable;
Begin
  Result := Inherited StartTable;
  Result.VerticalMargin := 1;
End;

function THL7V2DocumentPublisher.StartTable(const sHeadings: array of String): TWPDocumentTable;
begin
  Result := StartTable;
  AddTableRow(sHeadings, true).Header := True;
end;

function THL7V2DocumentPublisher.AddTableRow(const sContent: Array of String; bBold : Boolean = false): TWPDocumentTableRow;
begin
  Result := AddTableRowFlip(sContent, 0, bBold);
end;

function THL7V2DocumentPublisher.StartRowFlip(iCount : Integer): TWPDocumentTableRow;
begin
  Result := StartTableRow;
  if iCount Mod 2 = 1 Then
    Result.Background := clWebWhiteSmoke;
end;

function THL7V2DocumentPublisher.AddTableRowFlip(Const aContent: Array of String; iCount : Integer; bBold : Boolean = false): TWPDocumentTableRow;
Var
  iLoop : Integer;
begin
  Result := StartRowFlip(iCount);
  For iLoop := Low(aContent) To High(aContent) Do
    AddTableCell(aContent[iLoop], false, bBold);
  EndTableRow;
end;

function THL7V2DocumentPublisher.AddTableCellURL(const sText, sLink, sPrefix, sSuffix: String): TWPDocumentTableCell;
begin
  Result := StartTableCell;
  ParaURL(sText, sLink, sPrefix, sSuffix);
  EndTableCell;
end;

function THL7V2DocumentPublisher.ParaURL(const sText, sLink, sPrefix, sSuffix: String): TWPDocumentParagraph;
begin
  Result := StartParagraph;
  if sPrefix <> '' Then
    AddTextStyledByContext(sPrefix);
  URL(sText, sLink);
  if sSuffix <> '' Then
    AddTextStyledByContext(sSuffix);
  EndParagraph;
end;

function THL7V2DocumentPublisher.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, FTitleStyle.sizeInBytes);
end;

{ THL7V2HTMLPublisher }

Procedure THL7V2HTMLPublisher.ProcessMap(Const sPath : String; oMap : TFslStringMatch);
Var
  sLeft, sRight : String;
  sName, sValue : String;
Begin
  Stringsplit(sPath, '?', sLeft, sRight);
  oMap.Forced := True;
  While sRight <> '' Do
  Begin
    StringSplit(sRight, '&', sLeft, sRight);
    StringSplit(sLeft, '=', sName, sValue);
    oMap.SetValueByKey(DecodePercent(sName), DecodePercent(sValue));
  End;
End;

Function THL7V2HTMLPublisher.PublishDict(Const sPath : String; sPrefix : String; Var sTitle : String): String;
Var
  oMap : TFslStringMatch;
Begin
  if pos('type', sPrefix) = 0 Then
    if pos('?', sPrefix) > 0 Then
      sPrefix := sPrefix + '&type=hl7v2'
    Else
      sPrefix := sPrefix + '?type=hl7v2';

  oMap := TFslStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    Result := PublishDict(oMap, sPrefix, sTitle);
  Finally
    oMap.Free;
  End;
End;

Procedure THL7V2HTMLPublisher.PublishDict(Const sPath : String; sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  oMap : TFslStringMatch;
Begin
  if pos('type', sPrefix) = 0 Then
    if pos('?', sPrefix) > 0 Then
      sPrefix := sPrefix + '&type=hl7v2'
    Else
      sPrefix := sPrefix + '?type=hl7v2';

  oMap := TFslStringMatch.Create;
  Try
    ProcessMap(sPath, oMap);
    If oMap.ExistsByKey('version') Then
      PublishDict(oMap, sPrefix, oBuilder)
    Else
      PublishDictHomeInternal(sPrefix, oBuilder);
  Finally
    oMap.Free;
  End;
End;

destructor THL7V2HTMLPublisher.Destroy;
begin
  FDict.Free;
  inherited;
end;

Function THL7V2HTMLPublisher.DocToHTML(oDocument : TWPDocument; Const sTitle: String): String;
Var
  oStream : TFslStringStream;
Begin
  oStream := TFslStringStream.Create;
  Try
    TWPFormatConvertor.Convert(oDocument, oStream, wpfHTML, [foNoHtmlBody]);
    Result := oStream.Data;
  Finally
    oStream.Free;
  End;
End;

Function THL7V2HTMLPublisher.PublishDict(oMap: TFslStringMatch; sPrefix: String; Var sTitle: String): String;
var
  oBuilder : THL7V2DocumentPublisher;
Begin
  if pos('type', sPrefix) = 0 Then
    if pos('?', sPrefix) > 0 Then
      sPrefix := sPrefix + '&type=hl7v2'
    Else
      sPrefix := sPrefix + '?type=hl7v2';

  oBuilder := THL7V2DocumentPublisher.Create;
  Try
    oBuilder.Start;
    PublishDict(oMap, sPrefix, oBuilder);
    oBuilder.Stop;
    sTitle := oBuilder.Title;
    Result := DocToHTML(oBuilder.Document, sTitle);
  Finally
    oBuilder.Free;
  End;
End;

Procedure THL7V2HTMLPublisher.PublishDict(oMap : TFslStringMatch; sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Begin
  if pos('type', sPrefix) = 0 Then
    if pos('?', sPrefix) > 0 Then
      sPrefix := sPrefix + '&type=hl7v2'
    Else
      sPrefix := sPrefix + '?type=hl7v2';

  Try
    PublishDictInternal(oMap, sPrefix, oBuilder);
  Except
    On e:Exception Do
      Begin
      oBuilder.Title := 'Exception: '+e.Message;
      oBuilder.AddParagraph(e.Message);
      End;
  End;
End;

Procedure THL7V2HTMLPublisher.PublishDictInternal(oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  oModel : THL7V2Model;
  sURL : String;
  sURL2 : String;
  sView : String;
  iLoop : Integer;
  aVer : THL7V2Version;
Begin
  oModel := FDict[FromVersionCode(oMap.GetValueByKey('version'))];
  sURL := sPrefix +'&version='+NAMES_HL7V2_VERSION[oModel.Version]+'&';
  sView := oMap.GetValueByKey('view');

  sURL2 := '';
  For iLoop := 0 To oMap.Count - 1 Do
    If Not StringEquals(oMap.KeyByIndex[iLoop], 'Version') And not StringExists(sPrefix, oMap.KeybyIndex[iLoop] + '=' + oMap.ValueByIndex[iLoop]) Then
        sURL2 := sURL2 + '&'+oMap.KeyByIndex[iLoop] + '=' + oMap.ValueByIndex[iLoop];

  oBuilder.StartParagraph;
  If sView <> '' Then
//    oBuilder.AddText('Version ' + NAMES_HL7V2_VERSION[oModel.Version] + ' Home', true, false)
//  Else
    oBuilder.URL('Version ' + NAMES_HL7V2_VERSION[oModel.Version] + ' Home', sURL);

  For aVer := Low(THL7V2Version) To High(THL7V2Version) Do
  Begin
    oBuilder.AddText('  ', false, false);
    If aVer = oModel.Version Then
      oBuilder.AddText(NAMES_HL7V2_VERSION[aVer], true, false)
    Else If (aVer In FDict.Versions) Then
      oBuilder.URL(NAMES_HL7V2_VERSION[aVer], sPrefix+'&version=' + NAMES_HL7V2_VERSION[aVer] + sURL2)
    Else
      oBuilder.AddText(NAMES_HL7V2_VERSION[aVer], false, false).Font.Foreground := clGray;
  End;

  oBuilder.EndParagraph;
  oBuilder.AddLine;

  If sView = '' Then
    GetVersionHome(oModel, sURL, oBuilder)
  Else If sView = 'tables' Then
    ViewTables(oModel, oMap, sURL, oBuilder)
  Else If sView = 'segments' Then
    ViewSegments(oModel, oMap, sURL, oBuilder)
  Else If sView = 'elements' Then
    ViewElements(oModel, oMap, sURL, oBuilder)
  Else If sView = 'structures' Then
    ViewStructures(oModel, oMap, sURL, oBuilder)
  Else If sView = 'events' Then
    ViewEvents(oModel, oMap, sURL, oBuilder)
  Else If sView = 'msgtypes' Then
    ViewMessageTypes(oModel, oMap, sURL, oBuilder)
  Else
    RaiseError('Request', 'View "'+oMap.GetValueByKey('view')+'" not recognised');
End;

Procedure THL7V2HTMLPublisher.GetVersionHome(oModel : THL7V2Model; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Begin
  oBuilder.AddTitle('HL7 Version ' + NAMES_HL7V2_VERSION[oModel.Version]);
  oBuilder.ParaURL('Segments', sPrefix + 'view=segments').Format.LeftIndent := 2;
  oBuilder.ParaURL('Elements', sPrefix + 'view=elements').Format.LeftIndent := 2;
  oBuilder.ParaURL('Tables', sPrefix + 'view=tables').Format.LeftIndent := 2;
  oBuilder.ParaURL('Structures', sPrefix + 'view=structures').Format.LeftIndent := 2;
  oBuilder.ParaURL('Events', sPrefix + 'view=events').Format.LeftIndent := 2;
  oBuilder.ParaURL('Messages', sPrefix + 'view=msgtypes').Format.LeftIndent := 2;
End;

Procedure THL7V2HTMLPublisher.ViewTables(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
Begin
  If oMap.GetValueByKey('tableid') <> '' Then
    ViewTable(oModel, oMap, sPrefix, oBuilder)
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Tables');
    oBuilder.StartTable;

    For iLoop := 0 To oModel.Tables.Count - 1 Do
    Begin
      oBuilder.StartRowFlip(iLoop);
      oBuilder.AddTableCell(StringPadLeft(IntegerToString(oModel.Tables[iLoop].ID), '0', 4));
      oBuilder.AddTableCellURL(oModel.Tables[iLoop].Description, sPrefix + 'view=tables&tableid=' + IntegerToString(oModel.Tables[iLoop].ID));
      oBuilder.EndTableRow;
    End;
    oBuilder.EndTable;
  End;
End;

Procedure THL7V2HTMLPublisher.ViewTable(oModel: THL7V2Model; oMap: TFslStringMatch; Const sPrefix: String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop, iInner, iRow : Integer;
  oTable : THL7V2ModelTable;
  oItem: THL7V2ModelTableItem;
  oStructure : THL7V2ModelStructure;
  oComponent : THL7V2ModelComponent;
  oSegment : THL7V2ModelSegment;
  oDataElement : THL7V2ModelDataElement;
Begin
  oTable := oModel.Tables.GetByID(StrToIntDef(oMap.GetValueByKey('tableid'), -1));
  If Not Assigned(oTable) Then
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Table ' + oMap.GetValueByKey('tableid'));
    oBuilder.AddError('Table ' + oMap.GetValueByKey('tableid') + ' not defined in version V' + NAMES_HL7V2_VERSION[oModel.Version]);
  End
  Else if oMap.GetValueByKey('mode') = 'uses' Then
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ': uses of table ' + StringPadLeft(IntegerToString(oTable.ID), '0', 4)+': '+ oTable.Description);
    oBuilder.StartTable(['ID', 'Name']);

    iRow := 0;
    for iLoop := 0 to oModel.Structures.Count - 1 Do
    Begin
      oStructure := oModel.Structures[iLoop];
      for iInner := 0 to oStructure.Components.Count - 1 Do
      Begin
        oComponent := oStructure.Components[iInner];
        if oComponent.RefTable = oTable Then
        Begin
          oBuilder.StartRowFlip(iRow);
          oBuilder.AddTableCellURL(oStructure.Name, sPrefix + 'view=structures&structureid='+oStructure.Name, '', '.'+inttostr(iInner + 1));
          oBuilder.AddTableCell(oComponent.Name);
          oBuilder.EndTableRow;
          inc(iRow);
        End;
      End;
    End;
    for iLoop := 0 to oModel.Segments.Count - 1 Do
    Begin
      oSegment := oModel.Segments[iLoop];
      for iInner := 0 to oSegment.Fields.Count - 1 Do
      Begin
        oDataElement := oSegment.Fields[iInner].RefDataElement;
        if oDataElement.RefTable = oTable Then
        Begin
          oBuilder.StartRowFlip(iRow);
          oBuilder.AddTableCellURL(oSegment.Code, sPrefix + 'view=segments&segmentid='+oSegment.Code, '', '.'+inttostr(iInner + 1));
          oBuilder.AddTableCell(oDataElement.Description);
          oBuilder.EndTableRow;
          inc(iRow);
        End;
      End
    End;
    oBuilder.EndTable;
  End
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Table ' + StringPadLeft(IntegerToString(oTable.ID), '0', 4)+': '+ oTable.Description);
    oBuilder.StartTable(['Code', '', 'ID', '', 'Description']);

    For iLoop := 0 To oTable.Items.Count - 1 Do
    Begin
      oItem := oTable.Items[iLoop];
      oBuilder.AddTableRowFlip([oItem.Code, '', IntegerToString(oItem.ID), '', oItem.Description], iLoop);
    End;

    oBuilder.EndTable;
    oBuilder.AddParagraph('');
    oBuilder.ParaURL('2.16.840.1.113883.12.'+oMap.GetValueByKey('tableid'), 'http://www.hl7.org/oid/OID_view.cfm?Comp_OID=2.16.840.1.113883.12.'+oMap.GetValueByKey('tableid'),
                      'OID for this table: ', '. Related Links:');
    oBuilder.ParaURL('All Tables', sPrefix + 'view=tables').Format.ListType := WPSParagraphListTypeBullets;
    oBuilder.ParaURL('Uses of this table', sPrefix + 'view=tables&tableid='+oMap.GetValueByKey('tableid')+'&mode=uses').Format.ListType := WPSParagraphListTypeBullets;
  End;
End;

Function THL7V2HTMLPublisher.ViewDataType(oDataType : THL7V2ModelDataType; oMap: TFslStringMatch; Const sPrefix: String): String;
Begin
  If oDataType.Length <> 0 Then
    Result := oDataType.Name + ': ' + oDataType.Description + ' (' + IntegerToString(oDataType.Length) + ')'
  Else
    Result := oDataType.Name + ': ' + oDataType.Description;

End;

Procedure THL7V2HTMLPublisher.ViewDataElement(oElement : THL7V2ModelDataElement; oMap: TFslStringMatch; Const sPrefix: String; bId : Boolean; oBuilder : THL7V2DocumentPublisher);
Var
  oStruc: THL7V2ModelStructure;
Begin
  oStruc := oElement.RefStructure;
  If bId Then
  Begin
    oBuilder.AddTableCell(StringPadLeft(IntegerToString(oElement.Id), '0', 5));
    oBuilder.AddTableCell('');
  End;

  oBuilder.AddTableCell(oElement.Description);
  oBuilder.AddTableCell('');
  ViewStructure(oStruc, oMap, sPrefix, True, False, Nil, oBuilder);
  oBuilder.AddTableCell('');
  oBuilder.AddTableCell(oElement.LengthDesc);
  oBuilder.AddTableCell('');
  If oElement.Table = 0 Then
    oBuilder.AddTableCell('')
  Else If oElement.RefTable.Items.Count  > 0 Then
    oBuilder.AddTableCellURL(oElement.RefTable.Description, sPrefix + 'view=tables&tableid=' + IntegerToString(oElement.RefTable.ID))
  Else
    oBuilder.AddTableCell(oElement.RefTable.Description + ' (No specified values)');
End;

Procedure THL7V2HTMLPublisher.ViewField(oField : THL7V2ModelField; oMap: TFslStringMatch; Const sPrefix: String; oBuilder : THL7V2DocumentPublisher);
Var
  sReq: String;
  sRep: String;
Begin
  If oField.Required Then
    sReq := 'true'
  Else
    sReq := '';
  If Not oField.Repeatable Then
    sRep := ''
  Else If oField.RepeatCount = 0 Then
    sRep := 'Y'
  Else
    sRep := 'Y(' + IntegerToString(oField.RepeatCount) + ')';
  oBuilder.AddTableCell(IntegerToString(oField.FieldNumber));
  ViewDataElement(oField.RefDataElement, oMap, sPrefix, False, oBuilder);
  oBuilder.AddTableCell('');
  oBuilder.AddTableCell(sReq);
  oBuilder.AddTableCell('');
  oBuilder.AddTableCell(sRep);
End;

Procedure THL7V2HTMLPublisher.ViewSegment(oModel: THL7V2Model; oMap: TFslStringMatch; Const sPrefix: String; oBuilder : THL7V2DocumentPublisher);
Var
  oSegment : THL7V2ModelSegment;
  iLoop : Integer;
  iMsg : Integer;
  oEvent : THL7V2ModelEvent;
  oMsg : THL7V2ModelEventMessage;
Begin
  oSegment := oModel.Segments.GetByCode(oMap.GetValueByKey('segmentid'));
  If Not Assigned(oSegment) Then
    RaiseError('ViewSegment', 'Segment "'+oMap.GetValueByKey('segmentid')+'" not found');
  oBuilder.AddTitle('Segment ' + oSegment.Code + ': '+oSegment.Description+' (v' + NAMES_HL7V2_VERSION[oModel.Version] + ')');
  oBuilder.StartTable(['', 'Description', '', 'Data Type', '', 'Size', '', 'Table', '', 'Required', '', 'Repetition']);

  For iLoop := 0 To oSegment.Fields.Count - 1 Do
  Begin
    oBuilder.StartRowFlip(iLoop);
    ViewField(oSegment.Fields[iLoop], oMap, sPrefix, oBuilder);
    oBuilder.EndTableRow;
  End;
  oBuilder.EndTable;
  oBuilder.addLine;
  oBuilder.Heading('Events using this segment');

  For iLoop := 0 To oModel.Events.Count - 1 Do
    Begin
    oEvent := oModel.Events[iLoop];
    For iMsg := 0 To oEvent.Messages.Count - 1 Do
      Begin
      oMsg := oEvent.Messages[iMsg];
      If oMsg.RefStructure.UsesSegment(oSegment) Then
        oBuilder.ParaURL(oMsg.RefStructure.Name, sPrefix+'view=msgtypes&msgid='+oMsg.RefStructure.Name, oEvent.Name+': ').Format.ListType := WPSParagraphListTypeBullets;
      If oMsg.RefReplyStructure.UsesSegment(oSegment) Then
        oBuilder.ParaURL(oMsg.RefReplyStructure.Name, sPrefix+'view=msgtypes&msgid='+oMsg.RefReplyStructure.Name, oEvent.Name+': ').Format.ListType := WPSParagraphListTypeBullets;
      End;
    End;
End;

Procedure THL7V2HTMLPublisher.ViewSegments(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
Begin
  If oMap.GetValueByKey('segmentid') <> '' Then
    ViewSegment(oModel, oMap, sPrefix, oBuilder)
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Segments');
    oBuilder.StartTable(['Code', 'Description']);
    For iLoop := 0 To oModel.Segments.Count - 1 Do
      If Length(oModel.Segments[iLoop].code) = 3 Then
      Begin
        oBuilder.StartRowFlip(iLoop);
        oBuilder.AddTableCellURL(oModel.Segments[iLoop].code, sPrefix + 'view=segments&segmentid=' + oModel.Segments[iLoop].code);
        oBuilder.AddTableCell(oModel.Segments[iLoop].description);
        oBuilder.EndTableRow;
      End;
    oBuilder.EndTable;
  End;
End;

Procedure THL7V2HTMLPublisher.ViewElements(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
Begin
  oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Elements');
  oBuilder.StartTable(['ID', '', 'Name', '', 'Structure', 'Length', '', 'Table']);

  For iLoop := 0 To oModel.DataElements.count - 1 Do
  Begin
    oBuilder.StartRowFlip(iLoop);
    ViewDataElement(oModel.DataElements[iLoop], oMap, sPrefix, True, oBuilder);
    oBuilder.EndTableRow;
  End;
  oBuilder.EndTable;
End;

Procedure THL7V2HTMLPublisher.ViewComponent(oComponent : THL7V2ModelComponent; oMap: TFslStringMatch; Const sPrefix: String; bExpand : Boolean; oBuilder : THL7V2DocumentPublisher);
Begin
  oBuilder.AddTableCell(oComponent.Name);
  oBuilder.AddTableCell('');

  If Assigned(oComponent.RefStructure) And (THL7V2ModelStructure(oComponent.RefStructure).Components.Count > 0) Then
    oBuilder.AddTableCellURL(ViewDataType(oComponent.RefDataType, oMap, sPrefix), sPrefix + 'view=structures&structureid=' + THL7V2ModelStructure(oComponent.RefStructure).Name)
  Else If Assigned(oComponent.RefDataType) Then
    oBuilder.AddTableCell(ViewDataType(oComponent.RefDataType, oMap, sPrefix))
  Else
    oBuilder.AddTableCell('Unknown Data Type');

  oBuilder.AddTableCell('');

  If Assigned(oComponent.RefTable) Then
    oBuilder.AddTableCellURL(oComponent.RefTable.Description, sPrefix + 'view=tables&tableid=' + IntegerToString(oComponent.RefTable.ID))
  Else
  Begin
    oBuilder.AddTableCell('');
    oBuilder.AddTableCell('');
    oBuilder.AddTableCell('');
  End;
End;

Procedure THL7V2HTMLPublisher.ViewStructure(oStructure : THL7V2ModelStructure; oMap: TFslStringMatch; Const sPrefix: String; bQuick, bStructureTableView : Boolean; oModel : THL7V2Model; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop, iInner, iRow, iComp : Integer;
  oComponent : THL7V2ModelComponent;
  oSegment : THL7V2ModelSegment;
  oDataElement : THL7V2ModelDataElement;
Begin
  If bQuick Then
  Begin
    If oStructure.Components.Count > 0 Then
      If (oStructure.Description <> '') Then
        oBuilder.AddTableCellURL(oStructure.DataType, sPrefix + 'view=structures&structureid=' + oStructure.Name, '', ' : ' + oStructure.Description)
      Else
        oBuilder.AddTableCellURL(oStructure.DataType, sPrefix + 'view=structures&structureid=' + oStructure.Name, '')
    Else If oStructure.RefDataType <> Nil Then
      oBuilder.AddTableCell(ViewDataType(oStructure.RefDataType, oMap, sPrefix))
    Else
      oBuilder.AddTableCell(oStructure.DataType+' : ' + oStructure.Description);
  End
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Component ' + oStructure.Name);
    If (oStructure.Components.Count = 0) Then
      oBuilder.AddError('Data Type ' + oStructure.Name + ' has no components in this version of HL7')
    Else
    Begin
      oBuilder.AddParagraph('Data Structure Definition for ' + oStructure.Name + ' ('+oStructure.Description + ')');
      oBuilder.StartTable(['ID', 'Name', '', 'Data Type', '', 'Table', '', 'Table Values']);
      For iLoop := 0 To oStructure.Components.Count - 1 Do
      Begin
        oBuilder.StartRowFlip(iLoop);
        oBuilder.AddTableCell(IntegerToString(iLoop + 1));
        ViewComponent(oStructure.Components[iLoop], oMap, sPrefix, True, oBuilder);
        oBuilder.EndTableRow;
      End;
      oBuilder.EndTable;
    End;
    oBuilder.AddParagraph('');
    oBuilder.AddParagraph('Uses of '+oStructure.Name);

    oBuilder.StartTable(['ID', 'Name']);

    iRow := 0;
    for iLoop := 0 to oModel.Segments.Count - 1 Do
    Begin
      oSegment := oModel.Segments[iLoop];
      for iInner := 0 to oSegment.Fields.Count - 1 Do
      Begin
        oDataElement := oSegment.Fields[iInner].RefDataElement;
        if oDataElement.RefStructure = oStructure Then
        Begin
          oBuilder.StartRowFlip(iRow);
          oBuilder.AddTableCellURL(oSegment.Code, sPrefix + 'view=segments&segmentid='+oSegment.Code, '', '.'+inttostr(iInner + 1));
          oBuilder.AddTableCell(oDataElement.Description);
          oBuilder.EndTableRow;
          inc(iRow);
        End
        Else
        Begin
          For iComp := 0 to oDataElement.RefStructure.Components.Count - 1 Do
          Begin
            oComponent := oDataElement.RefStructure.Components[iComp];
            if oComponent.RefStructure = oStructure Then
            Begin
              oBuilder.StartRowFlip(iRow);
              oBuilder.AddTableCellURL(oSegment.Code, sPrefix + 'view=segments&segmentid='+oSegment.Code, '', '.'+inttostr(iInner + 1)+'.'+inttostr(iComp + 1));
              oBuilder.AddTableCell(oDataElement.Description+' / '+oComponent.Name);
              oBuilder.EndTableRow;
              inc(iRow);
            End;
          End;
        End;
      End
    End;
    for iLoop := 0 to oModel.Segments.Count - 1 Do
    Begin
      oSegment := oModel.Segments[iLoop];
      for iInner := 0 to oSegment.Fields.Count - 1 Do
      Begin
        If oSegment.Fields[iInner].RefDataElement.Structure = 'varies' Then
        Begin
          oBuilder.StartRowFlip(iRow);
          oBuilder.AddTableCellURL(oSegment.Code, sPrefix + 'view=segments&segmentid='+oSegment.Code, '*: ', '.'+inttostr(iInner + 1));
          oBuilder.AddTableCell(oSegment.Fields[iInner].RefDataElement.Description);
          oBuilder.EndTableRow;
          inc(iRow);
        End;
      End
    End;

    oBuilder.EndTable;
  End;
End;

Procedure THL7V2HTMLPublisher.ViewStructures(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
Begin
  If oMap.GetValueByKey('structureid') <> '' Then
  Begin
    If oModel.Structures.ExistsByName(oMap.GetValueByKey('structureid')) Then
      ViewStructure(oModel.Structures.GetByName(oMap.GetValueByKey('structureid')), oMap, sPrefix, False, True, oModel, oBuilder)
    Else
      RaiseError('ViewStructures', 'Structure "'+oMap.GetValueByKey('structureid')+'" not found');
  End
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Structures');
    oBuilder.StartTable(['Name', 'Description', 'Type']);

    For iLoop := 0 To oModel.Structures.Count - 1 Do
    Begin
      oBuilder.StartRowFlip(iLoop);
      ViewStructure(oModel.Structures[iLoop], oMap, sPrefix, True, True, Nil, oBuilder);
      oBuilder.EndTableRow;
    End;
    oBuilder.EndTable;
  End;
End;

Function THL7V2HTMLPublisher.ViewMessageSummary(oEvent : THL7V2ModelEvent; oMap: TFslStringMatch; Const sPrefix: String): String;
Var
  iLoop : Integer;
  oMsg : THL7V2ModelEventMessage;
Begin
  Result := '';
  For iLoop := 0 To oEvent.Messages.count - 1 Do
    Begin
    oMsg := oEvent.Messages[iLoop];
    StringAppend(Result, '('+oMsg.Message+':'+oMsg.Reply+')', ', ');
    End;
End;

Procedure THL7V2HTMLPublisher.ViewEvent(oEvent : THL7V2ModelEvent; oMap: TFslStringMatch; Const sPrefix: String; oModel : THL7V2Model; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
  oMsg : THL7V2ModelEventMessage;
Begin
  oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Event ' + oEvent.Name+': '+oEvent.Description);

  If oEvent.Messages.count = 1 Then
  Begin
    oMsg := oEvent.Messages[0];
    oBuilder.Heading('Message: ' + oMsg.Message+'/'+ oMsg.Structure);
    If Assigned(oMsg.RefStructure) Then
      ViewMessageType(oModel, oMsg.RefStructure, oMap, sPrefix, False, False, oBuilder);

    oBuilder.Heading('Reply: ' + oMsg.Reply+'/'+ oMsg.ReplyStructure);
    If Assigned(oMsg.RefReplyStructure) Then
      ViewMessageType(oModel, oMsg.RefReplyStructure, oMap, sPrefix, False, False, oBuilder)
  End
  Else
    For iLoop := 0 To oEvent.Messages.count - 1 Do
    Begin
      oMsg := oEvent.Messages[iLoop];
      oBuilder.AddParagraph('Message: ' + oMsg.Message+'/'+ oMsg.Structure+ '; Reply : ' + oMsg.Reply+'/'+ oMsg.ReplyStructure).Format.ListType := WPSParagraphListTypeBullets;
    End;
End;

Procedure THL7V2HTMLPublisher.ViewEvents(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
  oEvent : THL7V2ModelEvent;
Begin
  If oMap.GetValueByKey('eventid') <> '' Then
    Begin
    If oModel.Events.ExistsByName(oMap.GetValueByKey('eventid')) Then
      ViewEvent(oModel.Events.GetByName(oMap.GetValueByKey('eventid')), oMap, sPrefix, oModel, oBuilder)
    Else
      RaiseError('ViewEvents', 'Event "'+oMap.GetValueByKey('eventid')+'" not found');
    End
  Else
  Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Events');

    oBuilder.StartTable(['Code', 'Description']);

    For iLoop := 0 To oModel.Events.Count - 1 Do
    Begin
      oEvent := oModel.Events[iLoop];
      oBuilder.StartRowFlip(iLoop);
      oBuilder.AddTableCellURL(oEvent.Name, sPrefix + 'view=events&eventid=' + oEvent.Name);
      oBuilder.AddTableCell(oModel.Events[iLoop].Description+ ': '+ ViewMessageSummary(oEvent, oMap, sPrefix));
      oBuilder.EndTableRow;
    End;
    oBuilder.EndTable;
  End;

End;

Function DescribeCardinality(oGroup : THL7V2ModelSegmentGroup): String;
Begin
  If oGroup.Optional Then
    Result := '0..'
  Else
    Result := '1..';
  If oGroup.Repeating Then
    Result := Result + '*'
  Else
    Result := Result + '1'
End;

Procedure THL7V2HTMLPublisher.ViewSegmentMap(oGroup : THL7V2ModelSegmentGroup; oMap : TFslStringMatch; Const sPrefix : String; iLevel : Integer; Var iCount : Integer; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
  sDesc : String;
Begin
  If oGroup = Nil Then
    oBuilder.AddTableRow(['not loaded', ''])
  Else If oGroup.GroupType = gtSingle Then
  Begin
    inc(iCount);
    oBuilder.StartRowFlip(iCount);
    oBuilder.AddTableCellURL(oGroup.Code, sPrefix + 'view=segments&segmentid=' + oGroup.Code, Indent(iLevel));
    oBuilder.AddTableCell(DescribeCardinality(oGroup));
    oBuilder.EndTableRow();
  End
  Else
  Begin
    If iLevel > 0 Then
    Begin
      inc(iCount);
      If oGroup.GroupType = gtChoice Then
        sDesc := '<Choice of:'
      Else If oGroup.Optional Then
        sDesc := '['+ oGroup.Code
      Else If oGroup.Repeating Then
        sDesc := '{'+ oGroup.Code;
      oBuilder.StartRowFlip(iCount);
      oBuilder.AddTableCell(Indent(iLevel)+sDesc);
      oBuilder.AddTableCell(DescribeCardinality(oGroup));
      oBuilder.EndTableRow();
    End;
    For iLoop := 0 To oGroup.Children.Count - 1 Do
      ViewSegmentMap(oGroup.Children[iLoop], oMap, sPrefix, iLevel + 1, iCount, oBuilder);
    If iLevel > 0 Then
    Begin
      If oGroup.GroupType = gtChoice Then
        sDesc := '>'
      Else If oGroup.Repeating Then
        sDesc := '}'
      Else If oGroup.Optional Then
        sDesc := ']';
      inc(iCount);
      oBuilder.AddTableRowFlip([Indent(iLevel)+sDesc, ''], iCount)
    End;
  End;
End;

Procedure THL7V2HTMLPublisher.ViewMessageType(oModel : THL7V2Model; oStruc : THL7V2ModelMessageStructure; oMap : TFslStringMatch; Const sPrefix : String; bEvents, bTitle : Boolean; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
  iCount : Integer;
  oEvent : THL7V2ModelEvent;
  sXMLError : String;
Begin
  If Not Assigned(oStruc.XMLMap) Then
    Begin
    If oModel.Version <= hv23 Then
      // sXmlError := 'XML Encoding not defined for this version'
    Else If FDict.SchemaStore = Nil Then
      sXmlError := 'No XML Schema support'
    Else
      Try
        oStruc.XMLMap := FDict.SchemaStore.ProduceSchemaMap(oModel.Version, oStruc.Name);
        If Not Assigned(oStruc.XMLMap) Then
          sXMLError := '&lt;No Map&gt;'
      Except
        On e:Exception Do
          sXMLError := e.Message;
      End;
    End;

  if bTitle Then
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Message Type ' + oStruc.Name);

  If sXMLError <> '' Then
    oBuilder.AddError(sXMLError);

  oBuilder.StartTable(['Segment', 'Cardinality']).BorderPolicy := BorderPolicyBox;
  iCount := -1;
  ViewSegmentMap(oStruc.SegmentMap, oMap, sPrefix, 0, iCount, oBuilder);
  oBuilder.EndTable;

  If bEvents And Assigned(oModel) Then
  Begin
    oBuilder.Heading('Events using this structure');
    oBuilder.StartTable(['Code', 'Description']);
    For iLoop := 0 To oModel.Events.Count - 1 Do
    Begin
      oEvent := oModel.Events[iLoop];
      oBuilder.StartRowFlip(iLoop);
      oBuilder.AddTableCellURL(oEvent.Name, sPrefix + 'view=events&eventid=' + oEvent.Name);
      oBuilder.AddTableCell(oEvent.Description);
      oBuilder.EndTableRow;
    End;
    oBuilder.EndTable;
  End;
End;

Procedure THL7V2HTMLPublisher.ViewMessageTypes(oModel : THL7V2Model; oMap : TFslStringMatch; Const sPrefix : String; oBuilder : THL7V2DocumentPublisher);
Var
  iLoop : Integer;
Begin
  If oMap.GetValueByKey('msgid') <> '' Then
    Begin
    If oModel.MessageStructures.ExistsByName(oMap.GetValueByKey('msgid')) Then
      ViewMessageType(oModel, oModel.MessageStructures.GetByName(oMap.GetValueByKey('msgid')), oMap, sPrefix, True, True, oBuilder)
    Else
      RaiseError('ViewMessageTypes', 'Message Type "'+oMap.GetValueByKey('msgid')+'" not found');
    End
  Else
    Begin
    oBuilder.AddTitle('v' + NAMES_HL7V2_VERSION[oModel.Version] + ' Message Types');
    For iLoop := 0 To oModel.MessageStructures.Count - 1 Do
      oBuilder.ParaURL(oModel.MessageStructures[iLoop].Name, sPrefix + 'view=msgtypes&msgid=' + oModel.MessageStructures[iLoop].Name).Format.ListType := WPSParagraphListTypeBullets;
    End;
End;

Function THL7V2HTMLPublisher.PublishMsg(oMsg: THL7V2Message; bHtml, bFullView, b3: Boolean): String;
Begin
  Result := HTMLViewMessage(oMsg, bHtml, bFullView, 0);
End;

Function THL7V2HTMLPublisher.HTMLView(oObject : THL7V2BaseObject; bHtml, bFullView : Boolean; iOffset, iIndex : Integer; sPrefix: String) : String;
Begin
  If oObject Is THL7V2Segment Then
    Result := HTMLViewSegment(oObject As THL7V2Segment, bHtml, bFullView, iOffset)
  Else If oObject Is THL7V2DataElement  Then
    Result := HTMLViewDataElement(oObject As THL7V2DataElement, bHtml, bFullView, iOffset, iIndex)
  Else If oObject Is THL7V2Component  Then
    Result := HTMLViewComponent(oObject As THL7V2Component, bHtml, bFullView, iOffset, iIndex, sPrefix)
  Else
    Result := 'THL7V2HTMLPublisher.HTMLView: Unknown Object ' + oObject.ClassName  + '<br>';
End;

Function THL7V2HTMLPublisher.HTMLViewMessage(oMsg : THL7V2Message; bHtml, bFullView : Boolean; iOffset : Integer) : String;
Var
  sStr : String;
  iLoop : Integer;
Begin
  sStr := '';
  If bHtml Then
    Begin
    sStr := sStr + '<span style="font-weight:bold;">Structure</span><p>' + cReturn;
    For iLoop := 0 To  oMsg.Segments.Count - 1 Do
      sStr := sStr + '<span style="font-weight:bold;">' + oMsg.Segments.Segment[iLoop].Code + '</span> ' + HTMLView(oMsg.Segments.Segment[iLoop], bHtml, bFullView, iOffset, 0, '')
    End
  Else
    Begin
    sStr := sStr + 'Structure:' + cReturn;
    For iLoop := 0 To oMsg.Segments.Count - 1 Do
      sStr := sStr + oMsg.Segments.Segment[iLoop].Code + HTMLView(oMsg.Segments.Segment[iLoop], bHtml, bFullView, iOffset, 2, '');
    End;
  Result := sStr;
End;

Function THL7V2HTMLPublisher.HTMLViewSegment(oSegment : THL7V2Segment; bHtml, bFullView : Boolean; iOffset : Integer) : String;
Var
  iLoop : Integer;
Begin
  If bHtml Then
    Begin
    If Assigned(oSegment.Definition) Then
      Result := StringReverseCamel(oSegment.Definition.Description) + '<span style="color:#DFDFDF;"></span><br>' + cReturn
    Else
      Result := '<span style="color:#DFDFDF;">(Unknown)</span><br>' + cReturn;

    For iLoop := 0 To oSegment.Fields.Count - 1 Do
      Result := Result + HTMLView(oSegment.Fields[iLoop], bHtml, bFullView, iOffset + 2, iLoop + 1, '') + cReturn;
    End
  Else
    Begin
    If Assigned(oSegment.Definition) Then
      Result := ' ' + oSegment.Definition.Description + cReturn
    Else
      Result := '';

    For iLoop := 0 To oSegment.Fields.Count - 1 Do
      Result := Result + HTMLView((oSegment.Fields[iLoop] As THL7V2DataElement), BHtml, BFullView, iOffset + 2, iLoop + 1, '');
    End;
End;

Function THL7V2HTMLPublisher.HTMLViewDataElement(oDataElement : THL7V2DataElement; bHtml, bFullView: Boolean; iOffset, iIndex: Integer): String;
Var
  iLoop : Integer;
Begin
  Result := HTMLViewLocalDataElement(oDataElement, bHtml, bFullView, iOffset, iIndex);
  If oDataElement.Repeats.Count > 0 Then
    Begin
    If bHtml Then
      Begin
      Result := Result + LeftPad(bHtml, iOffset + 2) + 'Repeats = ' + IntegerToString(oDataElement.Repeats.Count) + '<br>' + cReturn;
      For iLoop := 1 To oDataElement.Repeats.Count Do
        Result := Result + HTMLViewLocalDataElement(oDataElement.Repeats[iLoop - 1], bHtml, bFullView, iOffset + 4, iIndex)
      End
    Else
      Begin
      Result := Result + LeftPad(bHtml, iOffset) + 'Repeats = ' + IntegerToString(oDataElement.Repeats.Count) + cReturn;
      For iLoop := 1 To oDataElement.Repeats.Count Do
        Result := Result + HTMLViewLocalDataElement(oDataElement.Repeats[iLoop - 1], bHtml, bFullView, iOffset + 4, iIndex)
      End;
    End;
End;

Function THL7V2HTMLPublisher.HTMLViewLocalDataElement(oDataElement : THL7V2DataElement; bHtml, bFullView : Boolean; iOffset, iIndex: Integer) : String;
Var
  iLoop : Integer;
  sStr : String;
Begin
  If iindex <> -1 Then
    sStr := LeftPad(bHtml, 2, IntegerToString(iIndex))
  Else
    sStr := '';
  If bHtml Then
    Begin
    Result := LeftPad(BHtml, iOffset) + sStr + ' ';
    If oDataElement.Definition = Nil Then
      Result := Result + 'Unknown'
    Else
      Result := Result + oDataElement.Definition.RefDataElement.Description;

    If (oDataElement.Components.Count = 0) Then
      Result := Result + ' = "' + ShowContent(oDataElement, true) + '"<br>' + cReturn
    Else
      Begin
      Result := Result + '<br>' + cReturn;
      For iLoop := 0 To oDataElement.Components.Count - 1 Do
        Result := Result + HTMLView(oDataElement.Components[iLoop], bHtml, bFullView, iOffset + 2, iLoop + 1, IntegerToString(iIndex));
      End;
    End
  Else
    Begin
    Result := LeftPad(bHtml, iOffset) + sStr + ' ';
    If oDataElement.Definition = Nil Then
      Result := Result + 'Unknown'
    Else
      Result := Result + oDataElement.Definition.RefDataElement.Description;

    Result := Result + '=' + ShowContent(oDataElement, false) + cReturn;

    For iLoop := 0 To oDataElement.Components.Count - 1 Do
      Result := Result + HTMLView(oDataElement.Components[iLoop], bHtml, bFullView, iOffset + 2, iLoop + 1, IntegerToString(iIndex));
    End;
End;

Function THL7V2HTMLPublisher.HTMLViewComponent(oComponent : THL7V2Component; bHtml, bFullView : Boolean; iOffset, iIndex : Integer; sPrefix: String) : String;
Var
  iLoop : Integer;
Begin
  Result := LeftPad(bHtml, iOffset) + LeftPad(bHtml, 4, sPrefix + '.' + IntegerToString(iIndex)) + ' ';

  If Assigned(oComponent.Definition) Then
    Result := Result + oComponent.Definition.Name
  Else
    Result := Result + 'unknown';

  If oComponent.Components.Count > 0 Then
    Begin
    Result := Result + '<br>' + cReturn;
    For iLoop := 0 To oComponent.Components.Count - 1 Do
      Result := Result + HTMLView(oComponent.Components[iLoop], bHtml, bFullView, iOffset + 4, iLoop + 1, sPrefix + '.' + IntegerToString(iIndex));
    End
  Else
    If bHtml Then
      Result := Result + '="' + ShowContent(oComponent, true) + '"<br>' + cReturn
    Else
      Result := Result + '=' + ShowContent(oComponent, true) + cReturn;
End;

Function THL7V2HTMLPublisher.FormatTextToHTML(Const sValue: String): String;
Begin
  Result := StringReplace(EncodeXML(sValue), '&#x0D;&#x0A;', '<br>');
End;

procedure THL7V2HTMLPublisher.SetDict(const Value: THL7V2Dictionary);
begin
  FDict.Free;
  FDict := Value;
end;

function THL7V2HTMLPublisher.ShowContent(oCell: THL7V2Cell;  bHtml: Boolean): String;
begin
  result := oCell.GetRawContentForScripting;
  if bHtml Then
    result := FormatTextToHTML(result);
end;

Procedure THL7V2HTMLPublisher.PublishDictHomeInternal(
  Const sPrefix: String; oBuilder: THL7V2DocumentPublisher);
Var
  aVer : THL7V2Version;
Begin
  oBuilder.AddTitle('Knowledge Base: HL7v2');

  For aVer := Low(THL7V2Version) To High(THL7V2Version) Do
  If aVer In FDict.Versions Then
  Begin
    oBuilder.StartParagraph.Format.LeftIndent := 2;
    oBuilder.URL(NAMES_HL7V2_VERSION[aVer], sPrefix +'&version=' + NAMES_HL7V2_VERSION[aVer]);
    oBuilder.EndParagraph;
  End;
End;

function THL7V2HTMLPublisher.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDict.sizeInBytes);
end;

end.
