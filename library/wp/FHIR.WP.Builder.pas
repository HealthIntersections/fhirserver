Unit FHIR.WP.Builder;

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
  fsl_base, fsl_utilities, fsl_stream, fsl_collections,
  dicom_Dictionary,
  wp_graphics,
  wp_types, wp_document, wp_format, wp_working, wp_definers;
 
Type
  TWPDocumentBuilderScopeAllowedItem = (aisSection, aisParaStart, aisTableStart, aisRowStart, aisCellStart, aisImage, aisFieldStart,
                                        aisText, aisSectionStop, aisTableStop, aisRowStop, aisCellStop, aisFieldStop, aisParaStop,
                                        aisBreak);

  TWPDocumentBuilderScopeAllowedItems = Set Of TWPDocumentBuilderScopeAllowedItem;

  TWPDocumentBuilderScopeTableStatus = (tsNone, tsInside, tsBanned);

  TWPDocumentBuilderScope = Class(TFslObject)
    Private
      FAllowed : TWPDocumentBuilderScopeAllowedItems;
      FObject : TWPDocumentObject;
      FTableStatus : TWPDocumentBuilderScopeTableStatus;

      Function GetObject : TWPDocumentObject;
      Procedure SetObject(Const Value : TWPDocumentObject);
    Protected
      Function ErrorClass : EFslExceptionClass; Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPDocumentBuilderScope; Overload;
      Function Clone : TWPDocumentBuilderScope; Overload;

      Procedure Assign(oObject : TFslObject); Overload; Override;

      Function AllowedAsText : String;

      Property Allowed : TWPDocumentBuilderScopeAllowedItems Read FAllowed Write FAllowed;
      Property TableStatus : TWPDocumentBuilderScopeTableStatus Read FTableStatus Write FTableStatus;
      Property Object_ : TWPDocumentObject Read GetObject Write SetObject;
  End;

  TWPDocumentBuilderScopes = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPDocumentBuilderScope;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBuilderScope);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function Get(Const aValue : Integer) : TWPDocumentBuilderScope; Reintroduce; Overload; Virtual;

    Public
      Function Link : TWPDocumentBuilderScopes; Overload;
      Function Clone : TWPDocumentBuilderScopes; Overload;

      Function New : TWPDocumentBuilderScope; Reintroduce; Overload; Virtual;

      Procedure Push(aAllowed : TWPDocumentBuilderScopeAllowedItems; oObject : TWPDocumentObject; bIntoTable : Boolean); Overload; Virtual;
      Procedure Pop; Overload; Virtual;
      Function Current : TWPDocumentBuilderScope; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPDocumentBuilderScope Read GetElement Write SetElement; Default;
  End;

Const
  TWPDOCUMENTBUILDERSCOPE_ALLOWED_VALUE : Array[TWPDocumentBuilderScopeAllowedItem] Of String =
    ('Section', 'Paragraph', 'Table', 'Row', 'Cell', 'Image', 'Field', 'Text', 'SectionEnd', 'TableStop', 'RowStop', 'CellStop', 'FieldStop', 'ParaStop', 'Break');

  TWPDOCUMENTBUILDERSCOPETABLESTATUS_NAMES : Array [TWPDocumentBuilderScopeTableStatus] Of String =
    ('None', 'Inside', 'Banned');

Type
  TWPDocumentBuilderImportOption = (ioSuppressSections, ioSuppressTables, ioTableExceptions);
  TWPDocumentBuilderImportOptions = Set Of TWPDocumentBuilderImportOption;

  TFieldAction = (FieldActionDelete, FieldActionLeave, FieldActionReplaceContent, FieldActionReplaceAll);

  TWPDocumentBuilder = Class;

  // this is a twin to TWPFieldValueProvider - and the namespaces must match
  TWPFieldValueProvider = Class (TFslObject)
    Public
      Function GetNamespace : String; Overload; Virtual;

      // when a field or a section is encountered, query to find out what to do
      Function GetAction(oField : TWPDocumentField): TFieldAction; Overload; Virtual;
      Function GetAction(oField : TWPDocumentSection): TFieldAction; Overload; Virtual;

      // if GetAction returns FieldActionReplace, then the existing contents are deleted,
      // and then this method is called. The Value Provider may use any of the methods
      // that are appropriate for the context of a field or a section
      Procedure Replace(oField : TWPDocumentField; oBuilder : TWPDocumentBuilder); Overload; Virtual;
      Procedure Replace(oField : TWPDocumentSection; oBuilder : TWPDocumentBuilder); Overload; Virtual;
  End;

  TWPFieldValueProviderList = Class(TFslObjectList)
    Private
      Function GetElement(Const iIndex : Integer) : TWPFieldValueProvider;
      Procedure SetElement(Const iIndex : Integer; Const oValue : TWPFieldValueProvider);

    Protected
      Function ItemClass : TFslObjectClass; Override;
    Public
      Function Link : TWPFieldValueProviderList; Overload;
      Function Clone : TWPFieldValueProviderList; Overload;

      Function New : TWPFieldValueProvider; Reintroduce; Overload; Virtual;

      Function GetByName(Const sName : String) : TWPFieldValueProvider; Overload; Virtual;

      Property Elements[Const iIndex : Integer] : TWPFieldValueProvider Read GetElement Write SetElement; Default;
  End;


  TWPDocumentBuilder = Class (TFslObject)
    Private
      FDocument : TWPDocument;
      FScopeList : TWPDocumentBuilderScopes;
      FIterator : TWPDocumentIterator;
      FFocus : TWPDocumentObject;
      FFieldValueProviders : TWPFieldValueProviderList;
      FDicomDictionary: TDicomDictionary;

      Function GetDocument : TWPDocument;
      Procedure SetDocument(Const Value : TWPDocument);

      Function GetHasDocument : Boolean;
      Procedure SetHasDocument(Const Value : Boolean);

      Function Current(aClass : TFslObjectClass) : TFslObject;

      Function Summarise(aItems :TWPDocumentBuilderScopeAllowedItems) : String;

      Procedure ImportDocumentStyles(oDocument : TWPDocument);
      Procedure AddTableContents(oTable : TWPDocumentTable; oBlocks : TWPDocumentObjects);
      Procedure AddSectionContents(oSection : TWPDocumentSection; oBlocks : TWPDocumentObjects; bInCell : Boolean; aOptions : TWPDocumentBuilderImportOptions);
      Function InsertTextStyledByContext(Const sText : String; Const iIndex : Integer) : TWPDocumentText; Overload; // grabs style from parent element

      Procedure ProcessFields(oParagraph : TWPDocumentParagraph); Overload;
      Procedure ProcessFields(oTable : TWPDocumentTable); Overload;
      Procedure ProcessFields(oRow : TWPDocumentTableRow); Overload;
      Procedure ProcessFields(oCell : TWPDocumentTableCell); Overload;
      Procedure ProcessFields(oStack : TFslStringList; oContainer : TWPDocumentContainer); Overload;
      Function ProcessSection(oStack : TFslStringList; oSection : TWPDocumentSection; oList : TWPDocumentBlocks; iIndex : Integer) : Integer; Overload;
      Function ProcessField(oField : TWPDocumentField; oList : TWPDocumentContents; iIndex : Integer) : Integer; Overload;
    procedure SetDicomDictionary(const Value: TDicomDictionary);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      constructor Create(oDocument : TWPDocument); Overload; Virtual;
      destructor Destroy; Override;

      Function Link : TWPDocumentBuilder; Overload;

      // API part 0: Automated processing of fields
      // this goes through all fields (including section type fields)
      // in the document. For each field, if there is a value provider registered,
      // it asks the value provider what to do (or else it leaves it untouched)
      // The value provider can say to remove it, leave it in place, or replace the
      // contents. Replaced contents will also be processed (though there is a check to
      // prevent recursion)
      Procedure ProcessFields; Overload;

      // API part 1: finding current location
      // default location is the document itself
      Procedure Home; // select current document as context. Starting anything will append content (so clear if you want to start again)
      Function More : Boolean;
      Procedure FirstField;
      Procedure NextField;
      Function CurrentField : TWPDocumentObject; // may be either a Field or a Section

      // API part 2: transactional control
      // once you've found the place from which you want to start, you
      // must start / stop - these set up the transactional system required
      // for changes to be made
      Procedure Start;
      Procedure Stop;
      Procedure Reset;
      Function HasStarted : Boolean;

      // API part 3: content management
      // Note: GetCurrent will return a transient wrapper.
      // always free the object you get from this routine.
      Function GetCurrent : TWPDocumentObject;
      // the way to use this is to getCurrent, set the properties, and then SetCurrent(current), finally freeing it
      // do not change current while doing this - results will be very unpredictable but always bad.
      // set current does not set children, only properties
      Procedure SetCurrent(oObject : TWPDocumentObject);

      Procedure Clear; // completely clear the current context (properties and children)
      Procedure ClearChildren; // clear the current context (usually the content of the thing, not it's properties)

      // all these can only be used in the right context. an exception will be thrown if the context is wrong

      // a section may be added to either a document or a section
      Function StartSection : TWPDocumentSection; Overload;
      Function StartSection(Const sNamespace, sName : String) : TWPDocumentSection; Overload;
      Function StartSection(Const sNamespace, sName, sTitle : String) : TWPDocumentSection; Overload;
      Procedure EndSection;

      // a table may be added to a document or a section
      Function CanStartTable : Boolean;
      Function StartTable(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTable; Overload; Virtual;
      Procedure EndTable;

      // a table row may be added to a table
      Function StartTableRow(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableRow; Overload;
      // the owning row does not contain the row, but owns it for purposes of table indenting and document folding
      Function StartTableRow(oOwningRow : TWPDocumentTableRow; aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableRow; Overload;
      Procedure EndTableRow;

      // a table cell may be added to a table row
      Function StartTableCell(iSpan : Integer = 1; aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableCell;
      Procedure EndTableCell;
      Function AddTableCell(Const sText : String; bBold : Boolean = False; bItalics : Boolean = False; iSpan : Integer = 1) : TWPDocumentTableCell; Overload;
      Function AddTableCell(Const sText : String; oStyle : TWPStyle) : TWPDocumentTableCell; Overload;
      Function AddTableCell(Const sText, sStyle : String) : TWPDocumentTableCell; Overload;

      Function ContextInField : Boolean;

      // a field may be added to a paragraph
      Function StartField(oStyle : TWPStyle = Nil) : TWPDocumentField; Overload;
      Function StartField(Const sNamespace, sName : String; bUseContextStyle : Boolean = False) : TWPDocumentField; Overload;
      Procedure EndField;
      Function AddField(Const sNamespace, sName, sContent : String) : TWPDocumentField; Overload;
      Function AddField(Const sNamespace, sName, sContent : String; oStyle : TWPStyle) : TWPDocumentField; Overload;

      // can only use this when there is a field in context. This will end the field,
      // and start a new field which is a clone of the current field, and insert sText
      // between the fields
      // Note that this feature will be removed in the future when nested fields
      // are allowed.- or when some other approach is figured out
      Function CloneField(Const sText : String) : TWPDocumentField;

      // you can define a hotspot when the context is a field, a table cell, or a table row.
      Function DefineHotspot(Const sURL : String) : TWPHotspot; Overload;
      // the meaning of colour depends on the type of object.
      //   For a field, the colours will apply to the font.
      //   For table rows and cells, they will apply to the background of the row/cell.
      //   For images, they are the color of the line around the focus when clicked or hovered respectively
      Function DefineHotspot(Const sURL : String; Const iLink, iHover : TColour) : TWPHotspot; Overload;

      Function URL(Const sText, sLink : String) : TWPDocumentField; Overload;

      // you can add a paragraph to a document, a section, or a table cell
      Function StartParagraph(oStyle : TWPStyle = Nil) : TWPDocumentParagraph;
      Procedure EndParagraph(bDropIfEmpty : Boolean = False);
      Function AddParagraph : TWPDocumentParagraph; Overload;
      Function AddParagraph(Const sContent : String; oStyle : TWPStyle = Nil) : TWPDocumentParagraph; Overload;

      // you can add a line break to a document or a section
      Function AddLineBreak : TWPDocumentLineBreak;
      Function AddLine(rWidth : Real = 1) : TWPDocumentBreak; Overload;
      Function AddLine(aColour : TColour; iWidth : Integer = 1) : TWPDocumentBreak; Overload;
      Function AddLine(rWidth : Real; aColour : TColour; iWidth : Integer = 1) : TWPDocumentBreak; Overload;
      Function AddPageBreak : TWPDocumentBreak;

      // you can add text to a paragaph or a field
      // no cReturns - have to break that into paragraphs (or see ImportText)
      Function AddTextPlain(Const sText : String) : TWPDocumentText; Overload;
      Function AddTextStyledByContext(Const sText : String) : TWPDocumentText; Overload; // grabs style from parent element
      Function AddText(Const sText, sStyle : String) : TWPDocumentText; Overload;
      Function AddText(Const sText : String; oStyle : TWPStyle) : TWPDocumentText; Overload;
      Function AddText(Const sText : String; bBold, bItalics : Boolean) : TWPDocumentText; Overload;
      Function AddText(Const sText : String; bBold, bItalics : Boolean; iSize : Integer) : TWPDocumentText; Overload;

      Procedure AddTextWithBreaks(Const sText : String; bStyled : Boolean); Overload;

      // you can add an image to a paragaph or a field
      Function AddImage(oImage : TFslGraphic; oStyle : TWPStyle = Nil) : TWPDocumentImage; Overload;
      Function AddImage(oImage, oSelectionImage : TFslGraphic; oStyle : TWPStyle = Nil) : TWPDocumentImage; Overload;
      Function AddImage(Const sFilename : String; oStyle : TWPStyle = Nil) : TWPDocumentImage; Overload;
      Function AddImage(Const sFilename, sSelectionFilename : String; oStyle : TWPStyle = Nil) : TWPDocumentImage; Overload;

      // true if any content imported
      // what you can import depends on the context. The rules outlined above are followed
      Function ImportText(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String) : Boolean;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : AnsiString; aImageLoader : TWPLoadImageEvent = Nil) : Boolean; Overload;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : AnsiString; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean; Overload;
{$IFNDEF VER130}
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String; aImageLoader : TWPLoadImageEvent = Nil) : Boolean; Overload;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean; Overload;
{$ENDIF}
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oStream : TFslStream; aImageLoader : TWPLoadImageEvent) : Boolean; Overload;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oStream : TFslStream; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean; Overload;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oDocument : TWPDocument) : Boolean; Overload;
      Function ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oDocument : TWPWorkingDocument) : Boolean; Overload;
      Function ImportBuffer(aOptions : TWPDocumentBuilderImportOptions; Const oBuffer : TFslBuffer) : Boolean;

      // macros
      Function ReplaceField(Const sNamespace, sName, sContent : String) : Boolean; Overload;
      Function ReplaceField(Const aNamespaces : Array Of String; sName, sContent: String): Boolean; Overload;

      // Settings
      Property Document : TWPDocument Read GetDocument Write SetDocument;
      Property HasDocument : Boolean Read GetHasDocument Write SetHasDocument;
      Property FieldValueProviders : TWPFieldValueProviderList Read FFieldValueProviders;
      Property DicomDictionary : TDicomDictionary read FDicomDictionary write SetDicomDictionary;
  End;



Implementation


Uses
  wp_imaging, wp_text, wp_native;



Constructor TWPDocumentBuilderScope.Create;
Begin
  Inherited;
End;


Destructor TWPDocumentBuilderScope.Destroy;
Begin
  FObject.Free;
  Inherited;
End;


Function TWPDocumentBuilderScope.Link : TWPDocumentBuilderScope;
Begin
  Result := TWPDocumentBuilderScope(Inherited Link);
End;


Function TWPDocumentBuilderScope.Clone : TWPDocumentBuilderScope;
Begin
  Result := TWPDocumentBuilderScope(Inherited Clone);
End;


Procedure TWPDocumentBuilderScope.Assign(oObject : TFslObject);
Begin
  Inherited;

  Allowed := TWPDocumentBuilderScope(oObject).Allowed;
  Object_ := TWPDocumentBuilderScope(oObject).Object_;
  TableStatus := TWPDocumentBuilderScope(oObject).TableStatus;
End;



Function TWPDocumentBuilderScope.ErrorClass : EFslExceptionClass;
Begin
  Result := EWPException;
End;


Function TWPDocumentBuilderScope.GetObject : TWPDocumentObject;
Begin
  Assert(Invariants('GetObject', FObject, TWPDocumentObject, 'Object'));
  Result := FObject;
End;


Procedure TWPDocumentBuilderScope.SetObject(Const Value : TWPDocumentObject);
Begin
  FObject.Free;
  FObject := Value;
End;


function TWPDocumentBuilderScope.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FObject.sizeInBytes);
end;

Function TWPDocumentBuilderScopes.Link : TWPDocumentBuilderScopes;
Begin
  Result := TWPDocumentBuilderScopes(Inherited Link);
End;


Function TWPDocumentBuilderScopes.Clone : TWPDocumentBuilderScopes;
Begin
  Result := TWPDocumentBuilderScopes(Inherited Clone);
End;


Function TWPDocumentBuilderScopes.New : TWPDocumentBuilderScope;
Begin
  Result := TWPDocumentBuilderScope(Inherited New);
End;


Function TWPDocumentBuilderScopes.ItemClass : TFslObjectClass;
Begin
  Result := TWPDocumentBuilderScope;
End;


Function TWPDocumentBuilderScopes.GetElement(Const iIndex : Integer) : TWPDocumentBuilderScope;
Begin
  Result := TWPDocumentBuilderScope(ObjectByIndex[iIndex]);
End;


Procedure TWPDocumentBuilderScopes.SetElement(Const iIndex : Integer; Const oValue : TWPDocumentBuilderScope);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


Function TWPDocumentBuilderScopes.Get(Const aValue : Integer) : TWPDocumentBuilderScope;
Begin
  Result := TWPDocumentBuilderScope(Inherited Get(aValue));
End;


Procedure TWPDocumentBuilderScopes.Push(aAllowed : TWPDocumentBuilderScopeAllowedItems; oObject : TWPDocumentObject; bIntoTable : Boolean);
Var
  oOld : TWPDocumentBuilderScope;
  oScope : TWPDocumentBuilderScope;
Begin
  If Count > 0 Then
    oOld := Current
  Else
    oOld := Nil;

  oScope := New;
  Try
    oScope.Allowed := aAllowed;
    oScope.Object_ := oObject;

    If (Count > 0) And Assigned(oOld) And ((oOld.FTableStatus = tsInside) Or bIntoTable) Then
      oScope.FTableStatus := tsInside
    Else
      oScope.FTableStatus := tsNone;

    Insert(0, oScope.Link);
  Finally
    oScope.Free;
  End;
End;


Function TWPDocumentBuilderScopes.Current : TWPDocumentBuilderScope;
Begin
  Result := Get(0);
  If Result = Nil Then
    RaiseError('Current', 'Attempt to read scope when scope stack is empty');
End;


Procedure TWPDocumentBuilderScopes.Pop;
Begin
  DeleteByIndex(0);
End;


function TWPDocumentBuilderScope.AllowedAsText: String;
var
  aLoop : TWPDocumentBuilderScopeAllowedItem;
  oBuilder : TFslStringBuilder;
begin
  oBuilder := TFslStringBuilder.Create;
  Try
    For aLoop := aisSection To aisBreak Do
      If aLoop in FAllowed Then
      Begin
        oBuilder.Append(TWPDOCUMENTBUILDERSCOPE_ALLOWED_VALUE[aLoop]);
        oBuilder.Append(' ');
      End;
    Result := oBuilder.ToString;
  Finally
    oBuilder.Free;
  End;
end;


Type
  TWPFieldIterator = Class (TWPDocumentIterator)
    Protected
      Function Included(oObject : TWPDocumentObject) : Boolean; Overload; Override;
    End;


Function TWPFieldIterator.Included(oObject : TWPDocumentObject) : Boolean;
Begin
  Result := (oObject Is TWPDocumentField);
End;



Constructor TWPDocumentBuilder.Create;
Begin
  Inherited;

  FDocument := Nil;
  FFieldValueProviders := TWPFieldValueProviderList.Create;
  FScopeList := TWPDocumentBuilderScopes.Create;
End;


Constructor TWPDocumentBuilder.Create(oDocument : TWPDocument);
Begin
  Create;

  Document := oDocument;
End;


Destructor TWPDocumentBuilder.Destroy;
Begin
  FDicomDictionary.Free;
  FScopeList.Free;
  FDocument.Free;
  FIterator.Free;
  FFieldValueProviders.Free;

  Inherited;
End;


Function TWPDocumentBuilder.Link : TWPDocumentBuilder;
Begin
  Result := TWPDocumentBuilder(Inherited Link);
End;


Function TWPDocumentBuilder.GetDocument: TWPDocument;
Begin
  Assert(Invariants('GetDocument', FDocument, TWPDocument, 'FDocument'));

  Result := FDocument;
End;


procedure TWPDocumentBuilder.SetDicomDictionary(const Value: TDicomDictionary);
begin
  FDicomDictionary.free;
  FDicomDictionary := Value;
end;

Procedure TWPDocumentBuilder.SetDocument(Const Value : TWPDocument);
Begin
  Assert(Not Assigned(Value) Or Invariants('SetDocument', Value, TWPDocument, 'Value'));
  Assert(CheckCondition(FIterator = Nil, 'SetDocument', 'Cannot call SetDocument while iterating'));
  Assert(CheckCondition(Not HasStarted, 'SetDocument', 'Cannot call SetDocument when started'));

  FDocument.Free;
  FDocument := Value;

  FFocus := FDocument;
End;


Function TWPDocumentBuilder.GetHasDocument : Boolean;
Begin
  Result := Assigned(FDocument);
End;


Procedure TWPDocumentBuilder.SetHasDocument(Const Value : Boolean);
Begin
  If HasDocument And Not Value Then
  Begin
    FDocument.Free;
    FDocument := Nil;
  End
  Else If Not HasDocument And Value Then
  Begin
    FDocument := TWPDocument.Create;
    FFocus := FDocument;
  End;
End;


Function TWPDocumentBuilder.More: Boolean;
Begin
  Result := Assigned(FIterator);
End;


Procedure TWPDocumentBuilder.Home;
Begin
  Assert(CheckCondition(Not HasStarted, 'SetDocument', 'Cannot Navigate while started'));
  FFocus := FDocument;
  FIterator.Free;
  FIterator := Nil;
End;



Procedure TWPDocumentBuilder.FirstField;
Begin
  Assert(CheckCondition(Not HasStarted, 'FirstField', 'Cannot Navigate while started'));
  Assert(CheckCondition(FIterator = Nil, 'FirstField', 'Cannot call FirstField while iterating'));
  FIterator := TWPFieldIterator.Create;
  FIterator.Document := FDocument.Link;
  FIterator.First;
  If FIterator.More Then
    FFocus :=  FIterator.Current
  Else
    Home;
End;

Procedure TWPDocumentBuilder.NextField;
Begin
  Assert(CheckCondition(Not HasStarted, 'NextField', 'Cannot Navigate while started'));
  Assert(CheckCondition(FIterator <> Nil, 'NextField', 'Cannot call NextField while not iterating'));
  FIterator.Next;
  If FIterator.More Then
    FFocus :=  FIterator.Current
  Else
    Home;
End;


Function TWPDocumentBuilder.CurrentField: TWPDocumentObject;
Begin
  If (FFocus <> Nil) And (FFocus Is TWPDocumentField) Then
    Result := FFocus
  Else
    Result := Nil;
End;

Function TWPDocumentBuilder.HasStarted : Boolean;
Begin
  Result := FScopeList.Count > 0;
End;


Procedure TWPDocumentBuilder.Reset;
Begin
  FScopeList.Clear;
  Document.Clear;
  Home;
End;

Procedure TWPDocumentBuilder.Start;
Begin
  Assert(CheckCondition(Not HasStarted, 'Start', 'Cannot Navigate while started'));
  Assert(Invariants('Start', FFocus, TWPDocumentObject, 'FFocus'));

  // what to push depends on what focus is
  If FFocus Is TWPDocumentContainer Then
    FScopeList.Push([aisSection, aisBreak, aisParaStart, aisTableStart], FFocus.Link, False)
  Else If FFocus Is TWPDocumentField Then
    FScopeList.Push([aisFieldStop, aisText, aisImage], FFocus.Link, False)
  Else If FFocus Is TWPDocumentParagraph Then
    FScopeList.Push([aisParaStop, aisFieldStart, aisText, aisImage], FFocus.Link, False)
  Else
    RaiseError('Start', 'Unhandled start class '+FFocus.className);
End;


Procedure TWPDocumentBuilder.Stop;
Begin
  Assert(CheckCondition(HasStarted, 'Stop', 'Must call Start before using the document builder'));

  FScopeList.Pop;

  Try
    If FScopeList.Count > 0 Then
      RaiseError('Stop', StringFormat('%d open scopes exist', [FScopeList.Count]));
  Except
    // we catch this here. If this was raised in the absence of another error, there is a logic error in
    // the code using the builder. If there was an exception, then this error should be ignored.
  End;
  FScopeList.Clear;

  {$IFOPT C+}
  TWPDocumentValidator.ValidateAssert(Document);
  {$ENDIF}
End;


Procedure TWPDocumentBuilder.Clear;
Begin
  TWPDocumentObject(Current(TWPDocumentObject)).clear;
End;

Procedure TWPDocumentBuilder.ClearChildren;
Begin
  TWPDocumentObject(Current(TWPDocumentObject)).ClearChildren;
End;


Function TWPDocumentBuilder.StartSection : TWPDocumentSection;
Begin
  Result := StartSection('', '');
End;


Function TWPDocumentBuilder.StartSection(Const sNamespace, sName : String) : TWPDocumentSection;
Begin
  Result := StartSection(sNamespace, sName, '');
End;


Function TWPDocumentBuilder.Current(aClass : TFslObjectClass) : TFslObject;
Begin
  Assert(Invariants('Current', FScopeList.Current.Object_, aClass, 'Current Object'));

  Result := FScopeList.Current.Object_;
End;


Function TWPDocumentBuilder.GetCurrent: TWPDocumentObject;
Begin
  Result :=  TWPDocumentObject(Current(TWPDocumentObject)).Clone; // note comments in interface about the use of .Clone here
End;


Procedure TWPDocumentBuilder.SetCurrent(oObject: TWPDocumentObject);
Begin
  Assert(CheckCondition(HasStarted, 'SetCurrent', 'Must call Start before using the document builder'));

  TWPDocumentObject(Current(oObject.ClassType)).Assign(oObject);
End;



Function TWPDocumentBuilder.StartSection(Const sNamespace, sName, sTitle : String) : TWPDocumentSection;
Begin
  Assert(CheckCondition(HasStarted, 'StartSection', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisSection In FScopeList.Current.Allowed, 'StartSection', 'Unable to insert Sections at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentSection.Create;
  Try
    Result.Namespace := sNamespace;
    Result.Name := sName;
    Result.Title := sTitle;

    TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.Add(Result.Link);

    FScopeList.Push([aisSection, aisBreak, aisParaStart, aisTableStart, aisSectionStop], Result.Link, False);
  Finally
    Result.Free;
  End;
End;

Procedure TWPDocumentBuilder.EndSection;
Begin
  Assert(CheckCondition(HasStarted, 'EndSection', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisSectionStop In FScopeList.Current.Allowed, 'StartSection', 'Unable to insert Section Ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  FScopeList.Pop;
End;


Function TWPDocumentBuilder.CanStartTable: Boolean;
Begin
  Assert(CheckCondition(HasStarted, 'CanStartTable', 'Must call Start before using the document builder'));
  Result := aisTableStart In FScopeList.Current.Allowed;
End;


Function TWPDocumentBuilder.StartTable(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTable;
Begin
  Assert(CheckCondition(HasStarted, 'StartTable', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisTableStart In FScopeList.Current.Allowed, 'StartTable', 'Unable to insert Tables at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentTable.Create;
  Try
    Result.ReadOnly := aReadOnly;

    TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.Add(Result.Link);

    FScopeList.Push([aisTableStop, aisRowStart], Result.Link, True);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.StartTableRow(aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableRow;
Begin
  Result := StartTableRow(Nil, aReadOnly);
End;

Function TWPDocumentBuilder.StartTableRow(oOwningRow : TWPDocumentTableRow; aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableRow;
Begin
  Assert(CheckCondition(HasStarted, 'StartTableRow', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisRowStart In FScopeList.Current.Allowed, 'StartTableRow', 'Unable to insert Table Rows at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentTableRow.Create;
  Try
    Result.ReadOnly := aReadOnly;

    If oOwningRow = Nil Then
      TWPDocumentTable(Current(TWPDocumentTable)).Rows.Add(Result.Link)
    Else
    Begin
      Assert(TWPDocumentTable(Current(TWPDocumentTable)).OwnsRow(oOwningRow), 'Attempt to insert a nested row into a table where the owning row is not from the table');
      oOwningRow.Rows.Add(Result.Link)
    End;

    FScopeList.Push([aisRowStop, aisCellStart], Result.Link, False);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.StartTableCell(iSpan : Integer = 1; aReadOnly : TWPDocumentObjectReadOnly = ReadOnlyDefault) : TWPDocumentTableCell;
Begin
  Assert(CheckCondition(HasStarted, 'StartTableCell', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisCellStart In FScopeList.Current.Allowed, 'StartTableCell', 'Unable to insert Table Cells at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentTableCell.Create;
  Try
    Result.Span := iSpan;
    Result.ReadOnly := aReadOnly;

    TWPDocumentTableRow(Current(TWPDocumentTableRow)).Cells.Add(Result.Link);

    FScopeList.Push([aisCellStop, aisBreak, aisParaStart], Result.Link, False);
  Finally
    Result.Free;
  End;
End;


Procedure TWPDocumentBuilder.EndTableCell;
Begin
  Assert(CheckCondition(HasStarted, 'EndTableCell', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisCellStop In FScopeList.Current.Allowed, 'EndTableCell', 'Unable to insert Table Cell Ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  FScopeList.Pop;
End;


Procedure TWPDocumentBuilder.EndTableRow;
Begin
  Assert(CheckCondition(HasStarted, 'EndTableRow', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisRowStop In FScopeList.Current.Allowed, 'EndTableRow', 'Unable to insert Table Row Ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  FScopeList.Pop;
End;


Procedure TWPDocumentBuilder.EndTable;
Begin
  Assert(CheckCondition(HasStarted, 'EndTable', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisTableStop In FScopeList.Current.Allowed, 'EndTable', 'Unable to insert Table Ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  FScopeList.Pop;
End;


Function TWPDocumentBuilder.AddField(Const sNamespace, sName, sContent : String) : TWPDocumentField;
Begin
  Result := AddField(sNamespace, sName, sContent, Nil);
End;


Function TWPDocumentBuilder.AddField(Const sNamespace, sName, sContent: String; oStyle: TWPStyle): TWPDocumentField;
Begin
  Result := StartField(oStyle);
  Result.Namespace := sNamespace;
  Result.Name := sName;

  If Assigned(oStyle) Then
    AddText(sContent, oStyle)
  Else
    AddTextPlain(sContent);

  EndField;
End;

Function TWPDocumentBuilder.CloneField(Const sText : String) : TWPDocumentField;
Var
  oOld : TWPDocumentField;
  iIndex : Integer;
  oPara : TWPDocumentParagraph;
Begin
  Assert(CheckCondition(HasStarted, 'StartField', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisFieldStop In FScopeList.Current.Allowed, 'CloneField', 'Unable to clone Fields at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentField.Create;
  Try
    oOld := TWPDocumentField(Current(TWPDocumentField));
    Result.Assign(oOld);
    Result.Contents.Clear; // got them from assign and we don't want them

    // we can't pop the stack of scope lists, so we are going to hack it for now. (See comment where the method is declared)
    // contiuing the ugliness, we now have to find the paragraph that owns the field
    oPara := TWPDocumentParagraph(FDocument.GetOwnerFor(oOld));
    Assert(Invariants('CloneField', oPara, TWPDocumentParagraph, 'Parent Paragraph'));
    iIndex := oPara.Contents.IndexByReference(oOld);

    FScopeList.Current.Object_ := oPara.Link;
    FScopeList.Current.Allowed := [aisParaStop, aisFieldStart, aisText, aisImage];
    InsertTextStyledByContext(sText, iIndex+1);
    TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Insert(iIndex + 2, Result.Link);
    FScopeList.Current.Object_ := Result.Link;
    FScopeList.Current.Allowed := [aisFieldStop, aisText, aisImage];

    If (FFocus = oOld) Then
    Begin
      // the cloned field comes after the old field. If the iterator is pointing at the old one, then
      // we must update to point at the new field so it can more on properly.
      FIterator.Next;
      Assert(FIterator.Current = Result, 'Error cloning field: the iterator has been advanced to next in order to point to the newly cloned field, but it does not point to the correct field');
      FFocus := FIterator.Current;
    End;
  Finally
    Result.Free;
  End;
End;

Function TWPDocumentBuilder.StartField(oStyle : TWPStyle = Nil) : TWPDocumentField;
Begin
  Assert(CheckCondition(HasStarted, 'StartField', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisFieldStart In FScopeList.Current.Allowed, 'StartField', 'Unable to insert Fields at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentField.Create;
  Try
    If Assigned(oStyle) Then
      Result.Style := oStyle.Name;

    TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link);

    FScopeList.Push([aisFieldStop, aisText, aisImage], Result.Link, False);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.StartField(Const sNamespace, sName : String; bUseContextStyle : Boolean = False) : TWPDocumentField;
Begin
  Assert(CheckCondition(HasStarted, 'StartField', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisFieldStart In FScopeList.Current.Allowed, 'StartField', 'Unable to insert Fields at this point in the document ('+FScopeList.Current.AllowedAsText+')'));

  Result := TWPDocumentField.Create;
  Try
    Result.Namespace := sNamespace;
    Result.Name := sName;

    If bUseContextStyle Then
    Begin
      Result.Style := TWPDocumentParagraph(Current(TWPDocumentParagraph)).Style;
      Result.Font.Assign(TWPDocumentParagraph(Current(TWPDocumentParagraph)).Font);
    End;

    TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link);

    FScopeList.Push([aisFieldStop, aisText, aisImage], Result.Link, False);
  Finally
    Result.Free;
  End;
End;


Procedure TWPDocumentBuilder.EndField;
Begin
  Assert(CheckCondition(HasStarted, 'EndField', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisFieldStop In FScopeList.Current.Allowed, 'EndTable', 'Unable to insert Field Ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  FScopeList.Pop;
End;


Function TWPDocumentBuilder.AddParagraph : TWPDocumentParagraph;
Begin
  Result := AddParagraph('');
End;


Function TWPDocumentBuilder.AddParagraph(Const sContent : String; oStyle : TWPStyle = Nil) : TWPDocumentParagraph;
Begin
  Result := StartParagraph(oStyle);
  AddText(sContent, oStyle);
  EndParagraph;
End;


Function TWPDocumentBuilder.StartParagraph(oStyle: TWPStyle = Nil): TWPDocumentParagraph;
Begin
  Assert(CheckCondition(HasStarted, 'StartParagraph', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisParaStart In FScopeList.Current.Allowed, 'StartParagraph', 'Unable to insert paragraphs at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentParagraph.Create;
  Try
    If Assigned(oStyle) Then
      Result.Style := oStyle.Name;

    If FScopeList.Current.Object_ Is TWPDocumentTableCell Then
      TWPDocumentTableCell(Current(TWPDocumentTableCell)).Paragraphs.Add(Result.Link)
    Else
      TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.Add(Result.Link);

    FScopeList.Push([aisParaStop, aisFieldStart, aisText, aisImage], Result.Link, False);
  Finally
    Result.Free;
  End;
End;


Procedure TWPDocumentBuilder.EndParagraph(bDropIfEmpty : Boolean = False);
Var
  oPara : TWPDocumentParagraph;
Begin
  Assert(CheckCondition(HasStarted, 'EndParagraph', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisParaStop In FScopeList.Current.Allowed, 'EndParagraph', 'Unable to insert paragraph ends at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  oPara := TWPDocumentParagraph(Current(TWPDocumentParagraph)).Link;
  Try
    FScopeList.Pop;

    If bDropIfEmpty And (oPara.Contents.Count = 0) Then
    Begin
      If FScopeList.Current.Object_ Is TWPDocumentTableCell Then
        TWPDocumentTableCell(Current(TWPDocumentTableCell)).Paragraphs.DeleteByReference(oPara)
      Else
        TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.DeleteByReference(oPara);
    End;
  Finally
    oPara.Free;
  End;
End;


Function TWPDocumentBuilder.AddLineBreak : TWPDocumentLineBreak;
Begin
  Assert(CheckCondition(HasStarted, 'AddLineBreak', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisText In FScopeList.Current.Allowed, 'AddLineBreak', 'Unable to insert Line Breaks at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentLineBreak.Create;
  Try
    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link)
    Else
      TWPDocumentField(Current(TWPDocumentField)).Contents.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.AddTextPlain(Const sText : String) : TWPDocumentText;
Begin
  Result := AddText(sText, False, False);
End;


Function TWPDocumentBuilder.AddText(Const sText, sStyle : String) : TWPDocumentText;
Begin
  Result := AddText(sText, Document.Styles.GetByName(sStyle));
End;


Function TWPDocumentBuilder.AddTextStyledByContext(Const sText : String) : TWPDocumentText;
Begin
  Assert(CheckCondition(HasStarted, 'AddText', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisText In FScopeList.Current.Allowed, 'AddText', 'Unable to insert Text at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  If StringExists(sText, cReturn) Then
    RaiseError('AddText', StringFormat('Attempt to add a paragraph in text: %s', [sText]));

  Result := TWPDocumentText.Create;
  Try
    Result.Value := sText;

    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
    Begin
      Result.Style := TWPDocumentParagraph(Current(TWPDocumentParagraph)).Style;
      Result.Font.Assign(TWPDocumentParagraph(Current(TWPDocumentParagraph)).Font);
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link)
    End
    Else
    Begin
      Result.Style := TWPDocumentField(Current(TWPDocumentField)).Style;
      Result.Font.Assign(TWPDocumentField(Current(TWPDocumentField)).Font);
      TWPDocumentField(Current(TWPDocumentField)).Contents.Add(Result.Link);
    End;
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.InsertTextStyledByContext(Const sText : String; Const iIndex : Integer) : TWPDocumentText;
Begin
  Assert(CheckCondition(HasStarted, 'AddText', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisText In FScopeList.Current.Allowed, 'AddText', 'Unable to insert Text at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  If StringExists(sText, cReturn) Then
    RaiseError('AddText', StringFormat('Attempt to add a paragraph in text: %s', [sText]));

  Result := TWPDocumentText.Create;
  Try
    Result.Value := sText;

    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
    Begin
      Result.Style := TWPDocumentParagraph(Current(TWPDocumentParagraph)).Style;
      Result.Font.Assign(TWPDocumentParagraph(Current(TWPDocumentParagraph)).Font);
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Insert(iIndex, Result.Link)
    End
    Else
    Begin
      Result.Style := TWPDocumentField(Current(TWPDocumentField)).Style;
      Result.Font.Assign(TWPDocumentField(Current(TWPDocumentField)).Font);
      TWPDocumentField(Current(TWPDocumentField)).Contents.Insert(iIndex, Result.Link);
    End;
  Finally
    Result.Free;
  End;
End;



Function TWPDocumentBuilder.AddText(Const sText : String; oStyle : TWPStyle) : TWPDocumentText;
Begin
  Assert(CheckCondition(HasStarted, 'AddText', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisText In FScopeList.Current.Allowed, 'AddText', 'Unable to insert Text at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  If StringExists(sText, cReturn) Then
    RaiseError('AddText', StringFormat('Attempt to add a paragraph in text: %s', [sText]));

  Result := TWPDocumentText.Create;
  Try
    Result.Value := sText;

    If Assigned(oStyle) Then
      Result.Style := oStyle.Name;

    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link)
    Else
      TWPDocumentField(Current(TWPDocumentField)).Contents.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.AddText(Const sText : String; bBold, bItalics : Boolean) : TWPDocumentText;
Begin
  Result := AddText(sText, bBold, bItalics, 0);
End;


Procedure TWPDocumentBuilder.AddTextWithBreaks(Const sText : String; bStyled : Boolean);
Var
  oIter : TWPLineIterator;
  sValue : String;
Begin
  Assert(CheckCondition(HasStarted, 'AddTextWithBreaks', 'Must call Start before using the document builder'));

  oIter := TWPLineIterator.Create;
  Try
    oIter.Init(sText);
    While oIter.More Do
    Begin
      sValue := oIter.Next;
      If (sValue = #10) Or (sValue = #13) Then
        AddLineBreak
      Else If bStyled Then
        AddTextStyledByContext(sValue)
      Else
        AddTextPlain(sValue);
    End
  Finally
    oIter.Free;
  End;
End;

Function TWPDocumentBuilder.AddText(Const sText : String; bBold, bItalics : Boolean; iSize : Integer) : TWPDocumentText;
Begin
  Assert(CheckCondition(HasStarted, 'AddText', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisText In FScopeList.Current.Allowed, 'AddText', 'Unable to insert Text at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  If StringExists(sText, cReturn) Then
    RaiseError('AddText', 'Attempt to add a paragraph in text');

  Result := TWPDocumentText.Create;
  Try
    Result.Value := sText;

    If bBold Then
      Result.Font.Bold := tsTrue;

    If bItalics Then
      Result.Font.Italic := tsTrue;

    If iSize <> 0 Then
      Result.Font.Size := iSize;

    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link)
    Else
      TWPDocumentField(Current(TWPDocumentField)).Contents.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.ImportText(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String) : Boolean;
Var
  oDocument : TWPWorkingDocument;
  oReader : TWPTextReader;
  oStringStream : TFslStringStream;
Begin
  oDocument := TWPWorkingDocument.Create;
  oStringStream := TFslStringStream.Create;
  oReader := TWPTextReader.Create;
  Try
{$IFDEF VER130}
    oStringStream.Data := sContent;
{$ELSE}
    oStringStream.Bytes := TEncoding.UTF8.GetBytes(sContent);
{$ENDIF}

    oReader.Stream := oStringStream.Link;
    oReader.Styles := Document.Styles.Link;
    oReader.ExtendExisting := True;
    oReader.Read(oDocument);

    Result := ImportDocument(aOptions, oDocument);
  Finally
    oReader.Free;
    oStringStream.Free;
    oDocument.Free;
  End;
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oDocument : TWPWorkingDocument) : Boolean;
Var
  oTrans : TWPDocumentTranslator;
Begin
  oTrans := TWPDocumentTranslator.Create;
  Try
    oTrans.Document := TWPDocument.Create;
    oTrans.WorkingDocument := oDocument.Link;
    oTrans.WorkingStyles := oTrans.Document.Styles.Link;
    oTrans.TranslateToDocument;
    Result := ImportDocument(aOptions, oTrans.Document);
  Finally
    oTrans.Free;
  End;
End;


Procedure TWPDocumentBuilder.AddTableContents(oTable : TWPDocumentTable; oBlocks : TWPDocumentObjects);
Var
  iRow : Integer;
  oRow : TWPDocumentTableRow;
  iCell : Integer;
  oCell : TWPDocumentTableCell;
  iPara : Integer;
Begin
  For iRow := 0 To oTable.Rows.Count - 1 Do
  Begin
    oRow := oTable.Rows[iRow];
    For iCell := 0 To oRow.Cells.Count - 1 Do
    Begin
      oCell := oRow.Cells[iCell];
      For iPara := 0 To oCell.Paragraphs.Count - 1 Do
        oBlocks.Add(oCell.Paragraphs[iPara].Clone);
    End;
  End;
End;


Procedure TWPDocumentBuilder.AddSectionContents(oSection : TWPDocumentSection; oBlocks : TWPDocumentObjects; bInCell : Boolean; aOptions : TWPDocumentBuilderImportOptions);
Var
  iLoop : Integer;
  oBlock : TWPDocumentBlock;
Begin
  For iLoop := 0 To oSection.Blocks.Count - 1 Do
  Begin
    oBlock := oSection.Blocks[iLoop];
    If (oBlock Is TWPDocumentTable) And (bInCell Or (ioSuppressTables In aOptions)) Then
      AddTableContents(TWPDocumentTable(oBlock), oBlocks)
    Else If (oBlock Is TWPDocumentSection) Then
      AddSectionContents(TWPDocumentSection(oBlock), oBlocks, bInCell, aOptions)
    Else
      oBlocks.Add(oBlock.Clone);
  End;
End;


Procedure TWPDocumentBuilder.ImportDocumentStyles(oDocument : TWPDocument);
Var
  iLoop : Integer;
  oStyle : TWPStyle;
Begin
  // the master document styles override the imported Document's Styles.
  For iLoop := 0 To oDocument.Styles.Count - 1 Do
  Begin
    oStyle := oDocument.Styles[iLoop];
    If Not Document.Styles.ExistsByName(oStyle.Name) Then
      Document.Styles.Add(oStyle.Clone);
  End;
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oDocument : TWPDocument) : Boolean;
Var
  iLoop : Integer;
  oCurrent : TWPDocumentObject;
  oBlocks : TWPDocumentObjects;
  bInCell : Boolean;
  oBlock : TWPDocumentBlock;
  iCount : Integer;
  bPara : Boolean;
  sStyle : String;
Begin
  oCurrent := FScopeList.Current.Object_;
  oBlocks := Nil;
  bInCell := False;
  bPara := False;

  If (oCurrent Is TWPDocumentParagraph) And (oDocument.Blocks.Count = 1) And (oDocument.Blocks[0] Is TWPDocumentParagraph) Then
  Begin
    ImportDocumentStyles(oDocument);

    TWPDocumentParagraph(oCurrent).Contents.AddAllCloned(TWPDocumentParagraph(oDocument.Blocks[0]).Contents);

    Result := TWPDocumentParagraph(oDocument.Blocks[0]).Contents.Count > 0;
  End
  Else
  Begin
    If (oCurrent Is TWPDocumentParagraph) Then
    Begin
      bPara := True;
      sStyle := TWPDocumentParagraph(oCurrent).Style;
      EndParagraph(True);
      oCurrent := FScopeList.Current.Object_;
    End;

    If (oCurrent Is TWPDocumentContainer) Then
    Begin
      oBlocks := TWPDocumentContainer(oCurrent).Blocks;
    End
    Else If oCurrent Is TWPDocumentTableCell Then
    Begin
      oBlocks := TWPDocumentTableCell(oCurrent).Paragraphs;
      bInCell := True;
    End
    Else
    Begin
      RaiseError('ImportDocument', 'Cannot import a document at this point ('+oCurrent.ClassName+')');
    End;

    iCount := oBlocks.Count;

    For iLoop := 0 To oDocument.Blocks.Count - 1 Do
    Begin
      oBlock := oDocument.Blocks[iLoop];

      If (oBlock Is TWPDocumentTable) And bInCell And (ioTableExceptions In aOptions) Then
        RaiseError('ImportDocument', 'Cannot import a document containing Tables into a table cell (no nested tables)');

      If (oBlock Is TWPDocumentSection) And bInCell And (ioTableExceptions In aOptions) Then
        RaiseError('ImportDocument', 'Cannot import a document containing Sections into a table cell (no sections in tables)');

      If (oBlock Is TWPDocumentBreak) And (TWPDocumentBreak(oBlock).BreakType = BreakTypePageBreak) And bInCell And (ioTableExceptions In aOptions) Then
        RaiseError('ImportDocument', 'Cannot import a document containing Sections into a table cell (no sections in tables)');
    End;

    ImportDocumentStyles(oDocument);

    For iLoop := 0 To oDocument.Blocks.Count - 1 Do
    Begin
      oBlock := oDocument.Blocks[iLoop];

      If (oBlock Is TWPDocumentTable) And (bInCell Or (ioSuppressTables In aOptions)) Then
        AddTableContents(TWPDocumentTable(oBlock), oBlocks)
      Else If (oBlock Is TWPDocumentSection) And (bInCell Or (ioSuppressSections In aOptions)) Then
        AddSectionContents(TWPDocumentSection(oBlock), oBlocks, bInCell, aOptions)
      Else If Not bInCell Or Not ((oBlock Is TWPDocumentBreak) And (TWPDocumentBreak(oBlock).BreakType = BreakTypePageBreak)) Then
        oBlocks.Add(oBlock.Clone);
    End;

    Result := iCount <> oBlocks.Count;

    If bPara Then
      StartParagraph(Document.Styles.GetByName(sStyle));
  End;
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oStream : TFslStream; aImageLoader : TWPLoadImageEvent) : Boolean;
Begin
  Result := ImportDocument(aOptions, oStream, aImageLoader, False);
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; oStream : TFslStream; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean;
Var
  oDocument : TWPWorkingDocument;
  oReader : TWPReader;
Begin
  oDocument := TWPWorkingDocument.Create;
  Try
    If bIsText Then
      oReader := TWPTextReader.Create
    Else
      oReader := TWPNativeReader.Create;
    Try
      oReader.Stream := oStream.Link;
      oReader.Styles := Document.Styles.Link;
      oReader.ExtendExisting := True;
      oReader.OnLoadImage := aImageLoader;
      oReader.Read(oDocument);

      Result := ImportDocument(aOptions, oDocument);
    Finally
      oReader.Free;
    End;
  Finally
    oDocument.Free;
  End;
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : AnsiString; aImageLoader : TWPLoadImageEvent) : Boolean;
Begin
  Result := ImportDocument(aOptions, sContent, aImageLoader, False);
End;

{$IFNDEF VER130}
Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String; aImageLoader : TWPLoadImageEvent = Nil) : Boolean;
Begin
  Result := ImportDocument(aOptions, sContent, aImageLoader, False);
End;


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : String; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean;
Var
  oSource : TFslMemoryStream;
Begin
  Result := True;

  If sContent <> '' Then
  Begin
    oSource := TFslMemoryStream.Create;
    Try
      oSource.AsText := sContent;

      Result := ImportDocument(aOptions, oSource, aImageLoader, bIsText);
    Finally
      oSource.Free;
    End;
  End;
End;
{$ENDIF}


Function TWPDocumentBuilder.ImportDocument(aOptions : TWPDocumentBuilderImportOptions; Const sContent : AnsiString; aImageLoader : TWPLoadImageEvent; Const bIsText : Boolean) : Boolean;
Var
  oSource : TFslStringStream;
Begin
  If sContent <> '' Then
  Begin
    oSource := TFslStringStream.Create;
    Try
      oSource.Data := sContent;

      Result := ImportDocument(aOptions, oSource, aImageLoader, bIsText);
    Finally
      oSource.Free;
    End;
  End
  Else
    Result := False;
End;


Function TWPDocumentBuilder.AddImage(Const sFilename : String; oStyle : TWPStyle = Nil): TWPDocumentImage;
Begin
  Result := AddImage(sFilename, '', oStyle);
End;


Function TWPDocumentBuilder.AddImage(Const sFilename, sSelectionFilename : String; oStyle : TWPStyle = Nil): TWPDocumentImage;
Var
  oLoader : TWPImageLoader;
  oImage : TFslGraphic;
  oSelectionImage : TFslGraphic;
Begin
  oLoader := TWPImageLoader.Create;
  Try
    oLoader.DicomDictionary := FDicomDictionary.Link;
    oImage := Nil;
    Try
      oSelectionImage := Nil;
      Try
        If (sSelectionFilename <> '') Then
        Begin
          oLoader.Reset;
          oLoader.Filename := sSelectionFilename;
          oSelectionImage := oLoader.Load;
        End;
        oLoader.Reset;
        oLoader.Filename := sFilename;
        oImage := oLoader.Load;
        Result := AddImage(oImage, oSelectionImage, oStyle);
        Result.Name := PathTitle(sFilename);
      Finally
        oSelectionImage.Free;
      End;
    Finally
      oImage.Free;
    End;
  Finally
    oLoader.Free;
  End;
End;


Function TWPDocumentBuilder.AddImage(oImage : TFslGraphic; oStyle : TWPStyle = Nil): TWPDocumentImage;
Begin
  Result := AddImage(oImage, Nil, oStyle);
End;


Function TWPDocumentBuilder.AddImage(oImage, oSelectionImage: TFslGraphic; oStyle : TWPStyle = Nil): TWPDocumentImage;
Begin
  Assert(CheckCondition(HasStarted, 'AddImage', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisImage In FScopeList.Current.Allowed, 'AddImage', 'Unable to insert an Image at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));
  Assert(CheckCondition(Assigned(oImage), 'AddImage', 'oImage is not assigned'));

  Result := TWPImageLoader.Wrap(oImage);
  Try
    Result.SelectionImage := oSelectionImage.Link;
    If Assigned(oStyle) Then
      Result.Style := oStyle.Name;
    If FScopeList.Current.Object_ Is TWPDocumentParagraph Then
      TWPDocumentParagraph(Current(TWPDocumentParagraph)).Contents.Add(Result.Link)
    Else
      TWPDocumentField(Current(TWPDocumentField)).Contents.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.AddLine(rWidth : Real = 1) : TWPDocumentBreak;
Begin
  Result := AddLine(rWidth, clBlack);
End;


Function TWPDocumentBuilder.AddLine(aColour : TColour; iWidth : Integer = 1) : TWPDocumentBreak;
Begin
  Result := AddLine(1, aColour, iWidth);
End;


Function TWPDocumentBuilder.AddLine(rWidth : Real; aColour : TColour; iWidth : Integer = 1) : TWPDocumentBreak;
Begin
  Assert(CheckCondition(HasStarted, 'AddLine', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisBreak In FScopeList.Current.Allowed, 'AddLine', 'Unable to insert a Line at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentBreak.Create;
  Try
    Result.BreakType := BreakTypeLine;
    Result.Width := rWidth;
    Result.PenColour := aColour;
    Result.PenWidth := iWidth;
    Result.PenStyle := apsSolid;

    TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.AddPageBreak : TWPDocumentBreak;
Begin
  Assert(CheckCondition(HasStarted, 'AddPageBreak', 'Must call Start before using the document builder'));
  Assert(CheckCondition(aisBreak In FScopeList.Current.Allowed, 'AddPageBreak', 'Unable to insert a page break at this point in the document ('+Summarise(FScopeList.Current.Allowed)+')'));

  Result := TWPDocumentBreak.Create;
  Try
    Result.BreakType := BreakTypePageBreak;

    TWPDocumentContainer(Current(TWPDocumentContainer)).Blocks.Add(Result.Link);
  Finally
    Result.Free;
  End;
End;


Function TWPDocumentBuilder.ImportBuffer(aOptions : TWPDocumentBuilderImportOptions; Const oBuffer: TFslBuffer): Boolean;
Begin
  Assert(Invariants('ImportBuffer', oBuffer, TFslBuffer, 'oBuffer'));

  If oBuffer.StartsWith('<WordProcessor') Then
    Result := ImportDocument(aOptions, oBuffer.AsAscii)
  Else
{$IFDEF VER130}
    Result := ImportText(aOptions, oBuffer.AsAscii);
{$ELSE}
    Result := ImportText(aOptions, oBuffer.AsText);
{$ENDIF}
End;


Function TWPDocumentBuilder.AddTableCell(Const sText : String; bBold : Boolean = False; bItalics : Boolean = False; iSpan : Integer = 1) : TWPDocumentTableCell;
Begin
  Result := StartTableCell;
  StartParagraph;
  AddText(sText, bBold, bItalics);
  EndParagraph;
  EndTableCell;
End;


Function TWPDocumentBuilder.AddTableCell(Const sText : String; oStyle: TWPStyle) : TWPDocumentTableCell;
Begin
  Result := StartTableCell;
  AddParagraph(sText, oStyle);
  EndTableCell;
End;


Function TWPDocumentBuilder.AddTableCell(Const sText, sStyle: String) : TWPDocumentTableCell;
Begin
  Result := StartTableCell;
  StartParagraph;
  AddText(sText, sStyle);
  EndParagraph;
  EndTableCell;
End;


Function TWPDocumentBuilder.DefineHotspot(Const sURL: String) : TWPHotspot;
Begin
  Result := DefineHotspot(sURL, DEF_COLOUR, DEF_COLOUR);
End;


Function TWPDocumentBuilder.DefineHotspot(Const sURL: String; Const iLink, iHover: TColour) : TWPHotspot;
Begin
  TWPDocumentObject(Current(TWPDocumentObject)).HasHotspot := True;
  Result := TWPDocumentObject(Current(TWPDocumentObject)).Hotspot;
  Result.URL := sURL;
  Result.LinkColour := iLink;
  Result.HoverColour := iHover;
End;


Function TWPDocumentBuilder.Summarise(aItems :TWPDocumentBuilderScopeAllowedItems): String;
Var
  aLoop : TWPDocumentBuilderScopeAllowedItem;
Begin
  Result := '';
  For aLoop := Low(TWPDocumentBuilderScopeAllowedItem) To High(TWPDocumentBuilderScopeAllowedItem) Do
  Begin
    If Result <> '' Then
      Result := Result + ', ';
    Result := Result + TWPDOCUMENTBUILDERSCOPE_ALLOWED_VALUE[aLoop];
  End;
End;


Function TWPDocumentBuilder.ReplaceField(Const sNamespace, sName, sContent: String): Boolean;
Begin
  Result := ReplaceField([sNamespace], sName, sContent);
End;


Function TWPDocumentBuilder.ReplaceField(Const aNamespaces: Array Of String; sName, sContent: String): Boolean;
Begin
  Result := False;
  Home;
  FirstField;
  While More Do
  Begin
    If StringArrayExistsSensitive(aNamespaces, CurrentField.Namespace) And (CurrentField.Name = sName) Then
    Begin
      Start;
      ClearChildren;
      AddTextPlain(sContent);
      Stop;
      Result := True;
    End;
    NextField;
  End;
End;


Function TWPDocumentBuilder.ContextInField: Boolean;
Begin
  Result := HasStarted And (Current(TWPDocumentObject) Is TWPDocumentField);
End;

Procedure TWPDocumentBuilder.ProcessFields;
Var
  oStack : TFslStringList;
Begin
  oStack := TFslStringList.Create;
  Try
    ProcessFields(oStack, FDocument);
  Finally
    oStack.Free;
  End;
End;

Procedure TWPDocumentBuilder.ProcessFields(oTable: TWPDocumentTable);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oTable.Rows.Count - 1 Do
    ProcessFields(oTable.Rows[iLoop]);
End;

Procedure TWPDocumentBuilder.ProcessFields(oRow: TWPDocumentTableRow);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oRow.Cells.Count - 1 Do
    ProcessFields(oRow.Cells[iLoop]);
  For iLoop := 0 To oRow.Rows.Count - 1 Do
    ProcessFields(oRow.Rows[iLoop]);
End;

Procedure TWPDocumentBuilder.ProcessFields(oCell: TWPDocumentTableCell);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oCell.Paragraphs.Count - 1 Do
    ProcessFields(oCell.Paragraphs[iLoop]);
End;

Function TWPDocumentBuilder.ProcessSection(oStack : TFslStringList; oSection: TWPDocumentSection; oList : TWPDocumentBlocks; iIndex : Integer) : Integer;
Var
  oProvider : TWPFieldValueProvider;
  aAction : TFieldAction;
  bAdded : Boolean;
  iLoop : Integer;
Begin
  Result := 0;
  bAdded := False;
  aAction := FieldActionLeave;
  If (oSection.isField) Then
  Begin
    If oStack.ExistsByValue(oSection.NamePair) Then
      RaiseError('ProcessSection', 'Encountered recursive use of field '+oSection.NamePair+' in '+oStack.asCSV);
    oProvider := FFieldValueProviders.GetByName(oSection.Namespace);
    If (oProvider <> Nil) Then
    Begin
      aAction := oProvider.GetAction(oSection);
      If (aAction In [FieldActionDelete, FieldActionReplaceContent, FieldActionReplaceAll]) Then
        oSection.Blocks.Clear;
      If (aAction In [FieldActionReplaceContent, FieldActionReplaceAll]) Then
      Begin
        bAdded := True;
        oStack.Add(oSection.NamePair);
        FFocus := oSection;
        Start;
        oProvider.Replace(oSection, Self);
        Stop;
      End;
    End;
  End;
  ProcessFields(oStack, oSection);
  If bAdded Then
  Begin
    oStack.DeleteByIndex(oStack.Count - 1);
    If aAction = FieldActionReplaceAll Then
    Begin
      Result := oSection.Blocks.Count - 1; // may be -1, this is an acceptable value
      For iLoop := 0 To oSection.Blocks.Count - 1 Do
        oList.Insert(iLoop + iIndex+1, oSection.Blocks[iLoop].Link);
      oList.DeleteByIndex(iIndex); // bye bye section
    End;
  End;
End;

Procedure TWPDocumentBuilder.ProcessFields(oStack : TFslStringList; oContainer : TWPDocumentContainer);
Var
  iLoop : Integer;
  oBlock : TWPDocumentBlock;
Begin
  iLoop := 0;
  While iLoop < oContainer.Blocks.Count Do
  Begin
    oBlock := oContainer.Blocks[iLoop];
    If oBlock Is TWPDocumentParagraph Then
      ProcessFields(TWPDocumentParagraph(oBlock))
    Else If oBlock Is TWPDocumentTable Then
      ProcessFields(TWPDocumentTable(oBlock))
    Else If oBlock Is TWPDocumentSection Then
      Inc(iLoop, ProcessSection(oStack, TWPDocumentSection(oBlock), oContainer.Blocks, iLoop));
//     Else we just don't care...
    Inc(iLoop);
  End;
End;

Procedure TWPDocumentBuilder.ProcessFields(oParagraph: TWPDocumentParagraph);
Var
  iLoop : Integer;
  oContent : TWPDocumentContent;
Begin
  iLoop := 0;
  While iLoop < oParagraph.Contents.Count Do
  Begin
    oContent := oParagraph.Contents[iLoop];
    If oContent Is TWPDocumentField Then
      Inc(iLoop, ProcessField(TWPDocumentField(oContent), oParagraph.Contents, iLoop));
    // Else we don't care
    Inc(iLoop);
  End;
End;


Function TWPDocumentBuilder.ProcessField(oField : TWPDocumentField; oList : TWPDocumentContents; iIndex : Integer) : Integer;
Var
  oProvider : TWPFieldValueProvider;
  aAction : TFieldAction;
  oPara : TWPDocumentParagraph;
  iLoop : Integer;
Begin
  Result := 0;
  oProvider := FFieldValueProviders.GetByName(oField.Namespace);
  If (oProvider <> Nil) Then
  Begin
    aAction := oProvider.GetAction(oField);
    If (aAction In [FieldActionDelete, FieldActionReplaceContent, FieldActionReplaceAll]) Then
      oField.Contents.Clear;
    If (aAction = FieldActionReplaceContent) Then
    Begin
      FFocus := oField;
      Start;
      oProvider.Replace(oField, Self);
      Stop;
    End
    Else If aAction = FieldActionReplaceAll Then
    Begin
      oPara := TWPDocumentParagraph.Create;
      Try
        FFocus := oPara;
        Start;
        oProvider.Replace(oField, Self);
        Stop;
        Result := oPara.Contents.Count - 1; // may be -1, this is an acceptable value
        For iLoop := 0 To oPara.Contents.Count - 1 Do
          oList.Insert(iLoop + iIndex + 1, oPara.Contents[iLoop].Link);
        oList.DeleteByIndex(iIndex); // bye bye field
      Finally
        FFocus.Free;
      End;
    End;
  End;
End;


function TWPDocumentBuilder.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDocument.sizeInBytes);
  inc(result, FScopeList.sizeInBytes);
  inc(result, FIterator.sizeInBytes);
  inc(result, FFocus.sizeInBytes);
  inc(result, FFieldValueProviders.sizeInBytes);
  inc(result, FDicomDictionary.sizeInBytes);
end;

{ TWPFieldValueProviderList }

Function TWPFieldValueProviderList.Clone: TWPFieldValueProviderList;
Begin
  Result := TWPFieldValueProviderList(Inherited Clone);
End;

Function TWPFieldValueProviderList.GetByName(Const sName: String): TWPFieldValueProvider;
Var
  iLoop : Integer;
Begin
  Result := Nil;
  iLoop := 0;
  While (iLoop < Count) And (Result = Nil) Do
  Begin
    If (Elements[iLoop].GetNamespace = sName) Then
      Result := Elements[iLoop];
    Inc(iLoop);
  End;
End;

Function TWPFieldValueProviderList.GetElement(Const iIndex: Integer): TWPFieldValueProvider;
Begin
  Result := TWPFieldValueProvider(ObjectByIndex[iIndex]);
End;

Function TWPFieldValueProviderList.ItemClass: TFslObjectClass;
Begin
  Result := TWPFieldValueProvider;
End;

Function TWPFieldValueProviderList.Link: TWPFieldValueProviderList;
Begin
  Result := TWPFieldValueProviderList(Inherited Link);
End;

Function TWPFieldValueProviderList.New: TWPFieldValueProvider;
Begin
  Result := TWPFieldValueProvider(Inherited New);
End;

Procedure TWPFieldValueProviderList.SetElement(Const iIndex: Integer; Const oValue: TWPFieldValueProvider);
Begin
  Inherited ObjectByIndex[iIndex] := oValue;
End;


{ TWPFieldValueProvider }

Function TWPFieldValueProvider.GetAction(oField: TWPDocumentSection): TFieldAction;
Begin
  Result := FieldActionLeave;

End;

Function TWPFieldValueProvider.GetAction(oField: TWPDocumentField): TFieldAction;
Begin
  Result := FieldActionLeave;
End;

Function TWPFieldValueProvider.GetNamespace: String;
Begin
  RaiseError('Namespace', 'Must be overriden');
End;


Procedure TWPFieldValueProvider.Replace(oField: TWPDocumentSection; oBuilder: TWPDocumentBuilder);
Begin
  // nothing
End;

Procedure TWPFieldValueProvider.Replace(oField: TWPDocumentField; oBuilder: TWPDocumentBuilder);
Begin
  // nothing
End;


Function TWPDocumentBuilder.URL(Const sText, sLink: String): TWPDocumentField;
Begin
  Result := StartField(NS_FIELD_HOTSPOT, 'url', True);
  DefineHotspot(sLink);
  AddTextStyledByContext(sText);
  EndField;
End;

End.
