Unit FHIR.WP.Menus;


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
  SysUtils, Classes, Menus, Dialogs,
  fsl_utilities,
  fui_vclx_base, fui_vclx_controls,
  wp_types, wp_clipboard;

{
The menu is broken into columns
* general
* table
* image
* insert

They appear in this order. The column only appears if any items are enabled
}

Type
  TWPPopupMenu = Class(TUixPopupMenu)
    Private
      mnuSpelling : TUixMenuItem;
      mnuSpellingSuggest : TUixMenuItem;
      mnuSpellSeparator1 : TUixMenuItem;
      mnuSpellingAllow : TUixMenuItem;
      mnuSpellingAdd : TUixMenuItem;
      mnuSpellingCheck : TUixMenuItem;
      mnuSpellingReview : TUixMenuItem;

      FActionSeparator : TUixMenuItem;
      FFormatSeparator : TUixMenuItem;

      mnuCut : TUixMenuItem;
      mnuCopy : TUixMenuItem;
      mnuPaste : TUixMenuItem;
      mnuPasteSpecial : TUixMenuItem;
      mnuFont : TUixMenuItem;
      mnuParagraph : TUixMenuItem;
      mnuLine : TUixMenuItem;

      mnuChangeCase: TUixMenuItem;

      mnuInsert : TUixMenuItem;
      mnuInsertTable : TUixMenuItem;
      mnuTableConvertToTable : TUixMenuItem;
      mnuInsertLine : TUixMenuItem;
      mnuInsertPageBreak : TUixMenuItem;
      mnuInsertImage : TUixMenuItem;
      mnuInsertPDF : TUixMenuItem;
      mnuInsertTemplate : TUixMenuItem;
      mnuInsertSymbol : TUixMenuItem;

      mnuTable : TUixMenuItem;
      mnuTableInsertCopiedRowAbove : TUixMenuItem;
      mnuTableInsertCopiedRowBelow : TUixMenuItem;
      mnuTableInsertRowAbove : TUixMenuItem;
      mnuTableInsertRowBelow : TUixMenuItem;
      mnuTableInsertColumnLeft : TUixMenuItem;
      mnuTableInsertColumnRight : TUixMenuItem;
      mnuTableMergeCells : TUixMenuItem;
      mnuTableSplitCell : TUixMenuItem;
      mnuTableSeparator1 : TUixMenuItem;
      mnuTableSelectRow : TUixMenuItem;
      mnuTableSelectTable : TUixMenuItem;
      mnuTableSeparator2 : TUixMenuItem;
      mnuTableDeleteTable : TUixMenuItem;
      mnuTableDeleteRow : TUixMenuItem;
      mnuTableDeleteColumn : TUixMenuItem;
      mnuTableSeparator3 : TUixMenuItem;
      mnuTableSort: TUixMenuItem;
      mnuTableDetails : TUixMenuItem;

      mnuImage : TUixMenuItem;
      mnuImageSelect : TUixMenuItem;
      mnuImageDrawLine : TUixMenuItem;
      mnuImageRectangle : TUixMenuItem;
      mnuImageCircle : TUixMenuItem;
      mnuImageMark : TUixMenuItem;
      mnuImageZoom : TUixMenuItem;
      mnuImageItem : TUixMenuItem;
      mnuImageToBack : TUixMenuItem;
      mnuImageToFront : TUixMenuItem;
      mnuImageDelete : TUixMenuItem;
      mnuImageSeparator : TUixMenuItem;
      mnuImageMap : TUixMenuItem;
      mnuImageProperties : TUixMenuItem;

      mnuField : TUixMenuItem;
      mnuFieldNext : TUixMenuItem;
      mnuFieldPrevious : TUixMenuItem;
      mnuFieldEdit : TUixMenuItem;
      mnuFieldEditSection : TUixMenuItem;
      mnuFieldRemove : TUixMenuItem;
      mnuFieldSectionRemove : TUixMenuItem;

      mnuAnnotation : TUixMenuItem;
      mnuAnnotationEdit : TUixMenuItem;
      mnuAnnotationDelete : TUixMenuItem;

      mnuSupport : TUixMenuItem;
      mnuSptSnapshot : TUixMenuItem;
      mnuSptInspector : TUixMenuItem;

      FMouseInfo : TWPMouseInfo;

      Function MakeSeparator(Const sHint : String; Var oValue : TUixMenuItem) : TUixMenuItem;
      Function MakeMenu(Const sCaption : String; aEvent : TNotifyEvent; iImage : Integer; Var oValue : TUixMenuItem) : TUixMenuItem;
      Procedure LoadInsertFieldMenu;

      Procedure Refresh(oSender : TObject);
      Procedure LoadClipboardFormats;
      Procedure ShowClipboardFormats;

      // events
      Procedure SetFont(oSender : TObject);
      Procedure SetLine(oSender : TObject);
      Procedure SetPara(oSender : TObject);
      Procedure DoCut(oSender : TObject);
      Procedure DoCopy(oSender : TObject);
      Procedure DoPaste(oSender : TObject);
      Procedure DoPasteSpecial(oSender : TObject);

      Procedure DoSymbolInsert(oSender : TObject);
      Procedure DoAnnotationInsert(oSender : TObject);
      Procedure DoAnnotationEdit(oSender : TObject);
      Procedure DoAnnotationDelete(oSender : TObject);

      Procedure DoImageInsert(oSender : TObject);
      Procedure DoPDFInsert(oSender : TObject);
      Procedure DoImageProps(oSender : TObject);
      Procedure DoImageMaps(oSender : TObject);
      Procedure DoImageSelect(oSender : TObject);
      Procedure DoImageLine(oSender : TObject);
      Procedure DoImageRectangle(oSender : TObject);
      Procedure DoImageCircle(oSender : TObject);
      Procedure DoImageMark(oSender : TObject);
      Procedure DoImageZoom(oSender : TObject);
      Procedure DoImageToBack(oSender : TObject);
      Procedure DoImageToFront(oSender : TObject);
      Procedure DoImageDelete(oSender : TObject);

      Procedure DoTemplateInsert(oSender : TObject);
      Procedure DoFieldInsert(oSender : TObject);
      Procedure DoNextField(oSender : TObject);
      Procedure DoPreviousField(oSender : TObject);
      Procedure DoFieldProps(oSender : TObject);
      Procedure DoFieldSectionProps(oSender : TObject);
      Procedure DoRemoveField(oSender : TObject);
      Procedure DoRemoveFieldSection(oSender : TObject);

      Procedure DoSpellSuggest(oSender : TObject);
      Procedure DoSpellAllow(oSender : TObject);
      Procedure DoSpellAdd(oSender : TObject);
      Procedure DoSpellCheck(oSender : TObject);
      Procedure DoReviewAllowed(oSender : TObject);

      Procedure DoDbgSnapshot(oSender : TObject);
      Procedure DoDbgInspector(oSender : TObject);

      Procedure DoInsertTable(oSender : TObject);
      Procedure DoInsertLine(oSender : TObject);
      Procedure DoInsertPageBreak(oSender : TObject);

      Procedure DoConvertToTable(oSender : TObject);
      Procedure DoInsertCopiedRowAbove(oSender : TObject);
      Procedure DoInsertCopiedRowBelow(oSender : TObject);
      Procedure DoInsertRowAbove(oSender : TObject);
      Procedure DoInsertRowBelow(oSender : TObject);
      Procedure DoInsertColumnLeft(oSender : TObject);
      Procedure DoInsertColumnRight(oSender : TObject);
      Procedure DoMergeCells(oSender : TObject);
      Procedure DoSplitCell(oSender : TObject);
      Procedure DoSelectRow(oSender : TObject);
      Procedure DoSelectTable(oSender : TObject);
      Procedure DoDeleteTable(oSender : TObject);
      Procedure DoDeleteRow(oSender : TObject);
      Procedure DoDeleteColumn(oSender : TObject);
      Procedure DoSortTableRow(oSender: TObject);
      Procedure DoTableProperties(oSender : TObject);

      Procedure MakeChangeCaseMenu;
      Procedure DoChangeCase(oSender: TObject);
      Procedure SetMouseInfo(Const Value: TWPMouseInfo);

    Protected
      Procedure Initialise; Override;
      Procedure Finalise; Override;
    Public
      Property SupportMenu : TUixMenuItem Read mnuSupport;
      Property MouseInfo : TWPMouseInfo Read FMouseInfo Write SetMouseInfo;
  End;


Implementation


Uses
  fsl_collections,

  FHIR.WP.Icons, FHIR.WP.Control, wp_definers, FHIR.WP.Engine, wp_working;


Procedure TWPPopupMenu.Initialise;
Begin
  Inherited;

  Images := WPIconModule.Images;

  Items.Add(MakeSeparator('Actions', FActionSeparator));

  // Spelling.
  Items.Add(MakeMenu('&Spelling', Nil, WPIconModule.SPELLING, mnuSpelling));
  mnuSpelling.Add(MakeMenu('Suggestions...', DoSpellSuggest, WPIconModule.NONE, mnuSpellingSuggest));
  mnuSpelling.Add(MakeSeparator('', mnuSpellSeparator1));
  mnuSpelling.Add(MakeMenu('A&llow (for this Document)', DoSpellAllow, WPIconModule.NONE, mnuSpellingAllow));
  mnuSpelling.Add(MakeMenu('&Add (to System Dictionary)', DoSpellAdd, WPIconModule.NONE, mnuSpellingAdd));
  mnuSpelling.Add(MakeMenu('&Check', DoSpellCheck, WPIconModule.SPELLING, mnuSpellingCheck));
  mnuSpelling.Add(MakeMenu('&Review Words Allowed for this Document', DoReviewAllowed, WPIconModule.NONE, mnuSpellingReview));

  // Actions.
  Items.Add(MakeMenu('Cu&t', DoCut, WPIconModule.CUT, mnuCut));
  Items.Add(MakeMenu('&Copy', DoCopy, WPIconModule.Copy, mnuCopy));
  Items.Add(MakeMenu('&Paste', DoPaste, WPIconModule.PASTE, mnuPaste));
  Items.Add(MakeMenu('Paste &Special', Nil, WPIconModule.NONE, mnuPasteSpecial));
  LoadClipboardFormats;

  Items.Add(MakeMenu('C&hange Case', Nil, WPIconModule.CHANGE_CASE, mnuChangeCase));
  MakeChangeCaseMenu;

  // Format.
  Items.Add(MakeSeparator('Format', FFormatSeparator));
  Items.Add(MakeMenu('Font', SetFont, WPIconModule.FORMAT_FONT, mnuFont));
  Items.Add(MakeMenu('Paragraph', SetPara, WPIconModule.FORMAT_PARA, mnuParagraph));
  Items.Add(MakeMenu('Line Properties', SetLine, WPIconModule.INSERT_LINE, mnuLine));


  // Field.
  Items.Add(MakeMenu('&Field', Nil, WPIconModule.NONE, mnuField));
  mnuField.Add(MakeMenu('Next', DoNextField, WPIconModule.FIELD_NEXT, mnuFieldNext));
  mnuField.Add(MakeMenu('Previous', DoPreviousField, WPIconModule.FIELD_PREVIOUS, mnuFieldPrevious));
  mnuField.Add(MakeMenu('Edit', DoFieldProps, WPIconModule.OBJECT_PROPERTIES, mnuFieldEdit));
  mnuField.Add(MakeMenu('Edit', DoFieldSectionProps, WPIconModule.OBJECT_PROPERTIES, mnuFieldEditSection));
  mnuField.Add(MakeMenu('Remove', DoRemoveField, WPIconModule.FIELD_DELETE, mnuFieldRemove));
  mnuField.Add(MakeMenu('Remove', DoRemoveFieldSection, WPIconModule.FIELD_DELETE, mnuFieldSectionRemove));
  // Comment
  Items.Add(MakeMenu('&Annotation', Nil, WPIconModule.NONE, mnuAnnotation));
  mnuAnnotation.Add(MakeMenu('Edit', DoAnnotationEdit, WPIconModule.OBJECT_PROPERTIES, mnuAnnotationEdit));
  mnuAnnotation.Add(MakeMenu('Delete', DoAnnotationDelete, WPIconModule.FIELD_DELETE, mnuAnnotationDelete));

  // Support Tools
  Items.Add(MakeMenu('Support Tools', Nil, -1, mnuSupport));
  mnuSupport.Add(MakeMenu('Snapshot', DoDbgSnapshot, -1, mnuSptSnapshot));
  mnuSupport.Add(MakeMenu('Inspector', DoDbgInspector, -1, mnuSptInspector));

  // Table.
  Items.Add(MakeSeparator('Table', mnuTable));
  mnuTable.Break := mbBarBreak;

  //Items.Add(MakeMenu('Paste as Row Above', DoInsertCopiedRowAbove, WPIconModule.COPIED_ROW_ABOVE, mnuTableInsertCopiedRowAbove));
  //Items.Add(MakeMenu('Paste as Row Below', DoInsertCopiedRowBelow, WPIconModule.COPIED_ROW_BELOW, mnuTableInsertCopiedRowBelow));
  Items.Add(MakeMenu('Insert Row Above', DoInsertRowAbove, WPIconModule.ROW_ABOVE, mnuTableInsertRowAbove));
  Items.Add(MakeMenu('Insert Row Below', DoInsertRowBelow, WPIconModule.ROW_BELOW, mnuTableInsertRowBelow));
  Items.Add(MakeMenu('Insert Column Left', DoInsertColumnLeft, WPIconModule.COLUMN_LEFT, mnuTableInsertColumnLeft));
  Items.Add(MakeMenu('Insert Column Right', DoInsertColumnRight, WPIconModule.COLUMN_RIGHT, mnuTableInsertColumnRight));
  Items.Add(MakeMenu('Merge With Cell To Right', DoMergeCells, WPIconModule.COLUMN_RIGHT, mnuTableMergeCells));
  Items.Add(MakeMenu('Split Cell', DoSplitCell, WPIconModule.COLUMN_RIGHT, mnuTableSplitCell));
  Items.Add(MakeSeparator('', mnuTableSeparator1));
  Items.Add(MakeMenu('Select Row', DoSelectRow, WPIconModule.SELECT_ROW, mnuTableSelectRow));
  Items.Add(MakeMenu('Select Table', DoSelectTable, WPIconModule.SELECT_TABLE, mnuTableSelectTable));
  Items.Add(MakeSeparator('', mnuTableSeparator2));
  Items.Add(MakeMenu('Delete Table', DoDeleteTable, WPIconModule.DELETE_TABLE, mnuTableDeleteTable));
  Items.Add(MakeMenu('Delete Row', DoDeleteRow, WPIconModule.DELETE_ROW, mnuTableDeleteRow));
  Items.Add(MakeMenu('Delete Column', DoDeleteColumn, WPIconModule.DELETE_COLUMN, mnuTableDeleteColumn));
  Items.Add(MakeSeparator('', mnuTableSeparator3));
  Items.Add(MakeMenu('Sort Row', DoSortTableRow, WPIconModule.SORT_TABLE, mnuTableSort));
  Items.Add(MakeMenu('Table/Row/Cell Properties', DoTableProperties, WPIconModule.OBJECT_PROPERTIES, mnuTableDetails));

  // Image.
  Items.Add(MakeSeparator('Image', mnuImage));
  mnuImage.Break := mbBarBreak;

  Items.Add(MakeMenu('Select', DoImageSelect, WPIconModule.IMAGE_SELECT, mnuImageSelect));  // crIBeam / crArrow / crHandPoint etc
  Items.Add(MakeMenu('Line', DoImageLine, WPIconModule.IMAGE_PEN, mnuImageDrawLine));
  Items.Add(MakeMenu('Rectangle', DoImageRectangle, WPIconModule.IMAGE_BOX, mnuImageRectangle));
  Items.Add(MakeMenu('Circle', DoImageCircle, WPIconModule.IMAGE_CIRCLE, mnuImageCircle));
  Items.Add(MakeMenu('Mark', DoImageMark, WPIconModule.IMAGE_MARK, mnuImageMark));
  Items.Add(MakeMenu('Zoom', DoImageZoom, WPIconModule.IMAGE_ZOOM, mnuImageZoom));

  Items.Add(MakeSeparator('Item', mnuImageItem));
  Items.Add(MakeMenu('To Back', DoImageToBack, WPIconModule.IMAGE_PEN, mnuImageToBack));
  Items.Add(MakeMenu('To Front', DoImageToFront, WPIconModule.IMAGE_PEN, mnuImageToFront));
  Items.Add(MakeMenu('Delete', DoImageDelete, WPIconModule.IMAGE_PEN, mnuImageDelete));
  Items.Add(MakeSeparator('', mnuImageSeparator));
  Items.Add(MakeMenu('Image Map Editor', DoImageMaps, WPIconModule.OBJECT_PROPERTIES, mnuImageMap));
  Items.Add(MakeMenu('Properties', DoImageProps, WPIconModule.OBJECT_PROPERTIES, mnuImageProperties));

  mnuImageSelect.Checked := True;
  mnuImageSelect.ImageIndex := WPIconModule.IMAGE_SELECT_ON;

  // Insert.
  Items.Add(MakeSeparator('Insert', mnuInsert));
  mnuInsert.Break := mbBarBreak;

  Items.Add(MakeMenu('&Table', DoInsertTable, WPIconModule.INSERT_TABLE, mnuInsertTable));
  Items.Add(MakeMenu('&Convert Text To Table', DoConvertToTable, WPIconModule.CONVERTTOTABLE, mnuTableConvertToTable));
  Items.Add(MakeMenu('&Line', DoInsertLine, WPIconModule.INSERT_LINE, mnuInsertLine));
  Items.Add(MakeMenu('&Page Break', DoInsertPageBreak, WPIconModule.INSERT_PAGEBREAK, mnuInsertPageBreak));
  Items.Add(MakeMenu('&Image', DoImageInsert, WPIconModule.INSERT_IMAGE, mnuInsertImage));
  Items.Add(MakeMenu('P&DF', DoPDFInsert, WPIconModule.INSERT_PDF, mnuInsertPDF));
  Items.Add(MakeMenu('T&emplate', DoTemplateInsert, WPIconModule.INSERT_TEMPLATE, mnuInsertTemplate));
  {$IFDEF UNICODE}
  Items.Add(MakeMenu('&Unicode Character', DoSymbolInsert, WPIconModule.INSERT_SYMBOL, mnuInsertSymbol));
  {$ELSE}
  Items.Add(MakeMenu('&Symbol', DoSymbolInsert, WPIconModule.INSERT_SYMBOL, mnuInsertSymbol));
  {$ENDIF}


//  mnuInsert.Add(MakeSeparator('', mnuInsertSeparator));

  OnPopup := Refresh;
End;


Function TWPPopupMenu.MakeMenu(Const sCaption: String; aEvent: TNotifyEvent; iImage: Integer; Var oValue : TUixMenuItem): TUixMenuItem;
Begin
  Result := TUixMenuItem.Create(Self);

  oValue := Result;

  Result.Caption := sCaption;
  Result.OnClick := aEvent;
  Result.ImageIndex := iImage;
End;


Function TWPPopupMenu.MakeSeparator(Const sHint : String; Var oValue : TUixMenuItem): TUixMenuItem;
Begin
  Result := MakeMenu('-', Nil, -1, oValue);
  Result.Hint := sHint;
  Result.Enabled := False;
End;


Procedure TWPPopupMenu.SetFont(oSender: TObject);
Begin
  TWordProcessor(Owner).FontDialog;
End;


Procedure TWPPopupMenu.SetLine(oSender: TObject);
Begin
  TWordProcessor(Owner).LinePropertiesDialog;
End;


Procedure TWPPopupMenu.LoadInsertFieldMenu;
Var
  oWP : TWordProcessor;
  aCapabilities : TWPCapabilities;
  iLoop : Integer;
  oDefn : TWPFieldDefinitionProvider;
  oADefn : TWPAnnotationDefinitionProvider;
  iIcon : Integer;
  oMenu : TUixMenuItem;
Begin
  oWP := TWordProcessor(Owner);
  aCapabilities := oWP.PrimaryRange.Capabilities;
  For iLoop := Items.Count - 1 DownTo 0 Do
  Begin
    oMenu := Items[iLoop] As TUixMenuItem;
    If oMenu.Tag > 0 Then
      Items.Delete(iLoop);
  End;

  For iLoop := 0 To oWp.Settings.FieldDefinitions.Count - 1 Do
  Begin
    oDefn := oWp.Settings.FieldDefinitions[iLoop];
    iIcon := oDefn.GetStdIconIndex;
    If iIcon = -1 Then
      iIcon := WPIconModule.NONE;
    If oDefn.CanInsertField Then
    Begin
      // we can get away with a straight add here because we are always the last column
      Items.Add(MakeMenu(oDefn.GetTitle, DoFieldInsert, iIcon, oMenu));
      oMenu.Tag := iLoop+1;
      oMenu.Enabled := canInsertField In aCapabilities
    End;
  End;
  For iLoop := 0 To oWp.Settings.AnnotationDefinitions.Count - 1 Do
  Begin
    oADefn := oWp.Settings.AnnotationDefinitions[iLoop];
    iIcon := oADefn.GetIconIndex;
    If iIcon = -1 Then
      iIcon := WPIconModule.NONE;
    If oADefn.CanInsertAnnotation(oWP.PrimaryRange.SelectedText) Then
    Begin
      // we can get away with a straight add here because we are always the last column
      Items.Add(MakeMenu(oADefn.GetTitle, DoAnnotationInsert, iIcon, oMenu));
      oMenu.Tag := iLoop+1;
      oMenu.Enabled := canInsertAnnotation In aCapabilities
    End;
  End;
End;


Procedure TWPPopupMenu.Refresh(oSender: TObject);
Var
  oWP : TWordProcessor;
  aCapabilities : TWPCapabilities;
  sWord : String;
  oSuggestions : TFslStringList;
  iLoop : Integer;
  mnuWord : TUixMenuItem;
  aSpell : TWPWorkingDocumentSpellCheckingState;
  bEnabled : Boolean;
  bField : Boolean;
Begin
  oWP := TWordProcessor(Owner);

  LoadInsertFieldMenu;

  aCapabilities := oWP.PrimaryRange.Capabilities;

  mnuCut.Enabled := canCut In aCapabilities;
  mnuCopy.Enabled := canWrite In aCapabilities;
  mnuPaste.Enabled := (canInsert In aCapabilities) And Clipboard.CanPaste;
  mnuPasteSpecial.Enabled := (canInsert In aCapabilities) And Clipboard.CanPaste;
  mnuChangeCase.Enabled := (canWrite In aCapabilities) And Not TWordProcessor(Owner).ReadOnly;

  mnuFont.Enabled := CanFormat In aCapabilities;
  mnuLine.Enabled := canLineProps In aCapabilities;
  mnuLine.Visible := mnuLine.Enabled;
  mnuParagraph.Enabled := CanFormat In aCapabilities;

  // subMenus...
  If mnuPasteSpecial.Enabled Then
    ShowClipboardFormats;

  // Refresh Spelling Menu.
  mnuSpelling.Enabled := Not TWordProcessor(Owner).Settings.ReadOnly And TWordProcessor(Owner).HasSpeller;

  bField := False;
  If mnuSpelling.Enabled Then
  Begin
    While mnuSpelling.Items[1] <> mnuSpellSeparator1 Do
      mnuSpelling.Delete(1);

    If TWordProcessor(Owner).PrimaryRange.CursorInWord(sWord, aSpell) And (aSpell in [scsWrong, scsFieldWrong]) Then
    Begin
      oSuggestions := TFslStringList.Create;
      Try
        If TWordProcessor(Owner).PrimaryRange.HasCurrentFieldStart And TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.HasDefinitionProvider And
            (TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DefinitionProvider.ShouldCheckSpelling(TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DocField) <> FscsSpelling) Then
        Begin
          if TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DefinitionProvider.ShouldCheckSpelling(TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DocField) = FscsField Then
            TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DefinitionProvider.ListSuggestions(TWordProcessor(Owner).PrimaryRange.CurrentFieldStart.DocField, oSuggestions);
          bField := True;
        End
        Else
          TWordProcessor(Owner).Speller.ListSuggestions(sWord, oSuggestions);

        If oSuggestions.count = 0 Then
        Begin
          mnuSpellingSuggest.Caption := 'No Suggestions...';
          mnuSpellingSuggest.Enabled := False;
        End
        Else
        Begin
          mnuSpellingSuggest.Caption := oSuggestions[0];
          mnuSpellingSuggest.Enabled := True;
        End;

        For iLoop := oSuggestions.count - 1 DownTo 1 Do
        Begin
          mnuWord := TUixMenuItem.Create(mnuSpelling);
          mnuSpelling.Insert(1, mnuWord);
          mnuWord.Caption := oSuggestions[iLoop];
          mnuWord.OnClick := DoSpellSuggest;
        End;

        mnuSpellingAllow.Enabled := Not bField;
        mnuSpellingAdd.Enabled := Not bField;
      Finally
        oSuggestions.Free;
      End;
    End
    Else
    Begin
      mnuSpellingSuggest.Caption := 'Suggestions...';
      mnuSpellingSuggest.Enabled := False;
      mnuSpellingAllow.Enabled := False;
      mnuSpellingAdd.Enabled := False;
    End;
  End;

  mnuSpellingCheck.Enabled := True;

  mnuTable.Enabled := False;

  mnuInsertTable.Enabled := canInsertTable In aCapabilities;
  mnuInsertLine.Enabled := canInsertLine In aCapabilities;
  mnuInsertPageBreak.Enabled := canInsertPageBreak In aCapabilities;
  mnuInsertImage.Enabled := canInsertImage In aCapabilities;
  mnuInsertPDF.Enabled := canInsertImage In aCapabilities;
  mnuInsertTemplate.Enabled := canInsertTemplate In aCapabilities;
  mnuInsertSymbol.Enabled := canInsert In aCapabilities;
{
  mnuInsertComment.Enabled := canInsertAnnotation In aCapabilities;
  mnuInsertComment.Visible := TWordProcessor(Owner).Settings.ManageAnnotations;
  }
  mnuAnnotationEdit.Enabled := (canManageAnnotation in aCapabilities);
  mnuAnnotationDelete.Enabled := (canManageAnnotation in aCapabilities);
  mnuAnnotation.Enabled := (canManageAnnotation in aCapabilities);
  mnuAnnotation.Visible := mnuAnnotation.Enabled;

  mnuTableConvertToTable.Enabled := canConvertToTable In aCapabilities;
  bEnabled := False;
  For iLoop := 0 To mnuInsert.Count - 1 Do
    bEnabled := bEnabled Or mnuInsert.Items[iLoop].Enabled;
  mnuInsert.Enabled := bEnabled;

  mnuImageProperties.Enabled := canImageProps In aCapabilities;
  mnuImageMap.Visible := oWP.Settings.ImageMapEditing;
  mnuImageMap.Enabled := mnuImageMap.Visible And (canImageProps In aCapabilities);
  mnuImage.Enabled := (CanFormat In aCapabilities) And (mnuImageProperties.Enabled Or mnuImageMap.Enabled);
  mnuImage.Visible := mnuImage.Enabled;
  mnuImageProperties.Visible := mnuImage.Visible;
  mnuImageMap.Visible := mnuImage.Visible;

  mnuImageSelect.ImageIndex := WPIconModule.IMAGE_SELECT;
  mnuImageSelect.Checked := False;
  mnuImageDrawLine.ImageIndex := WPIconModule.IMAGE_PEN;
  mnuImageDrawLine.Checked := False;
  mnuImageRectangle.ImageIndex := WPIconModule.IMAGE_BOX;
  mnuImageRectangle.Checked := False;
  mnuImageCircle.ImageIndex := WPIconModule.IMAGE_CIRCLE;
  mnuImageCircle.Checked := False;
  mnuImageMark.ImageIndex := WPIconModule.IMAGE_MARK;
  mnuImageMark.Checked := False;
  mnuImageZoom.ImageIndex := WPIconModule.IMAGE_ZOOM;
  mnuImageZoom.Checked := False;

  mnuImageItem.Visible := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);
  mnuImageItem.Enabled := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);
  mnuImageToBack.Visible := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);
  mnuImageToBack.Enabled := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil) And (TWPWorkingDocumentImagePiece(FMouseInfo.Subject).Adornments.IndexByReference(FMouseInfo.Adornment) > 0);
  mnuImageToFront.Visible := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);
  mnuImageToFront.Enabled := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil) And (TWPWorkingDocumentImagePiece(FMouseInfo.Subject).Adornments.IndexByReference(FMouseInfo.Adornment) < TWPWorkingDocumentImagePiece(FMouseInfo.Subject).Adornments.Count - 1);
  mnuImageDelete.Visible := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);
  mnuImageDelete.Enabled := (FMouseInfo <> Nil) And (FMouseInfo.Adornment <> Nil);

  Case oWp.ImageTool Of
    itSelect :
      Begin
        mnuImageSelect.ImageIndex := WPIconModule.IMAGE_SELECT_ON;
        mnuImageSelect.Checked := True;
      End;
    itLine :
      Begin
        mnuImageDrawLine.ImageIndex := WPIconModule.IMAGE_PEN_ON;
        mnuImageDrawLine.Checked := True;
      End;
    itRectangle :
      Begin
        mnuImageRectangle.ImageIndex := WPIconModule.IMAGE_BOX_ON;
        mnuImageRectangle.Checked := True;
      End;
    itCircle :
      Begin
        mnuImageCircle.ImageIndex := WPIconModule.IMAGE_CIRCLE_ON;
        mnuImageCircle.Checked := True;
      End;
    itMark :
      Begin
        mnuImageMark.ImageIndex := WPIconModule.IMAGE_MARK_ON;
        mnuImageMark.Checked := True;
      End;
    itZoom :
      Begin
        mnuImageZoom.ImageIndex := WPIconModule.IMAGE_ZOOM_ON;
        mnuImageZoom.Checked := True;
      End;
  End;

  If Not mnuImage.Visible Then
  Begin
    mnuImageItem.Visible := False;
    mnuImageToBack.Visible := False;
    mnuImageToFront.Visible := False;
    mnuImageDelete.Visible := False;
    mnuImageSeparator.Visible := False;
    mnuImageMap.Visible := False;
    mnuImageProperties.Visible := False;
  End;
  mnuImageSelect.Visible := mnuImage.Visible;
  mnuImageDrawLine.Visible := mnuImage.Visible;
  mnuImageRectangle.Visible := mnuImage.Visible;
  mnuImageCircle.Visible := mnuImage.Visible;
  mnuImageMark.Visible := mnuImage.Visible;
  mnuImageZoom.Visible := mnuImage.Visible;

  mnuFieldNext.Enabled := canGotoField In aCapabilities;
  mnuFieldPrevious.Enabled := canGotoField In aCapabilities;
  mnuFieldEdit.Enabled := canFieldProps In aCapabilities;
  mnuFieldEditSection.Enabled := canFieldSectionProps In aCapabilities;
  mnuFieldRemove.Enabled := canRemoveField In aCapabilities;
  mnuFieldSectionRemove.Enabled := canRemoveFieldSection In aCapabilities;

  mnuFieldEditSection.Visible := mnuFieldEditSection.Enabled;
  mnuFieldEdit.Visible := mnuFieldEdit.Enabled;
  mnuFieldSectionRemove.Visible := mnuFieldSectionRemove.Enabled;
  mnuFieldRemove.Visible := mnuFieldRemove.Enabled;
  mnuFieldNext.Visible := mnuFieldNext.Enabled;
  mnuFieldPrevious.Visible := mnuFieldPrevious.Enabled;

  If oWp.PrimaryRange.HasCurrentFieldStart Then
    If oWp.PrimaryRange.CurrentFieldStart.Name = '' Then
    Begin
      mnuFieldEdit.Caption := '&Edit [Unnamed]';
      mnuFieldRemove.Caption := '&Remove [Unnamed]';
    End
    Else
    Begin
      mnuFieldEdit.Caption := '&Edit "'+oWp.PrimaryRange.CurrentFieldStart.Name+'"';
      mnuFieldRemove.Caption := '&Remove "'+oWp.PrimaryRange.CurrentFieldStart.Name+'"';
    End
  Else
  Begin
    mnuFieldEdit.Caption := '&Edit';
    mnuFieldRemove.Caption := '&Remove';
  End;
  If oWp.PrimaryRange.HasCurrentSectionStart Then
    If oWp.PrimaryRange.CurrentSectionStart.Name = '' Then
    Begin
      mnuFieldEditSection.Caption := '&Edit [Unnamed] (Multi-Paragraph)';
      mnuFieldSectionRemove.Caption := '&Remove [Unnamed] (Multi-Paragraph)';
    End
    Else
    Begin
      mnuFieldEditSection.Caption := '&Edit "'+oWp.PrimaryRange.CurrentSectionStart.Name+'"';
      mnuFieldSectionRemove.Caption := '&Remove "'+oWp.PrimaryRange.CurrentSectionStart.Name+'"';
    End
  Else
  Begin
    mnuFieldEditSection.Caption := '&Edit';
    mnuFieldSectionRemove.Caption := '&Remove';
  End;

  mnuField.Enabled := mnuFieldNext.Enabled Or mnuFieldPrevious.Enabled Or mnuFieldEdit.Enabled Or mnuFieldRemove.Enabled;
  mnuField.Visible := mnuField.Enabled;

  mnuTableInsertCopiedRowAbove.Enabled := (canInsertRowAbove In aCapabilities) And Clipboard.HasContentType(wcctNative);
  mnuTableInsertCopiedRowBelow.Enabled := (canInsertRowBelow In aCapabilities) And Clipboard.HasContentType(wcctNative);
  mnuTableInsertRowAbove.Enabled := canInsertRowAbove In aCapabilities;
  mnuTableInsertRowBelow.Enabled := canInsertRowBelow In aCapabilities;
  mnuTableInsertColumnLeft.Enabled := canInsertColumn In aCapabilities;
  mnuTableInsertColumnRight.Enabled := canInsertColumn In aCapabilities;
  mnuTableMergeCells.Enabled := canMergeCells In aCapabilities;
  mnuTableSplitCell.Enabled := canSplitCell In aCapabilities;

  mnuTableSelectRow.Enabled := canSelectRow In aCapabilities;
  mnuTableSelectTable.Enabled := canSelectTable In aCapabilities;
  mnuTableDeleteTable.Enabled := canRemoveTable In aCapabilities;
  mnuTableDeleteRow.Enabled := canRemoveRow In aCapabilities;
  mnuTableDeleteColumn.Enabled := canRemoveColumn In aCapabilities;
  mnuTableSort.Enabled := canSortTable In aCapabilities;
  mnuTableDetails.Enabled := canTableProps In aCapabilities;

  mnuTable.Enabled := mnuTableInsertCopiedRowAbove.Enabled Or mnuTableInsertCopiedRowBelow.Enabled Or
          mnuTableInsertRowAbove.Enabled Or 
          mnuTableInsertRowBelow.Enabled Or mnuTableInsertColumnLeft.Enabled Or
          mnuTableInsertColumnRight.Enabled Or mnuTableSelectRow.Enabled Or
          mnuTableMergeCells.Enabled Or mnuTableSplitCell.Enabled Or
          mnuTableSelectTable.Enabled Or mnuTableDeleteTable.Enabled Or
          mnuTableDeleteRow.Enabled Or mnuTableDeleteColumn.Enabled Or
          mnuTableDetails.Enabled;
  mnuTable.Visible := mnuTable.Enabled;
  mnuTableInsertCopiedRowAbove.Visible := mnuTable.Visible;
  mnuTableInsertCopiedRowBelow.Visible := mnuTable.Visible;
  mnuTableInsertRowAbove.Visible := mnuTable.Visible;
  mnuTableInsertRowBelow.Visible := mnuTable.Visible;
  mnuTableInsertColumnLeft.Visible := mnuTable.Visible;
  mnuTableInsertColumnRight.Visible := mnuTable.Visible;
  mnuTableMergeCells.Visible := mnuTable.Visible And mnuTableMergeCells.Enabled;
  mnuTableSplitCell.Visible := mnuTable.Visible And mnuTableSplitCell.Enabled;
  mnuTableSeparator1.Visible := mnuTable.Visible;
  mnuTableSelectRow.Visible := mnuTable.Visible;
  mnuTableSelectTable.Visible := mnuTable.Visible;
  mnuTableSeparator2.Visible := mnuTable.Visible;
  mnuTableDeleteTable.Visible := mnuTable.Visible;
  mnuTableDeleteRow.Visible := mnuTable.Visible;
  mnuTableDeleteColumn.Visible := mnuTable.Visible;
  mnuTableSeparator3.Visible := mnuTable.Visible;
  mnuTableSort.Visible := mnuTable.Visible;
  mnuTableDetails.Visible := mnuTable.Visible;

  mnuSptSnapshot.Enabled := oWP.Settings.SnapshotEmail <> '';
  bEnabled := oWp.Settings.ShowDocumentInspector;
  For iLoop := 0 To oWp.Inspectors.Count - 1 Do
    bEnabled := bEnabled And Not (oWp.Inspectors[iLoop] Is TWPPropertyInspector);

  mnuSptInspector.Enabled := bEnabled;
  oWP.RefreshPopup;

  mnuSupport.Visible := True;
End;

Procedure TWPPopupMenu.DoCopy(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.Copy;
End;

Procedure TWPPopupMenu.DoCut(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.Cut;
End;

Procedure TWPPopupMenu.DoPaste(oSender: TObject);
Var
  sError : String;
Begin
  If Not TWordProcessor(Owner).PrimaryRange.Paste(sError) Then
    DialogError(sError);
End;

Procedure TWPPopupMenu.DoPasteSpecial(oSender: TObject);
Var
  sError : String;
Begin
  If Not TWordProcessor(Owner).PrimaryRange.PasteSpecial(TWPClipboardContentType((oSender As TUixMenuItem).tag), sError) Then
    DialogError(sError);
End;

Procedure TWPPopupMenu.SetPara(oSender: TObject);
Begin
  TWordProcessor(Owner).ParaDialog;
End;

Procedure TWPPopupMenu.DoImageInsert(oSender: TObject);
Begin
  TWordProcessor(Owner).InsertImageDialog;
End;

Procedure TWPPopupMenu.DoPDFInsert(oSender: TObject);
Begin
//  TWordProcessor(Owner).InsertPDFDialog;
End;

Procedure TWPPopupMenu.DoImageProps(oSender: TObject);
Begin
  TWordProcessor(Owner).ImagePropertiesDialog;
End;

Procedure TWPPopupMenu.DoImageMaps(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageMapsDialog;
End;

Procedure TWPPopupMenu.DoFieldInsert(oSender: TObject);
Var
  oDefn : TWPFieldDefinitionProvider;
Begin
  oDefn :=  TWordProcessor(Owner).Settings.FieldDefinitions[TUixMenuItem(oSender).Tag - 1];
  TWordProcessor(Owner).InsertField(oDefn);
End;

Procedure TWPPopupMenu.DoTemplateInsert(oSender: TObject);
Begin
  TWordProcessor(Owner).InsertTemplate;
End;

Procedure TWPPopupMenu.DoSymbolInsert(oSender: TObject);
Begin
  TWordProcessor(Owner).InsertSymbol;
End;

Procedure TWPPopupMenu.DoNextField(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.NextField;
End;

Procedure TWPPopupMenu.DoPreviousField(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.PreviousField;
End;

Procedure TWPPopupMenu.DoFieldProps(oSender: TObject);
Begin
  TWordProcessor(Owner).FieldPropertiesDialog;
End;

Procedure TWPPopupMenu.DoFieldSectionProps(oSender: TObject);
Begin
  TWordProcessor(Owner).FieldSectionPropertiesDialog;
End;

Procedure TWPPopupMenu.DoRemoveField(oSender: TObject);
Begin
  Case MessageDlg('Remove Field. Keep Field Contents?', mtConfirmation, mbYesNoCancel, 0) Of
    mrYes : TWordProcessor(Owner).PrimaryRange.RemoveField(True);
    mrNo : TWordProcessor(Owner).PrimaryRange.RemoveField(False);
  End;
End;

Procedure TWPPopupMenu.DoRemoveFieldSection(oSender: TObject);
Begin
  Case MessageDlg('Remove Multi-Paragraph Field. Keep Field Contents?', mtConfirmation, mbYesNoCancel, 0) Of
    mrYes : TWordProcessor(Owner).PrimaryRange.RemoveFieldSection(True);
    mrNo : TWordProcessor(Owner).PrimaryRange.RemoveFieldSection(False);
  End;
End;


Procedure TWPPopupMenu.DoSpellSuggest(oSender: TObject);
Begin
  If TWordProcessor(Owner).PrimaryRange.SelectWord Then
    TWordProcessor(Owner).PrimaryRange.Insert(StringStrip((oSender As TUixMenuItem).Caption, '&'));
End;


Procedure TWPPopupMenu.DoSpellCheck(oSender : TObject);
Begin
  TWordProcessor(Owner).CheckSpelling;
End;


Procedure TWPPopupMenu.DoSpellAdd(oSender: TObject);
Var
  sWord : String;
Begin
  If TWordProcessor(Owner).PrimaryRange.CursorInWord(sWord) And TWordProcessor(Owner).Speller.AddWord(sWord) Then
    TWordProcessor(Owner).PrimaryRange.CheckSpelling(sWord);
End;

Procedure TWPPopupMenu.DoSpellAllow(oSender: TObject);
Var
  sWord : String;
Begin
  If TWordProcessor(Owner).PrimaryRange.CursorInWord(sWord) And TWordProcessor(Owner).Speller.IgnoreWord(sWord) Then
    TWordProcessor(Owner).PrimaryRange.CheckSpelling(sWord);
End;

Procedure TWPPopupMenu.DoReviewAllowed(oSender : TObject);
Begin
  TWordProcessor(Owner).ReviewWordDialog;
End;

Procedure TWPPopupMenu.DoInsertTable(oSender : TObject);
Begin
  TWordProcessor(Owner).InsertTableDialog;
End;


Procedure TWPPopupMenu.DoInsertLine(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertLine;
End;


Procedure TWPPopupMenu.DoInsertPageBreak(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertPageBreak;
End;


Procedure TWPPopupMenu.DoConvertToTable(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.ConvertTextToTable;
End;

Procedure TWPPopupMenu.DoInsertCopiedRowAbove(oSender : TObject);
Var
  sError : String;
Begin
  If Not TWordProcessor(Owner).PrimaryRange.InsertCopiedRowAbove(sError) Then
    DialogError(sError);
End;


Procedure TWPPopupMenu.DoInsertCopiedRowBelow(oSender : TObject);
Var
  sError: String;
Begin
  If Not TWordProcessor(Owner).PrimaryRange.InsertCopiedRowBelow(sError) Then
    DialogError(sError);
End;

Procedure TWPPopupMenu.DoInsertRowAbove(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertRowAbove;
End;


Procedure TWPPopupMenu.DoInsertRowBelow(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertRowBelow;
End;


Procedure TWPPopupMenu.DoInsertColumnLeft(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertColumnLeft;
End;


Procedure TWPPopupMenu.DoInsertColumnRight(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.InsertColumnRight;
End;


Procedure TWPPopupMenu.DoSelectRow(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.SelectRow;
End;


Procedure TWPPopupMenu.DoSelectTable(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.SelectTable;
End;


Procedure TWPPopupMenu.DoDeleteTable(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.DeleteTable;
End;


Procedure TWPPopupMenu.DoDeleteRow(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.DeleteRow;
End;


Procedure TWPPopupMenu.DoDeleteColumn(oSender : TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.DeleteColumn;
End;

Procedure TWPPopupMenu.LoadClipboardFormats;
Var
  aStyle : TWPClipboardContentType;
  mnuFormat : TUixMenuItem;
Begin
  mnuPasteSpecial.Add(MakeMenu('Paste as Row Above', DoInsertCopiedRowAbove, WPIconModule.COPIED_ROW_ABOVE, mnuTableInsertCopiedRowAbove));
  mnuPasteSpecial.Add(MakeMenu('Paste as Row Below', DoInsertCopiedRowBelow, WPIconModule.COPIED_ROW_BELOW, mnuTableInsertCopiedRowBelow));

  For aStyle := Succ(Low(TWPClipboardContentType)) To High(TWPClipboardContentType) Do
    Begin
      mnuFormat := TUixMenuItem.Create(mnuPasteSpecial);
      mnuPasteSpecial.Add(mnuFormat);
      mnuFormat.Caption := WPCLIPBOARDCONTENTTYPE_NAMES[aStyle];
      mnuFormat.Tag := Integer(aStyle);
      mnuFormat.OnClick := DoPasteSpecial;
    End;
End;

Procedure TWPPopupMenu.ShowClipboardFormats;
Var
  oClip : TWPClipboard;
  aStyle : TWPClipboardContentType;
  aStyles : TWPClipboardContentTypes;
Begin
  oClip := TWPClipboard.Create;
  Try
    oClip.Open;
    aStyles := oClip.ContentTypes;
    For aStyle := Succ(Low(TWPClipboardContentType)) To High(TWPClipboardContentType) Do
      // won't hide the first 2 submenus, which are for insert into table
      mnuPasteSpecial.Items[ord(aStyle) + 1].Visible := (Not TWordProcessor(Owner).Settings.ConsoleMode Or (aStyle In [wcctText, wcctUnicode])) And (aStyle In aStyles);
  Finally
    oClip.Free;
  End;
End;

Procedure TWPPopupMenu.DoChangeCase(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.ApplyChangeCase(TWPChangeCaseType((oSender As TUixMenuItem).Tag));
End;

Procedure TWPPopupMenu.MakeChangeCaseMenu;
Var
  mnuCase: TUixMenuItem;
  aCaseType: TWPChangeCaseType;
Begin
  For aCaseType := Succ(Low(TWPChangeCaseType)) To High(TWPChangeCaseType) Do
    Begin
      mnuCase := TUixMenuItem.Create(mnuChangeCase);
      mnuChangeCase.Add(mnuCase);
      mnuCase.Caption := WPCHANGECASETYPE_NAMES[aCaseType];
      mnuCase.Tag := Integer(aCaseType);
      mnuCase.OnClick := DoChangeCase;
    End;
End;

Procedure TWPPopupMenu.DoTableProperties(oSender: TObject);
Begin
  TWordProcessor(Owner).TablePropertiesDialog;
End;

Procedure TWPPopupMenu.DoSortTableRow(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.SelectTable;
  TWordProcessor(Owner).SortTableDialog;
End;

Procedure TWPPopupMenu.DoDbgInspector(oSender: TObject);
Var
  oInspector : TWPPropertyInspector;
Begin
  oInspector := TWPPropertyInspector.Create(TWordProcessor(Owner));
  TWordProcessor(Owner).RegisterInspector(oInspector);
  oInspector.WordProcessor := TWordProcessor(Owner);
End;

Procedure TWPPopupMenu.DoDbgSnapshot(oSender: TObject);
Begin
  TWordProcessor(Owner).Snapshot('Manual', True);
End;

Procedure TWPPopupMenu.DoImageRectangle(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itRectangle;
End;

Procedure TWPPopupMenu.DoImageCircle(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itCircle;
End;

Procedure TWPPopupMenu.DoImageLine(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itLine;
End;

Procedure TWPPopupMenu.DoImageMark(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itMark;
End;

Procedure TWPPopupMenu.DoImageSelect(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itSelect;
End;

Procedure TWPPopupMenu.DoImageZoom(oSender: TObject);
Begin
  TWordProcessor(Owner).ImageTool := itZoom;
End;


Procedure TWPPopupMenu.Finalise;
Begin
  FMouseInfo.Free;
  Inherited;
End;

Procedure TWPPopupMenu.SetMouseInfo(Const Value: TWPMouseInfo);
Begin
  FMouseInfo.Free;
  FMouseInfo := Value;
End;

Procedure TWPPopupMenu.DoImageDelete(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.DeleteAdornment(TWPWorkingDocumentImagePiece(FMouseInfo.Subject), FMouseInfo.Adornment);
End;

Procedure TWPPopupMenu.DoImageToBack(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.MoveAdornmentToBack(TWPWorkingDocumentImagePiece(FMouseInfo.Subject), FMouseInfo.Adornment);
End;

Procedure TWPPopupMenu.DoImageToFront(oSender: TObject);
Begin
  TWordProcessor(Owner).PrimaryRange.MoveAdornmentToFront(TWPWorkingDocumentImagePiece(FMouseInfo.Subject), FMouseInfo.Adornment);
End;

procedure TWPPopupMenu.DoMergeCells(oSender: TObject);
begin
  TWordProcessor(Owner).PrimaryRange.MergeCells;
end;

procedure TWPPopupMenu.DoSplitCell(oSender: TObject);
begin
  TWordProcessor(Owner).PrimaryRange.SplitCell;
end;

Procedure TWPPopupMenu.DoAnnotationInsert(oSender: TObject);
Var
  oDefn : TWPAnnotationDefinitionProvider;
Begin
  oDefn :=  TWordProcessor(Owner).Settings.AnnotationDefinitions[TUixMenuItem(oSender).Tag - 1];
  TWordProcessor(Owner).InsertAnnotation(oDefn);
End;

Procedure TWPPopupMenu.DoAnnotationEdit(oSender : TObject);
Begin
  TWordProcessor(Owner).EditAnnotation;
End;

Procedure TWPPopupMenu.DoAnnotationDelete(oSender : TObject);
Begin
  TWordProcessor(Owner).DeleteAnnotation;
End;


End.

