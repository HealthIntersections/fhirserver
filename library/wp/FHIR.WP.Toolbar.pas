Unit FHIR.WP.Toolbar;

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
  SysUtils, Classes, Graphics, Controls, StdCtrls, ExtCtrls, Dialogs, Buttons,
  fsl_utilities,
  fui_vclx_Base, fui_vclx_Images, fui_vclx_Controls, fui_vclx_Advanced,
  wp_types, FHIR.WP.Widgets, wp_definers, wp_clipboard,
  FHIR.WP.Icons, FHIR.WP.Control;

Type
  TWordProcessorToolbar = Class(TUixAdvancedToolBar)
    Private
      FWordProcessor : TWordProcessor;

      FVisibleWidgetSet : TWPToolbarWidgets;

      FNewComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FOpenComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FSaveComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FSaveAsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FPrintComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FPageDesignComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      FCutComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FCopyComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FCopyAllComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FPasteComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FPasteSpecialComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FUndoComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FRedoComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      FSearchComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FPlaybackComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FSpellingComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      FStyleComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FStyleComboBox : TUixAdvancedComboBox;

      FFontNameComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FFontNameComboBox : TUixAdvancedFontComboBox;

      FFontSizeComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FFontSizeComboBox : TUixAdvancedComboBox;

      FFontColourComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FFontColourComboBox : TUixAdvancedColourComboBox;

      FBackColourComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FBackColourComboBox : TUixAdvancedColourComboBox;

      FBoldComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FItalicComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FUnderlineComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FSuperscriptComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FSubscriptComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FChangeCaseComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FLeftComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FRightComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FCentreComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FJustifyComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FBulletsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FNumbersComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FIndentComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FOutdentComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertTableComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertImageComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertLineComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertPageBreakComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FEditHintsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FMacroComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertFieldComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      FInsertTemplateComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      FInsertFieldPopupMenu : TUixPopupMenu;
      FOnPageDesign: TNotifyEvent;
      FOnSave: TNotifyEvent;
      FOnSaveAs: TNotifyEvent;
      FOnNew: TNotifyEvent;
      FOnOpen: TNotifyEvent;
      FOnPrint: TNotifyEvent;

      Procedure NewButtonClickHandler(oSender : TObject);
      Procedure OpenButtonClickHandler(oSender : TObject);
      Procedure SaveButtonClickHandler(oSender : TObject);
      Procedure SaveAsButtonClickHandler(oSender : TObject);
      Procedure PrintButtonClickHandler(oSender : TObject);
      Procedure PageDesignButtonClickHandler(oSender : TObject);
      Procedure HintsButtonClickHandler(oSender : TObject);
      Procedure BoldButtonClickHandler(oSender : TObject);
      Procedure CutButtonClickHandler(oSender : TObject);
      Procedure CopyButtonClickHandler(oSender : TObject);
      Procedure CopyAllButtonClickHandler(oSender : TObject);
      Procedure PasteButtonClickHandler(oSender : TObject);
      Procedure PasteSpecialButtonClickHandler(oSender : TObject);
      Procedure ItalicsButtonClickHandler(oSender : TObject);
      Procedure UnderlineButtonClickHandler(oSender : TObject);
      Procedure SuperscriptButtonClickHandler(oSender : TObject);
      Procedure SubscriptButtonClickHandler(oSender : TObject);
      Procedure CaseButtonClickHandler(oSender : TObject);
      Procedure UndoButtonClickHandler(oSender : TObject);
      Procedure RedoButtonClickHandler(oSender : TObject);
      Procedure AlignLeftButtonClickHandler(oSender : TObject);
      Procedure AlignCentreButtonClickHandler(oSender : TObject);
      Procedure AlignRightButtonClickHandler(oSender : TObject);
      Procedure AlignJustifyButtonClickHandler(oSender : TObject);
      Procedure BulletsButtonClickHandler(oSender : TObject);
      Procedure NumbersButtonClickHandler(oSender : TObject);
      Procedure IndentButtonClickHandler(oSender : TObject);
      Procedure OutdentButtonClickHandler(oSender : TObject);
      Procedure InsertImageButtonClickHandler(oSender : TObject);
      Procedure InsertFieldDefaultButtonClickHandler(oSender : TObject);
      Procedure InsertTemplateButtonClickHandler(oSender : TObject);
      Procedure InsertLineButtonClickHandler(oSender : TObject);
      Procedure InsertPageBreakButtonClickHandler(oSender : TObject);
      Procedure InsertTableButtonClickHandler(oSender : TObject);
      Procedure VoicePlaybackButtonClickHandler(oSender : TObject);
      Procedure SpellingButtonClickHandler(oSender : TObject);
      Procedure SearchButtonClickHandler(oSender : TObject);
      Procedure MacroButtonClickHandler(oSender : TObject);

      Procedure StyleChangeHandler(oSender : TObject);
      Procedure FontNameChangeHandler(oSender : TObject);
      Procedure FontSizeChangeHandler(oSender : TObject);
      Procedure FontColourChangeHandler(oSender : TObject);
      Procedure BackColourChangeHandler(oSender : TObject);

      Procedure InsertFieldMenuItemClickHandler(oSender : TObject);

      Procedure WordProcessorObserverNotificationHandler(oSender : TObject);

      Procedure UpdateStatus;
      Procedure Build;
      Procedure LoadWordProcessorData;

      Procedure SetWordProcessor(Const Value: TWordProcessor);

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Refresh;

      Property VisibleWidgetSet : TWPToolbarWidgets Read FVisibleWidgetSet Write FVisibleWidgetSet;
      Property WordProcessor : TWordProcessor Read FWordProcessor Write SetWordProcessor;

      Property NewComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FNewComponentEntity;
      Property OpenComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FOpenComponentEntity;
      Property SaveComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSaveComponentEntity;
      Property SaveAsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSaveAsComponentEntity;
      Property PrintComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FPrintComponentEntity;
      Property PageDesignComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FPageDesignComponentEntity;
      Property BackColourComboBox : TUixAdvancedColourComboBox Read FBackColourComboBox;
      Property BackColourComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FBackColourComponentEntity;
      Property BoldComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FBoldComponentEntity;
      Property BulletsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FBulletsComponentEntity;
      Property CentreComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FCentreComponentEntity;
      Property ChangeCaseComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FChangeCaseComponentEntity;
      Property CopyAllComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FCopyAllComponentEntity;
      Property CopyComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FCopyComponentEntity;
      Property CutComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FCutComponentEntity;
      Property EditHintsComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FEditHintsComponentEntity;
      Property FontColourComboBox : TUixAdvancedColourComboBox Read FFontColourComboBox;
      Property FontColourComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FFontColourComponentEntity;
      Property FontNameComboBox : TUixAdvancedFontComboBox Read FFontNameComboBox;
      Property FontNameComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FFontNameComponentEntity;
      Property FontSizeComboBox : TUixAdvancedComboBox Read FFontSizeComboBox;
      Property FontSizeComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FFontSizeComponentEntity;
      Property IndentComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FIndentComponentEntity;
      Property InsertFieldComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertFieldComponentEntity;
      Property InsertImageComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertImageComponentEntity;
      Property InsertLineComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertLineComponentEntity;
      Property InsertPageBreakComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertPageBreakComponentEntity;
      Property InsertTableComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertTableComponentEntity;
      Property InsertTemplateComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FInsertTemplateComponentEntity;
      Property ItalicComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FItalicComponentEntity;
      Property JustifyComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FJustifyComponentEntity;
      Property LeftComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FLeftComponentEntity;
      Property MacroComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FMacroComponentEntity;
      Property NumbersComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FNumbersComponentEntity;
      Property OutdentComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FOutdentComponentEntity;
      Property PasteComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FPasteComponentEntity;
      Property PasteSpecialComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FPasteSpecialComponentEntity;
      Property PlaybackComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FPlaybackComponentEntity;
      Property RedoComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FRedoComponentEntity;
      Property RightComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FRightComponentEntity;
      Property SearchComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSearchComponentEntity;
      Property SpellingComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSpellingComponentEntity;
      Property StyleComboBox : TUixAdvancedComboBox Read FStyleComboBox;
      Property StyleComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FStyleComponentEntity;
      Property SubscriptComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSubscriptComponentEntity;
      Property SuperscriptComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FSuperscriptComponentEntity;
      Property UnderlineComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FUnderlineComponentEntity;
      Property UndoComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read FUndoComponentEntity;
      Property onNew : TNotifyEvent read FOnNew write FOnNew;
      Property onOpen : TNotifyEvent read FOnOpen write FOnOpen;
      Property onSave : TNotifyEvent read FOnSave write FOnSave;
      Property onSaveAs : TNotifyEvent read FOnSaveAs write FOnSaveAs;
      Property onPrint : TNotifyEvent read FOnPrint write FOnPrint;
      Property onPageDesign : TNotifyEvent read FOnPageDesign write FOnPageDesign;

  End;

Const

  WordProcessorToolbarWidgetDefaultSet =
    [tbwCut, tbwCopy, tbwPaste, tbwUndo, tbwRedo, tbwSearch, tbwPlayback, tbwSpelling,
    tbwStyle, tbwFontName, tbwFontSize, tbwFontColour, tbwBold, tbwItalic, tbwUnderline,
    tbwLeft, tbwCentre, tbwRight, tbwJustify, tbwBullet, tbwNumber, tbwIndent, tbwOutdent,
    tbwInsertImage, tbwInsertTable, tbwInsertPageBreak, tbwInsertField, tbwMacro];

  WordProcessorToolbarWidgetUniversalSet = [Low(TWPToolbarWidget)..High(TWPToolbarWidget)];

  WordProcessorToolbarWidgetCaptionArray : Array [TWPToolbarWidget] Of String = (
    'Cut', 'Copy', 'Copy All', 'Paste', 'Paste Special',
    'Undo', 'Redo', 'Search', 'Replay Dictation', 'Spelling', 'Style', 'Font Name', 'Font Size',
    'Font Colour', 'Background Color', 'Bold', 'Italic', 'Underline', 'Superscript', 'Subscript', 'ChangeCase',
    'Left Justify', 'Centre Justify', 'Right Justify', 'Block Justify', 'Bulleted', 'Numbered', 'Indent Paragraph', 'Unindent Paragraph',
    'Insert Table', 'Insert Image', 'Insert Horizontal Line', 'Insert Page Break', 'Show Paragraph markers', 'Expand as a Macro', 'Insert Field', 'Insert Template',
    'Insert Symbol', 'New', 'Open', 'Save', 'Save As', 'Print', 'Import', 'Export', 'Page Design'
    );

Const
   LEFT_INC_V = 60;
   LEFT_INC_H = 70;
   Btn_Offs_Cat_Operations = 0;
   Btn_Offs_New = Btn_Offs_Cat_Operations + 1;
   Btn_Offs_Open = Btn_Offs_New + 1;
   Btn_Offs_Save = Btn_Offs_Open + 1;
   Btn_Offs_SaveAs = Btn_Offs_Save + 1;
   Btn_Offs_Print_Preview = Btn_Offs_SaveAs + 1;
   Btn_Offs_Print = Btn_Offs_Print_Preview + 1;
   Btn_Offs_Email = Btn_Offs_Print + 1;
   Btn_Offs_Fax = Btn_Offs_Email + 1;
   Btn_Offs_Exit = Btn_Offs_Fax + 1;
   Btn_Offs_Cat_Edit = Btn_Offs_Exit + 1;
   Btn_Offs_Undo = Btn_Offs_Cat_Edit + 1;
   Btn_Offs_Redo = Btn_Offs_Undo + 1;
   Btn_Offs_Cut = Btn_Offs_Redo + 1;
   Btn_Offs_Copy = Btn_Offs_Cut + 1;
   Btn_Offs_Paste = Btn_Offs_Copy + 1;
   Btn_Offs_PasteSpecial = Btn_Offs_Paste + 1;
   Btn_Offs_SelectAll = Btn_Offs_PasteSpecial + 1;
   Btn_Offs_CopyFilename = Btn_Offs_SelectAll + 1;
   Btn_Offs_Search = Btn_Offs_CopyFilename + 1;
   Btn_Offs_Replace = Btn_Offs_Search + 1;
   Btn_Offs_ChangeCase = Btn_Offs_Replace + 1;
   Btn_Offs_Cat_Font = Btn_Offs_ChangeCase + 1;
   Btn_Offs_Bold = Btn_Offs_Cat_Font + 1;
   Btn_Offs_Italic = Btn_Offs_Bold + 1;
   Btn_Offs_Underline = Btn_Offs_Italic + 1;
   Btn_Offs_Colour = Btn_Offs_Underline + 1;
   Btn_Offs_Background = Btn_Offs_Colour + 1;
   Btn_Offs_Superscript = Btn_Offs_Background + 1;
   Btn_Offs_Subscript = Btn_Offs_Superscript + 1;
   Btn_Offs_FontProps = Btn_Offs_Subscript + 1;
   Btn_Offs_Cat_Paragraph = Btn_Offs_FontProps + 1;
   Btn_Offs_Left = Btn_Offs_Cat_Paragraph + 1;
   Btn_Offs_Center = Btn_Offs_Left + 1;
   Btn_Offs_Right = Btn_Offs_Center + 1;
   Btn_Offs_Justify = Btn_Offs_Right + 1;
   Btn_Offs_Normal = Btn_Offs_Justify + 1;
   Btn_Offs_Bullets = Btn_Offs_Normal + 1;
   Btn_Offs_Numbers = Btn_Offs_Bullets + 1;
   Btn_Offs_Indent = Btn_Offs_Numbers + 1;
   Btn_Offs_Outdent = Btn_Offs_Indent + 1;
   Btn_Offs_ParaProps = Btn_Offs_Outdent + 1;
   Btn_Offs_Cat_Insert = Btn_Offs_ParaProps + 1;
   Btn_Offs_Symbol = Btn_Offs_Cat_Insert + 1;
   Btn_Offs_Field = Btn_Offs_Symbol + 1;
   Btn_Offs_Template = Btn_Offs_Field + 1;
   Btn_Offs_Break = Btn_Offs_Template + 1;
   Btn_Offs_Picture = Btn_Offs_Break + 1;
   Btn_Offs_InsTable = Btn_Offs_Picture + 1;
   Btn_Offs_InsLine = Btn_Offs_InsTable + 1;
   Btn_Offs_Comment = Btn_Offs_InsLine + 1;
   Btn_Offs_Cat_Image = Btn_Offs_Comment + 1;
   Btn_Offs_Image = Btn_Offs_Cat_Image + 1;
   Btn_Offs_ImageMap = Btn_Offs_Image + 1;
   Btn_Offs_Select = Btn_Offs_ImageMap + 1;
   Btn_Offs_Line = Btn_Offs_Select + 1;
   Btn_Offs_Rect = Btn_Offs_Line + 1;
   Btn_Offs_Circle = Btn_Offs_Rect + 1;
   Btn_Offs_Mark = Btn_Offs_Circle + 1;
   Btn_Offs_Zoom = Btn_Offs_Mark + 1;
   Btn_Offs_ImageEdit = Btn_Offs_Zoom + 1;
   Btn_Offs_Cat_Field = Btn_Offs_ImageEdit + 1;
   Btn_Offs_Ins_Field = Btn_Offs_Cat_Field + 1;
   Btn_Offs_Previous = Btn_Offs_Ins_Field + 1;
   Btn_Offs_Next = Btn_Offs_Previous + 1;
   Btn_Offs_EditField = Btn_Offs_Next + 1;
   Btn_Offs_RemoveField = Btn_Offs_EditField + 1;
   Btn_Offs_cat_table = Btn_Offs_RemoveField + 1;
   Btn_Offs_TableToText = Btn_Offs_cat_table + 1;
   Btn_Offs_TextToTable = Btn_Offs_TableToText + 1;
   Btn_Offs_InsColLeft = Btn_Offs_TextToTable + 1;
   Btn_Offs_InsColRight = Btn_Offs_InsColLeft + 1;
   Btn_Offs_InsRowAbove = Btn_Offs_InsColRight + 1;
   Btn_Offs_InsRowBelow = Btn_Offs_InsRowAbove + 1;
   Btn_Offs_SelRow = Btn_Offs_InsRowBelow + 1;
   Btn_Offs_SelTable = Btn_Offs_SelRow + 1;
   Btn_Offs_DelCol = Btn_Offs_SelTable + 1;
   Btn_Offs_DelRow = Btn_Offs_DelCol + 1;
   Btn_Offs_DelTable = Btn_Offs_DelRow + 1;
   Btn_Offs_Sort = Btn_Offs_DelTable + 1;
   Btn_Offs_TableProps = Btn_Offs_Sort + 1;
   Btn_Offs_cat_Tools = Btn_Offs_TableProps + 1;
   Btn_Offs_Spell = Btn_Offs_cat_Tools + 1;
   Btn_Offs_Styles = Btn_Offs_Spell + 1;
   Btn_Offs_Options = Btn_Offs_Styles + 1;
   Btn_Offs_cat_Help = Btn_Offs_Options + 1;
   Btn_Offs_HelpIndex = Btn_Offs_cat_Help + 1;
   Btn_Offs_HelpHome = Btn_Offs_HelpIndex + 1;
   Btn_Offs_HelpVersion = Btn_Offs_HelpHome + 1;
   Btn_Offs_HelpSupportCase = Btn_Offs_HelpVersion + 1;

Type
  TWordProcessorTouchToolbar = Class(TUixPanel)
  Private
    FWordProcessor : TWordProcessor;
    FColourDialog : TColorDialog;

    FOperationsButton : TSpeedButton;
    FEditButton : TSpeedButton;
    FFontButton : TSpeedButton;
    FParagraphButton : TSpeedButton;
    FInsertButton : TSpeedButton;
    FImageButton : TSpeedButton;
    FFieldButton : TSpeedButton;
    FTableButton : TSpeedButton;
    FToolsButton : TSpeedButton;
    FHelpButton : TSpeedButton;

    FVisibleWidgetSet : TWPToolbarWidgets;
    FImages : TUixImages;
    FCurrentMenu : TUixPanel;
    FOnNew: TNotifyEvent;
    FOnSave: TNotifyEvent;
    FOnSaveAs: TNotifyEvent;
    FOnOpen: TNotifyEvent;
    FOnEmail: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnFax: TNotifyEvent;
    FOnPrint: TNotifyEvent;
    FOnCopyFilename: TNotifyEvent;
    FOnOptions : TNotifyEvent;
    FOnHelpIndex : TNotifyEvent;
    FOnHelpHome : TNotifyEvent;
    FOnHelpVersion : TNotifyEvent;
    FOnHelpSupportCase : TNotifyEvent;

    Function button(owner : TWinControl; top, left : integer; image : integer; hint : string; enabled : Boolean; captioned : boolean = false) : TSpeedButton;
    Procedure caption(owner : TWinControl; top, width : integer; title : string);

    Procedure WordProcessorObserverNotificationHandler(oSender : TObject);

    Procedure UpdateStatus;
    Procedure Build;
    Procedure LoadWordProcessorData;

    Procedure SetWordProcessor(Const Value: TWordProcessor);

    Procedure DoOperationsMenu(sender : TObject);
    Procedure DoNewOperation(sender : TObject);
    Procedure DoOpenOperation(sender : TObject);
    Procedure DoSaveOperation(sender : TObject);
    Procedure DoSaveAsOperation(sender : TObject);
    Procedure DoPrintOperation(sender : TObject);
    Procedure DoFaxOperation(sender : TObject);
    Procedure DoEmailOperation(sender : TObject);
    Procedure DoExitOperation(sender : TObject);

    Procedure DoEditMenu(sender : TObject);
    Procedure DoCutOperation(sender : TObject);
    Procedure DoCopyOperation(sender : TObject);
    Procedure DoPasteOperation(sender : TObject);
    Procedure DoPasteSpecialOperation(sender : TObject);
    Procedure DoCopyFilenameOperation(sender : TObject);
    Procedure DoUndoOperation(sender : TObject);
    Procedure DoRedoOperation(sender : TObject);
    Procedure DoSearchOperation(sender : TObject);
    Procedure DoReplaceOperation(sender : TObject);
    Procedure DoChangeCaseOperation(sender : TObject);

    Procedure DoFontMenu(sender : TObject);
    Procedure DoBoldButton(sender : TObject);
    Procedure DoItalicButton(sender : TObject);
    Procedure DoUnderlineButton(sender : TObject);
    Procedure DoColourButton(sender : TObject);
    Procedure DoBackgroundButton(sender : TObject);
    Procedure DoSuperscriptButton(sender : TObject);
    Procedure DoSubscriptButton(sender : TObject);
    Procedure DoFontPropertiesButton(sender : TObject);

    Procedure DoParagraphMenu(sender : TObject);
    Procedure DoLeftButton(sender : TObject);
    Procedure DoCenterButton(sender : TObject);
    Procedure DoRightButton(sender : TObject);
    Procedure DoJustifyButton(sender : TObject);
    Procedure DoNormalButton(sender : TObject);
    Procedure DoBulletsButton(sender : TObject);
    Procedure DoNumbersButton(sender : TObject);
    Procedure DoIndentButton(sender : TObject);
    Procedure DoOutdentButton(sender : TObject);
    Procedure DoParaPropsButton(sender : TObject);

    Procedure DoInsertMenu(sender : TObject);
    Procedure DoSymbolButton(sender : TObject);
    Procedure DoTemplateButton(sender : TObject);
    Procedure DoBreakButton(sender : TObject);
    Procedure DoPictureButton(sender : TObject);
    Procedure DoInsLineButton(sender : TObject);
    Procedure DoInsTableButton(sender : TObject);
    Procedure DoInsFieldButton(sender : TObject);

    Procedure DoImageMenu(sender : TObject);
    Procedure DoImageMapButton(sender : TObject);
    Procedure DoImageEditButton(sender : TObject);
    Procedure DoSelectButton(sender : TObject);
    Procedure DoLineButton(sender : TObject);
    Procedure DoRectButton(sender : TObject);
    Procedure DoCircleButton(sender : TObject);
    Procedure DoMarkButton(sender : TObject);
    Procedure DoZoomButton(sender : TObject);

    Procedure DoFieldMenu(sender : TObject);
    Procedure DoNextField(sender : TObject);
    Procedure DoPreviousField(sender : TObject);
    Procedure DoEditField(sender : TObject);
    Procedure DoRemoveField(sender : TObject);

    Procedure DoTableMenu(sender : TObject);
    Procedure DoTableToTextButton(sender : TObject);
    Procedure DoTextToTableButton(sender : TObject);
    Procedure DoTablePropsButton(sender : TObject);
    Procedure DoSelRowButton(sender : TObject);
    Procedure DoSelTableButton(sender : TObject);
    Procedure DoSortTableButton(sender : TObject);
    Procedure DoInsertLeftButton(sender : TObject);
    Procedure DoInsertRightButton(sender : TObject);
    Procedure DoInsertAboveButton(sender : TObject);
    Procedure DoInsertBelowButton(sender : TObject);
    Procedure DoDelColButton(sender : TObject);
    Procedure DoDelRowButton(sender : TObject);
    Procedure DoDelTableButton(sender : TObject);

    Procedure DoToolsMenu(sender : TObject);
    Procedure DoSpellButton(sender : TObject);
    Procedure DoStylesButton(sender : TObject);
    Procedure DoOptionsButton(sender : TObject);

    Procedure DoHelpMenu(sender : TObject);
    Procedure DoHelpIndexButton(sender : TObject);
    Procedure DoHelpHomeButton(sender : TObject);
    Procedure DoHelpVersionButton(sender : TObject);
    Procedure DoHelpSupportCaseButton(sender : TObject);
  Public
    constructor Create(oOwner : TComponent); Override;
    destructor Destroy; Override;

    Procedure Refresh;

    Property VisibleWidgetSet : TWPToolbarWidgets Read FVisibleWidgetSet Write FVisibleWidgetSet;
    Property WordProcessor : TWordProcessor Read FWordProcessor Write SetWordProcessor;

    Property OnNew : TNotifyEvent read FOnNew write FOnNew;
    Property OnOpen : TNotifyEvent read FOnOpen write FOnOpen;
    Property OnSave : TNotifyEvent read FOnSave write FOnSave;
    Property OnSaveAs : TNotifyEvent read FOnSaveAs write FOnSaveAs;
    Property OnPrint : TNotifyEvent read FOnPrint write FOnPrint;
    Property OnFax : TNotifyEvent read FOnFax write FOnFax;
    Property OnEmail : TNotifyEvent read FOnEmail write FOnEmail;
    Property OnExit : TNotifyEvent read FOnExit write FOnExit;
    Property OnOptions : TNotifyEvent read FOnOptions write FOnOptions;
    Property OnCopyFilename : TNotifyEvent read FOnCopyFilename write FOnCopyFilename;
    Property OnHelpIndex : TNotifyEvent read FOnHelpIndex write FOnHelpIndex;
    Property OnHelpHome : TNotifyEvent read FOnHelpHome write FOnHelpHome;
    Property OnHelpVersion : TNotifyEvent read FOnHelpVersion write FOnHelpVersion;
    Property OnHelpSupportCase : TNotifyEvent read FOnHelpSupportCase write FOnHelpSupportCase;
  End;

Const

  WordProcessorTouchToolbarWidgetDefaultSet =
    [
    tbwCut, tbwCopy, tbwPaste, tbwUndo, tbwRedo, tbwSearch, tbwPlayback, tbwSpelling,
    tbwStyle, tbwFontName, tbwFontSize, tbwFontColour, tbwBold, tbwItalic, tbwUnderline,
    tbwLeft, tbwCentre, tbwRight, tbwJustify, tbwBullet, tbwNumber, tbwIndent, tbwOutdent,
    tbwInsertImage, tbwInsertTable, tbwInsertPageBreak, tbwInsertField, tbwMacro];

  WordProcessorTouchToolbarWidgetUniversalSet = [Low(TWPToolbarWidget)..High(TWPToolbarWidget)];

  WordProcessorTouchToolbarWidgetCaptionArray : Array [TWPToolbarWidget] Of String = (
    'Cut', 'Copy', 'Copy All', 'Paste', 'Paste Special',
    'Undo', 'Redo', 'Search', 'Replay Dictation', 'Spelling', 'Style', 'Font Name', 'Font Size',
    'Font Colour', 'Background Color', 'Bold', 'Italic', 'Underline', 'Superscript', 'Subscript', 'ChangeCase',
    'Left Justify', 'Centre Justify', 'Right Justify', 'Block Justify', 'Bulleted', 'Numbered', 'Indent Paragraph', 'Unindent Paragraph',
    'Insert Table', 'Insert Image', 'Insert Horizontal Line', 'Insert Page Break', 'Show Paragraph markers', 'Expand as a Macro', 'Insert Field', 'Insert Template',
    'Insert Symbol', 'New', 'Open', 'Save', 'Save As', 'Print', 'Import', 'Export', 'Page Design'
    );



Implementation

Uses
  FHIR.WP.Engine;


{$R resources\FHIR.WP.Toolbar.ImageSet.Res}
{$R resources\FHIR.WP.Toolbar.Touch.ImageSet.Res}


{ TWordProcessorToolbar }


Constructor TWordProcessorToolbar.Create(oOwner: TComponent);
Begin
  Inherited;

  FWordProcessor := Nil;

  VisibleWidgetSet := WordProcessorToolbarWidgetDefaultSet;

  PresentationEntity.SpecificationEntity.IconSize := 16;
  PresentationEntity.SpecificationEntity.AllowWrapping := True;

  PresentationEntity.MarginLeft := 3;
  PresentationEntity.MarginRight := 3;
  PresentationEntity.MarginTop := 3;
  PresentationEntity.MarginBottom := 3;
End;


Destructor TWordProcessorToolbar.Destroy;
Begin
  FWordProcessor := Nil;

  Inherited;
End;


Procedure TWordProcessorToolbar.Build;
Var
  oAdvancedToolbarGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
  oAdvancedToolbarSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;
Begin
  PresentationEntity.GroupEntityList.Clear;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Clipboard';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FNewComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FNewComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwNew];
  FNewComponentEntity.HasButtonEntity := True;
  FNewComponentEntity.ButtonEntity.HasBitmapImage := True;
  FNewComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('New');
  FNewComponentEntity.ButtonEntity.ClickHandler := NewButtonClickHandler;

  FOpenComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FOpenComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwOpen];
  FOpenComponentEntity.HasButtonEntity := True;
  FOpenComponentEntity.ButtonEntity.HasBitmapImage := True;
  FOpenComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Open');
  FOpenComponentEntity.ButtonEntity.ClickHandler := OpenButtonClickHandler;

  FSaveComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSaveComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwSave];
  FSaveComponentEntity.HasButtonEntity := True;
  FSaveComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSaveComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Save');
  FSaveComponentEntity.ButtonEntity.ClickHandler := SaveButtonClickHandler;

  FSaveAsComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSaveAsComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwSaveAs];
  FSaveAsComponentEntity.HasButtonEntity := True;
  FSaveAsComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSaveAsComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('SaveAs');
  FSaveAsComponentEntity.ButtonEntity.ClickHandler := SaveAsButtonClickHandler;

  FPrintComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FPrintComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwPrint];
  FPrintComponentEntity.HasButtonEntity := True;
  FPrintComponentEntity.ButtonEntity.HasBitmapImage := True;
  FPrintComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Print');
  FPrintComponentEntity.ButtonEntity.ClickHandler := PrintButtonClickHandler;

  FPageDesignComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FPageDesignComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwPageDesign];
  FPageDesignComponentEntity.HasButtonEntity := True;
  FPageDesignComponentEntity.ButtonEntity.HasBitmapImage := True;
  FPageDesignComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('PageDesign');
  FPageDesignComponentEntity.ButtonEntity.ClickHandler := PageDesignButtonClickHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FCutComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FCutComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwCut];
  FCutComponentEntity.HasButtonEntity := True;
  FCutComponentEntity.ButtonEntity.HasBitmapImage := True;
  FCutComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Cut');
  FCutComponentEntity.ButtonEntity.ClickHandler := CutButtonClickHandler;

  FCopyComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FCopyComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwCopy];
  FCopyComponentEntity.HasButtonEntity := True;
  FCopyComponentEntity.ButtonEntity.HasBitmapImage := True;
  FCopyComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Copy');
  FCopyComponentEntity.ButtonEntity.ClickHandler := CopyButtonClickHandler;

  FCopyAllComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FCopyAllComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwCopyAll];
  FCopyAllComponentEntity.HasButtonEntity := True;
  FCopyAllComponentEntity.ButtonEntity.HasBitmapImage := True;
  FCopyAllComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Copy');
  FCopyAllComponentEntity.ButtonEntity.ClickHandler := CopyAllButtonClickHandler;

  FPasteComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FPasteComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwPaste];
  FPasteComponentEntity.HasButtonEntity := True;
  FPasteComponentEntity.ButtonEntity.HasBitmapImage := True;
  FPasteComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Paste');
  FPasteComponentEntity.ButtonEntity.ClickHandler := PasteButtonClickHandler;

  FPasteSpecialComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FPasteSpecialComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwPasteSpecial];
  FPasteSpecialComponentEntity.HasButtonEntity := True;
  FPasteSpecialComponentEntity.ButtonEntity.HasBitmapImage := True;
  FPasteSpecialComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Paste');
  FPasteSpecialComponentEntity.ButtonEntity.ClickHandler := PasteSpecialButtonClickHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FUndoComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FUndoComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwUndo];
  FUndoComponentEntity.HasButtonEntity := True;
  FUndoComponentEntity.ButtonEntity.HasBitmapImage := True;
  FUndoComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Undo');
  FUndoComponentEntity.ButtonEntity.ClickHandler := UndoButtonClickHandler;

  FRedoComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FRedoComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwRedo];
  FRedoComponentEntity.HasButtonEntity := True;
  FRedoComponentEntity.ButtonEntity.HasBitmapImage := True;
  FRedoComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Redo');
  FRedoComponentEntity.ButtonEntity.ClickHandler := RedoButtonClickHandler;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Font';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FStyleComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FStyleComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwStyle];
  FStyleComponentEntity.HasControlEntity := True;
  FStyleComboBox := TUixAdvancedComboBox.Create(Self);
  FStyleComboBox.Parent := Self;
  FStyleComboBox.Font.Color := clBlack;
  FStyleComponentEntity.ControlEntity.Control := FStyleComboBox;
  FStyleComponentEntity.ControlEntity.MinimumWidth := 50;
  FStyleComponentEntity.ControlEntity.MaximumWidth := 80;
  FStyleComboBox.OnChange := StyleChangeHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FFontNameComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FFontNameComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwFontName];
  FFontNameComponentEntity.HasControlEntity := True;
  FFontNameComboBox := TUixAdvancedFontComboBox.Create(Self);
  FFontNameComboBox.Parent := Self;
  FFontNameComboBox.Align := alNone;
  FFontNameComboBox.Height := 15;
  FFontNameComboBox.DropDownWidth := 280;
  FFontNameComboBox.OnChange := FontNameChangeHandler;
  FFontNameComboBox.Font.Color := clBlack;
  FFontNameComponentEntity.ControlEntity.Control := FFontNameComboBox;
  FFontNameComponentEntity.ControlEntity.MinimumWidth := 50;
  FFontNameComponentEntity.ControlEntity.MaximumWidth := 140;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FFontSizeComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FFontSizeComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwFontSize];
  FFontSizeComponentEntity.HasControlEntity := True;
  FFontSizeComboBox := TUixAdvancedComboBox.Create(Self);
  FFontSizeComboBox.Parent := Self;
  FFontSizeComboBox.Align := alNone;
  FFontSizeComboBox.Style := csDropDown;
  FFontSizeComboBox.Font.Color := clBlack;
  FFontSizeComboBox.OnChange := FontSizeChangeHandler;
  FFontSizeComponentEntity.ControlEntity.Control := FFontSizeComboBox;
  FFontSizeComponentEntity.ControlEntity.MinimumWidth := 50;
  FFontSizeComponentEntity.ControlEntity.MaximumWidth := 50;

  FFontSizeComboBox.Items.Clear;
  FFontSizeComboBox.Items.Add('4');
  FFontSizeComboBox.Items.Add('5');
  FFontSizeComboBox.Items.Add('6');
  FFontSizeComboBox.Items.Add('7');
  FFontSizeComboBox.Items.Add('8');
  FFontSizeComboBox.Items.Add('9');
  FFontSizeComboBox.Items.Add('10');
  FFontSizeComboBox.Items.Add('11');
  FFontSizeComboBox.Items.Add('12');
  FFontSizeComboBox.Items.Add('13');
  FFontSizeComboBox.Items.Add('14');
  FFontSizeComboBox.Items.Add('15');
  FFontSizeComboBox.Items.Add('16');
  FFontSizeComboBox.Items.Add('17');
  FFontSizeComboBox.Items.Add('18');
  FFontSizeComboBox.Items.Add('19');
  FFontSizeComboBox.Items.Add('20');
  FFontSizeComboBox.Items.Add('21');
  FFontSizeComboBox.Items.Add('22');
  FFontSizeComboBox.Items.Add('24');
  FFontSizeComboBox.Items.Add('26');
  FFontSizeComboBox.Items.Add('28');
  FFontSizeComboBox.Items.Add('30');
  FFontSizeComboBox.Items.Add('32');
  FFontSizeComboBox.Items.Add('34');
  FFontSizeComboBox.Items.Add('36');
  FFontSizeComboBox.Items.Add('38');
  FFontSizeComboBox.Items.Add('40');
  FFontSizeComboBox.Items.Add('44');
  FFontSizeComboBox.Items.Add('48');
  FFontSizeComboBox.Items.Add('52');
  FFontSizeComboBox.Items.Add('56');
  FFontSizeComboBox.Items.Add('60');
  FFontSizeComboBox.Items.Add('64');
  FFontSizeComboBox.Items.Add('72');
  FFontSizeComboBox.Items.Add('80');
  FFontSizeComboBox.Items.Add('96');
  FFontSizeComboBox.Items.Add('128');
  FFontSizeComboBox.Items.Add('172');

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FBackColourComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FBackColourComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwBackColor];
  FBackColourComponentEntity.HasControlEntity:= True;
  FBackColourComboBox := TUixAdvancedColourComboBox.Create(Self);
  FBackColourComboBox.Parent := Self;
  FBackColourComboBox.OnChange := BackColourChangeHandler;
  FBackColourComboBox.Height := 15;
  FBackColourComponentEntity.ControlEntity.Control := FBackColourComboBox;
  FBackColourComponentEntity.ControlEntity.MinimumWidth := 50;
  FBackColourComponentEntity.ControlEntity.MaximumWidth := 50;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FFontColourComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FFontColourComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwFontColour];
  FFontColourComponentEntity.HasControlEntity := True;
  FFontColourComboBox := TUixAdvancedColourComboBox.Create(Self);
  FFontColourComboBox.Parent := Self;
  FFontColourComboBox.Height := 15;
  FFontColourComboBox.OnChange := FontColourChangeHandler;
  FFontColourComponentEntity.ControlEntity.Control := FFontColourComboBox;
  FFontColourComponentEntity.ControlEntity.MinimumWidth := 45;
  FFontColourComponentEntity.ControlEntity.MaximumWidth := 45;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FBoldComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FBoldComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwBold];
  FBoldComponentEntity.HasButtonEntity := True;
  FBoldComponentEntity.ButtonEntity.HasBitmapImage := True;
  FBoldComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Font_Bold');
  FBoldComponentEntity.ButtonEntity.ClickHandler := BoldButtonClickHandler;

  FItalicComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FItalicComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwItalic];
  FItalicComponentEntity.HasButtonEntity := True;
  FItalicComponentEntity.ButtonEntity.HasBitmapImage := True;
  FItalicComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Font_Italics');
  FItalicComponentEntity.ButtonEntity.ClickHandler := ItalicsButtonClickHandler;

  FUnderlineComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FUnderlineComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwUnderline];
  FUnderlineComponentEntity.HasButtonEntity := True;
  FUnderlineComponentEntity.ButtonEntity.HasBitmapImage := True;
  FUnderlineComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Font_Underline');
  FUnderlineComponentEntity.ButtonEntity.ClickHandler := UnderlineButtonClickHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FChangeCaseComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FChangeCaseComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwChangeCase];
  FChangeCaseComponentEntity.HasButtonEntity := True;
  FChangeCaseComponentEntity.ButtonEntity.HasBitmapImage := True;
  FChangeCaseComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Font_Change_Case');
  FChangeCaseComponentEntity.ButtonEntity.ClickHandler := CaseButtonClickHandler;

  FSubScriptComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSubScriptComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwSubscript];
  FSubScriptComponentEntity.HasButtonEntity := True;
  FSubScriptComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSubScriptComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Copy');
  FSubScriptComponentEntity.ButtonEntity.ClickHandler := SubscriptButtonClickHandler;

  FSuperScriptComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSuperScriptComponentEntity.Caption := WordProcessorToolbarWidgetCaptionArray[tbwSuperscript];
  FSuperScriptComponentEntity.HasButtonEntity := True;
  FSuperScriptComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSuperScriptComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Copy');
  FSuperScriptComponentEntity.ButtonEntity.ClickHandler := SuperscriptButtonClickHandler;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Paragraph';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FLeftComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FLeftComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwLeft];
  FLeftComponentEntity.HasButtonEntity := True;
  FLeftComponentEntity.ButtonEntity.HasBitmapImage := True;
  FLeftComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Align_Text_Left');
  FLeftComponentEntity.ButtonEntity.ClickHandler := AlignLeftButtonClickHandler;

  FCentreComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FCentreComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwCentre];
  FCentreComponentEntity.HasButtonEntity := True;
  FCentreComponentEntity.ButtonEntity.HasBitmapImage := True;
  FCentreComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Align_Text_Center');
  FCentreComponentEntity.ButtonEntity.ClickHandler := AlignCentreButtonClickHandler;

  FRightComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FRightComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwRight];
  FRightComponentEntity.HasButtonEntity := True;
  FRightComponentEntity.ButtonEntity.HasBitmapImage := True;
  FRightComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Align_Text_Right');
  FRightComponentEntity.ButtonEntity.ClickHandler := AlignRightButtonClickHandler;

  FJustifyComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FJustifyComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwJustify];
  FJustifyComponentEntity.HasButtonEntity := True;
  FJustifyComponentEntity.ButtonEntity.HasBitmapImage := True;
  FJustifyComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Align_Text_Justify');
  FJustifyComponentEntity.ButtonEntity.ClickHandler := AlignJustifyButtonClickHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FOutdentComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FOutdentComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwOutdent];
  FOutdentComponentEntity.HasButtonEntity := True;
  FOutdentComponentEntity.ButtonEntity.HasBitmapImage := True;
  FOutdentComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Indent_Decrease');
  FOutdentComponentEntity.ButtonEntity.ClickHandler := OutdentButtonClickHandler;

  FIndentComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FIndentComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwIndent];
  FIndentComponentEntity.HasButtonEntity := True;
  FIndentComponentEntity.ButtonEntity.HasBitmapImage := True;
  FIndentComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Indent_Increase');
  FIndentComponentEntity.ButtonEntity.ClickHandler := IndentButtonClickHandler;

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FNumbersComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FNumbersComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwNumber];
  FNumbersComponentEntity.HasButtonEntity := True;
  FNumbersComponentEntity.ButtonEntity.HasBitmapImage := True;
  FNumbersComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Numbering');
  FNumbersComponentEntity.ButtonEntity.ClickHandler := NumbersButtonClickHandler;

  FBulletsComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FBulletsComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwBullet];
  FBulletsComponentEntity.HasButtonEntity := True;
  FBulletsComponentEntity.ButtonEntity.HasBitmapImage := True;
  FBulletsComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Bullets_Left');
  FBulletsComponentEntity.ButtonEntity.ClickHandler := BulletsButtonClickHandler;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Editing';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FSpellingComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSpellingComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwSpelling];
  FSpellingComponentEntity.HasButtonEntity := True;
  FSpellingComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSpellingComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('SpellCheck');
  FSpellingComponentEntity.ButtonEntity.ClickHandler := SpellingButtonClickHandler;

  FSearchComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FSearchComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwSearch];
  FSearchComponentEntity.HasButtonEntity := True;
  FSearchComponentEntity.ButtonEntity.HasBitmapImage := True;
  FSearchComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Search');
  FSearchComponentEntity.ButtonEntity.ClickHandler := SearchButtonClickHandler;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Insert';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FInsertFieldPopupMenu := TUixPopupMenu.Create(Self);

  FInsertFieldComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertFieldComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertField];
  FInsertFieldComponentEntity.HasButtonEntity := True;
  FInsertFieldComponentEntity.ButtonEntity.DropDownPopup := FInsertFieldPopupMenu;
  FInsertFieldComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertFieldComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Insert_Field');
  FInsertFieldComponentEntity.ButtonEntity.ClickHandler := InsertFieldDefaultButtonClickHandler;

  FInsertTemplateComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertTemplateComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertTemplate];
  FInsertTemplateComponentEntity.HasButtonEntity := True;
  FInsertTemplateComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertTemplateComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Insert_Template');
  FInsertTemplateComponentEntity.ButtonEntity.ClickHandler := InsertTemplateButtonClickHandler;

  FInsertPageBreakComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertPageBreakComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertPageBreak];
  FInsertPageBreakComponentEntity.HasButtonEntity := True;
  FInsertPageBreakComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertPageBreakComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Insert_Page_Break');
  FInsertPageBreakComponentEntity.ButtonEntity.ClickHandler := InsertPageBreakButtonClickHandler;

  FInsertImageComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertImageComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertImage];
  FInsertImageComponentEntity.HasButtonEntity := True;
  FInsertImageComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertImageComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Insert_Image');
  FInsertImageComponentEntity.ButtonEntity.ClickHandler := InsertImageButtonClickHandler;

  FInsertTableComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertTableComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertTable];
  FInsertTableComponentEntity.HasButtonEntity := True;
  FInsertTableComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertTableComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Insert_Table');
  FInsertTableComponentEntity.ButtonEntity.ClickHandler := InsertTableButtonClickHandler;

  FInsertLineComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FInsertLineComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwInsertLine];
  FInsertLineComponentEntity.HasButtonEntity := True;
  FInsertLineComponentEntity.ButtonEntity.HasBitmapImage := True;
  FInsertLineComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Horizontal_Line');
  FInsertLineComponentEntity.ButtonEntity.ClickHandler := InsertLineButtonClickHandler;

  FEditHintsComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FEditHintsComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwEditHints];
  FEditHintsComponentEntity.HasButtonEntity := True;
  FEditHintsComponentEntity.ButtonEntity.HasBitmapImage := True;
  FEditHintsComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Copy');
  FEditHintsComponentEntity.ButtonEntity.ClickHandler := HintsButtonClickHandler;

  oAdvancedToolbarGroupEntity := PresentationEntity.AddNewGroupEntity;
  oAdvancedToolbarGroupEntity.Caption := 'Voice';

  oAdvancedToolbarSectionEntity := oAdvancedToolbarGroupEntity.AddNewSectionEntity;

  FMacroComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FMacroComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwMacro];
  FMacroComponentEntity.HasButtonEntity := True;
  FMacroComponentEntity.ButtonEntity.HasBitmapImage := True;
  FMacroComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Voice_Macro');
  FMacroComponentEntity.ButtonEntity.ClickHandler := MacroButtonClickHandler;

  FPlaybackComponentEntity := oAdvancedToolbarSectionEntity.AddNewComponentEntity;
  FPlaybackComponentEntity.Caption := CAPTIONS_TOOLBARWIDGET[tbwPlayback];
  FPlaybackComponentEntity.HasButtonEntity := True;
  FPlaybackComponentEntity.ButtonEntity.HasBitmapImage := True;
  FPlaybackComponentEntity.ButtonEntity.BitmapImage.LoadPNGFromResource('Voice_Speaker');
  FPlaybackComponentEntity.ButtonEntity.ClickHandler := VoicePlaybackButtonClickHandler;

  Execute;
End;


Procedure TWordProcessorToolbar.LoadWordProcessorData;
Var
  oFontNameList : TStringList;
  iFontNameIndex : Integer;
  iStyleIndex : Integer;
Begin
  Assert(Invariants('LoadWordProcessorData', FWordProcessor, TWordProcessor, 'FWordProcessor'));

  FFontNameComboBox.Items.BeginUpdate;
  Try
    FFontNameComboBox.Items.Clear;

    oFontNameList := FWordProcessor.ListAllFonts;
    Try
      For iFontNameIndex := 0 To oFontNameList.Count - 1 Do
        FFontNameComboBox.Items.Add(oFontNameList[iFontNameIndex]);
    Finally
      oFontNameList.Free;
    End;

    FFontNameComboBox.Cache;
  Finally
    FFontNameComboBox.Items.EndUpdate;
  End;

  FStyleComboBox.Items.BeginUpdate;
  Try
    FStyleComboBox.Items.Clear;

    If FWordProcessor.HasWorkingStyles Then
    Begin
      For iStyleIndex := 0 To FWordProcessor.WorkingStyles.Count - 1 Do
        FStyleComboBox.Items.Add(FWordProcessor.WorkingStyles[iStyleIndex].Name);
    End;
  Finally
    FStyleComboBox.Items.EndUpdate;
  End;
End;


Procedure TWordProcessorToolbar.WordProcessorObserverNotificationHandler(oSender : TObject);
Begin
  UpdateStatus;
End;


Procedure TWordProcessorToolbar.UpdateStatus;
Var
  oWPStyle : TWPStyle;
  oWPFont : TWPSFontDetails;
  oWPParagraph : TWPSParagraphDetails;

  aRangeCapabilities : TWPCapabilities;
  iFieldDefinitionIndex : Integer;
  oDefinition : TWPFieldDefinitionProvider;
  iIconIndex : Integer;
  oMenu : TUixMenuItem;
  iStyleIndex : Integer;
Begin
  NewComponentEntity.IsVisible := (tbwNew in VisibleWidgetSet) and assigned(FOnNew);
  OpenComponentEntity.IsVisible := (tbwOpen in VisibleWidgetSet) and assigned(FOnOpen);
  SaveComponentEntity.IsVisible := (tbwSave in VisibleWidgetSet) and assigned(FOnSave);
  SaveAsComponentEntity.IsVisible := (tbwSaveAs in VisibleWidgetSet) and assigned(FOnSaveAs);
  PrintComponentEntity.IsVisible := (tbwPrint in VisibleWidgetSet) and assigned(FOnPrint);
  PageDesignComponentEntity.IsVisible := (tbwPageDesign in VisibleWidgetSet) and assigned(FOnPageDesign);
  FPasteSpecialComponentEntity.IsVisible := tbwPasteSpecial In VisibleWidgetSet;
  FPasteComponentEntity.IsVisible := tbwPaste In VisibleWidgetSet;
  FCopyAllComponentEntity.IsVisible := tbwCopyAll In VisibleWidgetSet;
  FCopyComponentEntity.IsVisible := tbwCopy In VisibleWidgetSet;
  FCutComponentEntity.IsVisible := tbwCut In VisibleWidgetSet;
  FUndoComponentEntity.IsVisible := tbwUndo In VisibleWidgetSet;
  FRedoComponentEntity.IsVisible := tbwRedo In VisibleWidgetSet;
  FStyleComponentEntity.IsVisible := tbwStyle In VisibleWidgetSet;
  FFontNameComponentEntity.IsVisible := tbwFontName In VisibleWidgetSet;
  FFontSizeComponentEntity.IsVisible := tbwFontSize In VisibleWidgetSet;
  FBackColourComponentEntity.IsVisible := tbwBackColor In VisibleWidgetSet;
  FFontColourComponentEntity.IsVisible := tbwFontColour In VisibleWidgetSet;
  FBoldComponentEntity.IsVisible := tbwBold In VisibleWidgetSet;
  FItalicComponentEntity.IsVisible := tbwItalic In VisibleWidgetSet;
  FUnderlineComponentEntity.IsVisible := tbwUnderline In VisibleWidgetSet;
  FChangeCaseComponentEntity.IsVisible := tbwChangeCase In VisibleWidgetSet;
  FSubScriptComponentEntity.IsVisible := tbwSubscript In VisibleWidgetSet;
  FSuperScriptComponentEntity.IsVisible := tbwSuperscript In VisibleWidgetSet;
  FLeftComponentEntity.IsVisible := tbwLeft In VisibleWidgetSet;
  FCentreComponentEntity.IsVisible := tbwCentre In VisibleWidgetSet;
  FRightComponentEntity.IsVisible := tbwRight In VisibleWidgetSet;
  FJustifyComponentEntity.IsVisible := tbwJustify In VisibleWidgetSet;
  FOutdentComponentEntity.IsVisible := tbwOutdent In VisibleWidgetSet;
  FIndentComponentEntity.IsVisible := tbwIndent In VisibleWidgetSet;
  FNumbersComponentEntity.IsVisible := tbwNumber In VisibleWidgetSet;
  FBulletsComponentEntity.IsVisible := tbwBullet In VisibleWidgetSet;
  FSpellingComponentEntity.IsVisible := tbwSpelling In VisibleWidgetSet;
  FSearchComponentEntity.IsVisible := tbwSearch In VisibleWidgetSet;
  FMacroComponentEntity.IsVisible := tbwMacro In VisibleWidgetSet;
  FInsertFieldComponentEntity.IsVisible := tbwInsertField In VisibleWidgetSet;
  FInsertTemplateComponentEntity.IsVisible := tbwInsertTemplate In VisibleWidgetSet;
  FInsertPageBreakComponentEntity.IsVisible := tbwInsertPageBreak In VisibleWidgetSet;
  FInsertImageComponentEntity.IsVisible := tbwInsertImage In VisibleWidgetSet;
  FInsertTableComponentEntity.IsVisible := tbwInsertTable In VisibleWidgetSet;
  FInsertLineComponentEntity.IsVisible := tbwInsertTable In VisibleWidgetSet;
  FEditHintsComponentEntity.IsVisible := tbwEditHints In VisibleWidgetSet;
  FPlaybackComponentEntity.IsVisible := tbwPlayback In VisibleWidgetSet;

  FStyleComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FFontNameComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FFontSizeComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FBackColourComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FFontColourComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FChangeCaseComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FSubScriptComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FSuperScriptComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FLeftComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FCentreComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FRightComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FJustifyComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FOutdentComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FIndentComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FNumbersComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FBulletsComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertFieldComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertTemplateComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertPageBreakComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertImageComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertTableComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;
  FInsertLineComponentEntity.IsVisible := Not WordProcessor.Settings.ConsoleMode;

  If Assigned(FWordProcessor) Then
  Begin
    FStyleComboBox.Items.BeginUpdate;
    Try
      FStyleComboBox.Items.Clear;

      If FWordProcessor.HasWorkingStyles Then
      Begin
        For iStyleIndex := 0 To FWordProcessor.WorkingStyles.Count - 1 Do
          If FWordProcessor.WorkingStyles[iStyleIndex].HasParagraphAspect Then
            FStyleComboBox.Items.Add(FWordProcessor.WorkingStyles[iStyleIndex].Name + ' 6')
          Else
            FStyleComboBox.Items.Add(FWordProcessor.WorkingStyles[iStyleIndex].Name);
      End;
    Finally
      FStyleComboBox.Items.EndUpdate;
    End;

    oWPStyle := FWordProcessor.WorkingStyles.GetByName(FWordProcessor.PrimaryRange.Style);

    If Not Assigned(oWPStyle) Then
      oWPStyle := FWordProcessor.WorkingStyles.DefaultStyle;

    Assert(Invariants('LoadWordProcessorData', oWPStyle, TWPStyle, 'oWPStyle'));

    If Assigned(oWPStyle) Then
    Begin
      FStyleComboBox.ItemIndex := FWordProcessor.WorkingStyles.IndexByName(oWPStyle.Name);

      FFontNameComboBox.Text := oWPStyle.Font.Name;

      oWPFont := FWordProcessor.PrimaryRange.Font;

      FFontNameComboBox.Text := oWPStyle.WorkingFontName(oWPFont);
      FFontSizeComboBox.Text := IntToStr(oWPStyle.WorkingFontSize(oWPFont));
      FFontColourComboBox.Value := oWPStyle.WorkingFontForeground(oWPFont);
      FBackColourComboBox.Value := oWPStyle.WorkingFontBackground(oWPFont, FWordProcessor.Color);

      FBoldComponentEntity.ButtonEntity.IsToggled := oWPStyle.WorkingFontBold(oWPFont) = tsTrue;
      FItalicComponentEntity.ButtonEntity.IsToggled := oWPStyle.WorkingFontItalic(oWPFont) = tsTrue;
      FUnderlineComponentEntity.ButtonEntity.IsToggled := oWPStyle.WorkingFontUnderline(oWPFont) = tsTrue;
      FSuperscriptComponentEntity.ButtonEntity.IsToggled := oWPStyle.WorkingFontState(oWPFont) = fsSuperscript;
      FSubscriptComponentEntity.ButtonEntity.IsToggled := oWPStyle.WorkingFontState(oWPFont) = fsSubscript;
    End;

    oWPParagraph := FWordProcessor.PrimaryRange.Paragraph;

    Assert(Invariants('LoadWordProcessorData', oWPParagraph, TWPSParagraphDetails, 'oWPParagraph'));

    FLeftComponentEntity.ButtonEntity.IsToggled := oWPParagraph.Align = WordProcessorParagraphAlignmentLeft;
    FRightComponentEntity.ButtonEntity.IsToggled := oWPParagraph.Align = WordProcessorParagraphAlignmentRight;
    FCentreComponentEntity.ButtonEntity.IsToggled := oWPParagraph.Align = WordProcessorParagraphAlignmentCentre;
    FJustifyComponentEntity.ButtonEntity.IsToggled := oWPParagraph.Align = WordProcessorParagraphAlignmentJustify;

    FBulletsComponentEntity.ButtonEntity.IsToggled := oWPParagraph.ListType = WPSParagraphListTypeBullets;
    FNumbersComponentEntity.ButtonEntity.IsToggled := oWPParagraph.ListType = WPSParagraphListTypeNumbers;

    FOutdentComponentEntity.IsEnabled := (oWPParagraph.LeftIndent > 0);

    aRangeCapabilities := FWordProcessor.PrimaryRange.Capabilities;

    FEditHintsComponentEntity.ButtonEntity.IsToggled := FWordProcessor.Settings.EditHints;
    FCutComponentEntity.IsEnabled := canCut In aRangeCapabilities;
    FCopyComponentEntity.IsEnabled := canWrite In aRangeCapabilities;
    FPasteComponentEntity.IsEnabled := (canInsert In aRangeCapabilities) And Clipboard.CanPaste;
    FPasteSpecialComponentEntity.IsEnabled := (canInsert In aRangeCapabilities) And Clipboard.CanPaste;
    FUndoComponentEntity.IsEnabled := canUndo In aRangeCapabilities;
    FRedoComponentEntity.IsEnabled := canRedo In aRangeCapabilities;
    FSearchComponentEntity.IsEnabled := FWordProcessor.Settings.Search;
    FMacroComponentEntity.IsEnabled := (canInsert In aRangeCapabilities) And Assigned(FWordProcessor.OnCodeCompletion);
    FPlaybackComponentEntity.IsEnabled := FWordProcessor.HasVoicePlayback;
    FSpellingComponentEntity.IsEnabled := Not FWordProcessor.Settings.ReadOnly;
    FStyleComboBox.Enabled := canFormat In aRangeCapabilities;
    FFontNameComboBox.Enabled := canFormat In aRangeCapabilities;
    FFontSizeComboBox.Enabled := canFormat In aRangeCapabilities;
    FFontColourComboBox.Enabled := canFormat In aRangeCapabilities;
    FBackColourComboBox.Enabled := canFormat In aRangeCapabilities;
    FBoldComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FItalicComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FUnderlineComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FSuperscriptComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FSubscriptComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FChangeCaseComponentEntity.IsEnabled := (canFormat In aRangeCapabilities) And FWordProcessor.PrimaryRange.Selection.HasSelection;
    FLeftComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FRightComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FCentreComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FJustifyComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FBulletsComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FNumbersComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FIndentComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FOutdentComponentEntity.IsEnabled := canFormat In aRangeCapabilities;
    FInsertTableComponentEntity.IsEnabled := canInsertTable In aRangeCapabilities;
    FInsertImageComponentEntity.IsEnabled := canInsertImage In aRangeCapabilities;
    FInsertLineComponentEntity.IsEnabled := canInsertLine In aRangeCapabilities;
    FInsertPageBreakComponentEntity.IsEnabled := canInsertPageBreak In aRangeCapabilities;

    If FInsertFieldComponentEntity.IsVisible Then
    Begin
      FInsertFieldPopupMenu.Items.Clear;

      For iFieldDefinitionIndex := 0 To FWordProcessor.Settings.FieldDefinitions.Count - 1 Do
      Begin
        oDefinition := FWordProcessor.Settings.FieldDefinitions[iFieldDefinitionIndex];
        iIconIndex := oDefinition.GetStdIconIndex;

        If iIconIndex = -1 Then
          iIconIndex := WPIconModule.NONE;

        oMenu := TUixMenuItem.Create(Self);
        oMenu.Caption := oDefinition.GetTitle;
        oMenu.OnClick := InsertFieldMenuItemClickHandler;
        oMenu.ImageIndex := iIconIndex;
        oMenu.Tag := iFieldDefinitionIndex;
        oMenu.Enabled := oDefinition.CanInsertField;

        FInsertFieldPopupMenu.Items.Add(oMenu);
      End;
    End;
  End;

  Invalidate;
End;


Procedure TWordProcessorToolbar.FontNameChangeHandler(oSender : TObject);
Begin
  If FFontNameComboBox.Items.IndexOf(FFontNameComboBox.Text) > -1 Then
  Begin
    FWordProcessor.PrimaryRange.ApplyFontName(FFontNameComboBox.Text);
    FWordProcessor.SetFocus;
  End;
End;


Procedure TWordProcessorToolbar.FontSizeChangeHandler(oSender: TObject);
Begin
  If StringIsInteger32(FFontSizeComboBox.Text) Then
    FWordProcessor.PrimaryRange.ApplyFontSize(StringToInteger32(FFontSizeComboBox.Text));

  FWordProcessor.SetFocus;
End;


Procedure TWordProcessorToolbar.AlignCentreButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.AlignCentre;
End;


Procedure TWordProcessorToolbar.AlignJustifyButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.AlignJustify;
End;


Procedure TWordProcessorToolbar.AlignLeftButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.AlignLeft;
End;


Procedure TWordProcessorToolbar.AlignRightButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.AlignRight;
End;


Procedure TWordProcessorToolbar.BoldButtonClickHandler(oSender: TObject);
Begin
  FBoldComponentEntity.ButtonEntity.IsToggled := Not FBoldComponentEntity.ButtonEntity.IsToggled;

  FWordProcessor.PrimaryRange.ApplyBold(FBoldComponentEntity.ButtonEntity.IsToggled);

  Refresh;
End;


Procedure TWordProcessorToolbar.BulletsButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.ApplyBullets(FWordProcessor.PrimaryRange.Paragraph.ListType <> WPSParagraphListTypeBullets);
End;


Procedure TWordProcessorToolbar.CaseButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.ChangeCaseDialog;
End;


Procedure TWordProcessorToolbar.CopyAllButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.CopyAllToClipboard;;
End;


Procedure TWordProcessorToolbar.CopyButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.Copy;
End;


Procedure TWordProcessorToolbar.CutButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.Cut;
End;


Procedure TWordProcessorToolbar.IndentButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.ApplyLeftIndent(+1);
End;


Procedure TWordProcessorToolbar.InsertFieldDefaultButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.InsertField(Nil);
End;


Procedure TWordProcessorToolbar.InsertImageButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.InsertImageDialog;
End;


Procedure TWordProcessorToolbar.InsertLineButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.InsertLine;
End;


Procedure TWordProcessorToolbar.InsertPageBreakButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.InsertPageBreak;
End;


Procedure TWordProcessorToolbar.InsertTableButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.InsertTableDialog;
End;


Procedure TWordProcessorToolbar.InsertTemplateButtonClickHandler;
Begin
  FWordProcessor.InsertTemplate;
End;


Procedure TWordProcessorToolbar.ItalicsButtonClickHandler;
Begin
  FItalicComponentEntity.ButtonEntity.IsToggled := Not FItalicComponentEntity.ButtonEntity.IsToggled;

  FWordProcessor.PrimaryRange.ApplyItalic(FItalicComponentEntity.ButtonEntity.IsToggled);

  Refresh;
End;


Procedure TWordProcessorToolbar.MacroButtonClickHandler;
Begin
  FWordProcessor.CodeCompletePrompt;
End;


Procedure TWordProcessorToolbar.NumbersButtonClickHandler;
Begin
  FWordProcessor.PrimaryRange.ApplyNumbers(FWordProcessor.PrimaryRange.Paragraph.ListType <> WPSParagraphListTypeNumbers);
End;


Procedure TWordProcessorToolbar.OutdentButtonClickHandler;
Begin
  FWordProcessor.PrimaryRange.ApplyLeftIndent(-1);
End;


Procedure TWordProcessorToolbar.PasteButtonClickHandler;
Var
  sError : String;
Begin
  If Not FWordProcessor.PrimaryRange.Paste(sError) Then
    DialogError(sError);
End;


Procedure TWordProcessorToolbar.PasteSpecialButtonClickHandler;
Begin
  FWordProcessor.PasteSpecialDialog;
End;


Procedure TWordProcessorToolbar.RedoButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.Redo;
End;


Procedure TWordProcessorToolbar.SearchButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.SearchDialog;
End;


Procedure TWordProcessorToolbar.SpellingButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.CheckSpelling;
End;


Procedure TWordProcessorToolbar.SubscriptButtonClickHandler(oSender: TObject);
Begin
  FSubscriptComponentEntity.ButtonEntity.IsToggled := Not FSubscriptComponentEntity.ButtonEntity.IsToggled;

  FWordProcessor.PrimaryRange.ApplySubscript(FSubscriptComponentEntity.ButtonEntity.IsToggled);
  Refresh;
End;


Procedure TWordProcessorToolbar.SuperscriptButtonClickHandler(oSender: TObject);
Begin
  FSuperscriptComponentEntity.ButtonEntity.IsToggled := Not FSuperscriptComponentEntity.ButtonEntity.IsToggled;
  FWordProcessor.PrimaryRange.ApplySuperscript(FSuperscriptComponentEntity.ButtonEntity.IsToggled);
  Refresh;
End;


Procedure TWordProcessorToolbar.UnderlineButtonClickHandler(oSender: TObject);
Begin
  FUnderlineComponentEntity.ButtonEntity.IsToggled := Not FUnderlineComponentEntity.ButtonEntity.IsToggled;

  FWordProcessor.PrimaryRange.ApplyUnderline(FUnderlineComponentEntity.ButtonEntity.IsToggled);

  Refresh;
End;


Procedure TWordProcessorToolbar.UndoButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.Undo;
End;


Procedure TWordProcessorToolbar.VoicePlaybackButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.VoicePlayback;
End;


Procedure TWordProcessorToolbar.HintsButtonClickHandler(oSender: TObject);
Begin
  FWordProcessor.Settings.EditHints := Not FWordProcessor.Settings.EditHints;
End;


Procedure TWordProcessorToolbar.FontColourChangeHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.ApplyForeground(FFontColourComboBox.Value);
  FWordProcessor.SetFocus;
End;


Procedure TWordProcessorToolbar.BackColourChangeHandler(oSender: TObject);
Begin
  FWordProcessor.PrimaryRange.ApplyBackground(FBackColourComboBox.Value);
  FWordProcessor.SetFocus;
End;


Procedure TWordProcessorToolbar.Refresh;
Begin
  UpdateStatus;
End;


Procedure TWordProcessorToolbar.InsertFieldMenuItemClickHandler(oSender: TObject);
Var
  oDefinition : TWPFieldDefinitionProvider;
Begin
  oDefinition :=  FWordProcessor.Settings.FieldDefinitions[TUixMenuItem(oSender).Tag];

  FWordProcessor.InsertField(oDefinition);
End;


Procedure TWordProcessorToolbar.SetWordProcessor(Const Value: TWordProcessor);
Begin
  FWordProcessor := Value;

  If Assigned(FWordProcessor) Then
  Begin
    Build;
    LoadWordProcessorData;

    FWordProcessor.RegisterObserver(Self, WordProcessorObserverNotificationHandler);
  End;
End;


Procedure TWordProcessorToolbar.StyleChangeHandler(oSender : TObject);
var
  sStyle : String;
Begin
  If FStyleComboBox.Items.IndexOf(FStyleComboBox.Text) > -1 Then
  Begin
    sStyle := FStyleComboBox.Text;
    if StringEndsWith(sStyle, ' 6') Then
      SetLength(sStyle, length(sStyle)-2);
    FWordProcessor.PrimaryRange.Style := sStyle;
    FWordProcessor.SetFocus;
  End;
End;

Procedure TWordProcessorToolbar.NewButtonClickHandler(oSender : TObject);
begin
  if assigned(onNew) then
    onNew(self);
end;

Procedure TWordProcessorToolbar.OpenButtonClickHandler(oSender : TObject);
begin
  if assigned(onOpen) then
    onOpen(self);
end;

Procedure TWordProcessorToolbar.SaveButtonClickHandler(oSender : TObject);
begin
  if assigned(onSave) then
    onSave(Self);
end;

Procedure TWordProcessorToolbar.SaveAsButtonClickHandler(oSender : TObject);
begin
  if assigned(onSaveAs) then
    onSaveAs(Self);
end;

procedure TWordProcessorToolbar.PrintButtonClickHandler(oSender : TObject);
begin
  if assigned(onPrint) then
    onPrint(Self);
end;

Procedure TWordProcessorToolbar.PageDesignButtonClickHandler(oSender : TObject);
begin
  if assigned(onPageDesign) then
    onPageDesign(Self);
end;





{ TWordProcessorTouchToolbar }


Constructor TWordProcessorTouchToolbar.Create(oOwner: TComponent);
Begin
  Inherited;
  FColourDialog := TColorDialog.Create(Owner);
  FColourDialog.Options := [cdFullOpen, cdSolidColor, cdAnyColor];

  FImages := TUixImages.Create(Nil);
  FImages.Height := 32;
  FImages.Width := 32;
  FImages.LoadBitmapFromResource('WordProcessorTouchToolbarImageSet', clFuchsia);

  FWordProcessor := Nil;

  VisibleWidgetSet := WordProcessorTouchToolbarWidgetDefaultSet;

End;


Destructor TWordProcessorTouchToolbar.Destroy;
Begin
  FColourDialog.Free;
  FWordProcessor := Nil;
  FImages.free;

  Inherited;
End;

Function TWordProcessorTouchToolbar.button(owner : TWinControl;  top, left : integer; image : integer; hint : string; enabled : boolean; captioned : boolean) : TSpeedButton;
var
  bmp : TBitmap;
begin
  result := TSpeedButton.create(owner);
  result.Parent := owner;
  result.Left := 4 + left * LEFT_INC_H;
  result.Top := 4 +  top * LEFT_INC_V;
  if captioned then
    result.Top := result.Top + 20;
  result.Height := 58;
  result.Width := 68;
  result.Caption := hint;
  result.Layout := blGlyphTop;
  result.Hint := hint;
  result.Enabled := enabled;
  result.Flat := true;

  bmp := TBitmap.Create;
  try
    FImages.GetBitmap(image, bmp);
    result.Glyph.Assign(bmp);
    result.NumGlyphs := 1;
  finally
    bmp.Free;
  end;
end;

procedure TWordProcessorTouchToolbar.caption(owner: TWinControl; top, width: integer; title: string);
var
  bev : TUixBevel;
  lbl : TUixLabel;
begin
  bev := TUixBevel.Create(owner);
  bev.Parent := owner;
  bev.Top := top * LEFT_INC_V + 14;
  bev.Height := 10;
  bev.Shape := bsTopLine;
  bev.Left := 10;
  bev.Width := width * LEFT_INC_H - 10;
  lbl := TUixLabel.Create(owner);
  lbl.Parent := owner;
  lbl.Top := top * LEFT_INC_V + 7;
  lbl.Height := 20;
  lbl.Left := 2+15;
  lbl.Caption := '  '+title+'  ';
  lbl.Transparent := false;
end;

procedure TWordProcessorTouchToolbar.DoChangeCaseOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ChangeCaseDialog;
end;

procedure TWordProcessorTouchToolbar.DoCopyFilenameOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnCopyFilename(self);
end;

procedure TWordProcessorTouchToolbar.DoCopyOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.Copy;
end;

procedure TWordProcessorTouchToolbar.DoCutOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.Cut;
end;

procedure TWordProcessorTouchToolbar.DoEditField(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.FieldPropertiesDialog;
end;

procedure TWordProcessorTouchToolbar.DoEditMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H;
  FCurrentMenu.Width := LEFT_INC_H * 2 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 5 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_Cut, 'Cut', canCut in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoCutOperation;
  button(FCurrentMenu, 1, 0, Btn_Offs_Copy, 'Copy', canWrite in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoCopyOperation;
  button(FCurrentMenu, 2, 0, Btn_Offs_Paste, 'Paste', canInsert in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoPasteOperation;
  button(FCurrentMenu, 3, 0, Btn_Offs_PasteSpecial, 'Import', canInsert in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoPasteSpecialOperation;
  button(FCurrentMenu, 4, 0, Btn_Offs_CopyFilename, 'Copy name', true).OnClick := DoCopyFilenameOperation;
  button(FCurrentMenu, 0, 1, Btn_Offs_Undo, 'Undo', FWordProcessor.PrimaryRange.CanUndo_).OnClick := DoUndoOperation;
  button(FCurrentMenu, 1, 1, Btn_Offs_Redo, 'Redo', FWordProcessor.PrimaryRange.CanRedo_).OnClick := DoRedoOperation;
  button(FCurrentMenu, 2, 1, Btn_Offs_Search, 'Search', true).OnClick := DoSearchOperation;
  button(FCurrentMenu, 3, 1, Btn_Offs_Replace, 'Replace', true).OnClick := DoReplaceOperation;
  button(FCurrentMenu, 4, 1, Btn_Offs_ChangeCase, 'Case', FWordProcessor.PrimaryRange.Selection.HasSelection).OnClick := DoChangeCaseOperation;
end;

procedure TWordProcessorTouchToolbar.DoEmailOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnEmail(self);
end;

procedure TWordProcessorTouchToolbar.DoExitOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnExit(self);
end;

procedure TWordProcessorTouchToolbar.DoFaxOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnFax(self);
end;

procedure TWordProcessorTouchToolbar.DoFieldMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 6;
  FCurrentMenu.Width := LEFT_INC_H * 2 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 2 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_Previous, 'Previous', canGotoField In FWordProcessor.PrimaryRange.Capabilities).OnClick := DoPreviousField;
  button(FCurrentMenu, 0, 1, Btn_Offs_Next, 'Next', canGotoField In FWordProcessor.PrimaryRange.Capabilities).OnClick := DoNextField;
  button(FCurrentMenu, 1, 0, Btn_Offs_EditField, 'Edit', FWordProcessor.PrimaryRange.HasCurrentFieldStart).OnClick := DoEditField;
  button(FCurrentMenu, 1, 1, Btn_Offs_RemoveField, 'Remove', FWordProcessor.PrimaryRange.HasCurrentFieldStart).OnClick := DoRemoveField;
end;

procedure TWordProcessorTouchToolbar.DoFontMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 2;
  FCurrentMenu.Width := LEFT_INC_H * 2 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 4 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_Bold, 'Bold', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoBoldButton;
  button(FCurrentMenu, 1, 0, Btn_Offs_Italic, 'Italic', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoItalicButton;
  button(FCurrentMenu, 2, 0, Btn_Offs_Underline, 'Underline', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoUnderlineButton;
  button(FCurrentMenu, 3, 0, Btn_Offs_FontProps, 'Properties', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoFontPropertiesButton;
  button(FCurrentMenu, 0, 1, Btn_Offs_Colour, 'Colour', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoColourButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_Background, 'Background', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoBackgroundButton;
  button(FCurrentMenu, 2, 1, Btn_Offs_Superscript, 'Superscript', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSuperscriptButton;
  button(FCurrentMenu, 3, 1, Btn_Offs_Subscript, 'Subscript', canFormat in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSubscriptButton;
end;

procedure TWordProcessorTouchToolbar.DoImageEditButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImagePropertiesDialog;
end;

procedure TWordProcessorTouchToolbar.DoImageMapButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageMapsDialog;
end;

procedure TWordProcessorTouchToolbar.DoImageMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 5;
  FCurrentMenu.Width := LEFT_INC_H * 3 + 8;
  FCurrentMenu.Height := LEFT_INC_V * 3 + 5;

  button(FCurrentMenu, 0, 0, Btn_Offs_Picture, 'Insert', canInsertImage in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoPictureButton;
  button(FCurrentMenu, 1, 0, Btn_Offs_ImageMap, 'Map Editor', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoImageMapButton;
  button(FCurrentMenu, 2, 0, Btn_Offs_ImageEdit, 'Properties', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoImageEditButton;

  button(FCurrentMenu, 0, 1, Btn_Offs_Select, 'Select', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoSelectButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_Line, 'Draw Line', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoLineButton;
  button(FCurrentMenu, 2, 1, Btn_Offs_Rect, 'Draw Rect', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoRectButton;
  button(FCurrentMenu, 0, 2, Btn_Offs_Circle, 'Draw Circle', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoCircleButton;
  button(FCurrentMenu, 1, 2, Btn_Offs_Mark, 'Draw Mark', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoMarkButton;
  button(FCurrentMenu, 2, 2, Btn_Offs_Zoom, 'Zoom', FWordProcessor.PrimaryRange.HasCurrentImage).OnClick := DoZoomButton;


{
Image
Insert
Tool (select | line | rectangle | circle | mark | zoom)
Properties
}

end;

procedure TWordProcessorTouchToolbar.DoInsertMenu(sender: TObject);
var
  i : integer;
  definition : TWPFieldDefinitionProvider;
  btn : TSpeedButton;
begin
  i := IntegerMin(3, FWordProcessor.Settings.FieldDefinitions.Count);

  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 4;
  FCurrentMenu.Width := LEFT_INC_H * 3 + 6;
  if i mod 3 = 0 then
    FCurrentMenu.Height := LEFT_INC_V * (2 + i div 3) + 4 + 20
  else
    FCurrentMenu.Height := LEFT_INC_V * (3 + i div 3) + 4 + 20;

  button(FCurrentMenu, 0, 0, Btn_Offs_Symbol, 'Symbol', canInsert in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSymbolButton;
  button(FCurrentMenu, 0, 1, Btn_Offs_Picture, 'Picture', canInsertImage in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoPictureButton;
  button(FCurrentMenu, 0, 2, Btn_Offs_Break, 'Break', canInsertPageBreak in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoBreakButton;
  button(FCurrentMenu, 1, 0, Btn_Offs_Template, 'Template', canInsert in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoTemplateButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_InsLine, 'Line', canInsertLine in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsLineButton;
  button(FCurrentMenu, 1, 2, Btn_Offs_InsTable, 'Table', canInsertTable in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsTableButton;

  caption(FCurrentMenu, 2, 3, 'Fields');

  for i := 0 To FWordProcessor.Settings.FieldDefinitions.Count - 1 Do
  begin
    definition := FWordProcessor.Settings.FieldDefinitions[i];
    btn := button(FCurrentMenu, 2 + i div 3, i mod 3, definition.GetTouchIconIndex, definition.GetTitle, true, true);
    btn.OnClick := DoInsFieldButton;
    btn.Tag := Integer(definition);
  end;
  // todo : comment;
end;

procedure TWordProcessorTouchToolbar.DoInsFieldButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.InsertField(TWPFieldDefinitionProvider((sender as TComponent).tag));
end;

procedure TWordProcessorTouchToolbar.DoNewOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnNew(self);
end;

procedure TWordProcessorTouchToolbar.DoNextField(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.NextField;
end;

procedure TWordProcessorTouchToolbar.DoOpenOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnOpen(self);
end;

procedure TWordProcessorTouchToolbar.DoOperationsMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4;
  FCurrentMenu.Width := LEFT_INC_H * 2 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 4 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_New, 'New', true).OnClick := DoNewOperation;
  button(FCurrentMenu, 1, 0, Btn_Offs_Open, 'Open', true).OnClick := DoOpenOperation;
  button(FCurrentMenu, 2, 0, Btn_Offs_Save, 'Save', true).OnClick := DoSaveOperation;
  button(FCurrentMenu, 3, 0, Btn_Offs_SaveAs, 'Save As', true).OnClick := DoSaveAsOperation;
  button(FCurrentMenu, 0, 1, Btn_Offs_Print, 'Print', true).OnClick := DoPrintOperation;
  button(FCurrentMenu, 1, 1, Btn_Offs_Fax, 'Fax', true).OnClick := DoFaxOperation;
  button(FCurrentMenu, 2, 1, Btn_Offs_Email, 'Email', true).OnClick := DoEmailOperation;
  button(FCurrentMenu, 3, 1, Btn_Offs_Exit, 'Exit', true).OnClick := DoExitOperation;
end;

procedure TWordProcessorTouchToolbar.DoOptionsButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnOptions(self);
end;

procedure TWordProcessorTouchToolbar.DoParagraphMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 3;
  FCurrentMenu.Width := LEFT_INC_H * 4 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 3 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_Left, 'Left', true).OnClick := DoLeftButton;
  button(FCurrentMenu, 0, 1, Btn_Offs_Center, 'Center', true).OnClick := DoCenterButton;
  button(FCurrentMenu, 0, 2, Btn_Offs_Right, 'Right', true).OnClick := DoRightButton;
  button(FCurrentMenu, 0, 3, Btn_Offs_Justify, 'Justify', true).OnClick := DoJustifyButton;

  button(FCurrentMenu, 1, 0, Btn_Offs_Normal, 'Normal', true).OnClick := DoNormalButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_Bullets, 'Bullets', true).OnClick := DoBulletsButton;
  button(FCurrentMenu, 1, 2, Btn_Offs_Numbers, 'Numbers', true).OnClick := DoNumbersButton;

  button(FCurrentMenu, 2, 0, Btn_Offs_Indent, 'Indent', true).OnClick := DoIndentButton;
  button(FCurrentMenu, 2, 1, Btn_Offs_Outdent, 'Outdent', true).OnClick := DoOutdentButton;
  button(FCurrentMenu, 2, 3, Btn_Offs_ParaProps, 'Properties', true).OnClick := DoParaPropsButton;
end;

procedure TWordProcessorTouchToolbar.DoPasteOperation(sender: TObject);
var
  sErr : String;
begin
  FWordProcessor.WantCloseTouchMenu;
  if not FWordProcessor.Paste(sErr) then
    ShowMessage(sErr);
end;

procedure TWordProcessorTouchToolbar.DoPasteSpecialOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PasteSpecialDialog;
end;

procedure TWordProcessorTouchToolbar.DoPreviousField(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.PreviousField;
end;

procedure TWordProcessorTouchToolbar.DoPrintOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnPrint(self);
end;

procedure TWordProcessorTouchToolbar.DoRedoOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.Redo;
end;

procedure TWordProcessorTouchToolbar.DoRemoveField(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  Case MessageDlg('Remove Field. Keep Field Contents?', mtConfirmation, mbYesNoCancel, 0) Of
    mrYes : FWordProcessor.PrimaryRange.RemoveField(True);
    mrNo : FWordProcessor.PrimaryRange.RemoveField(False);
  End;
end;

procedure TWordProcessorTouchToolbar.DoReplaceOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ReplaceDialog;
end;

procedure TWordProcessorTouchToolbar.DoSaveAsOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnSaveAs(self);
end;

procedure TWordProcessorTouchToolbar.DoSaveOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnSave(self);
end;

procedure TWordProcessorTouchToolbar.DoSearchOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.SearchDialog;
end;

procedure TWordProcessorTouchToolbar.DoSpellButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.CheckSpelling;
end;

procedure TWordProcessorTouchToolbar.DoStylesButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  ShowMessage('todo');
end;

procedure TWordProcessorTouchToolbar.DoTableMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 7;
  FCurrentMenu.Width := LEFT_INC_H * 4 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 5 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_InsTable, 'Insert', canInsertTable in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsTableButton;
  if FWordProcessor.PrimaryRange.HasCurrentTableStart then
    button(FCurrentMenu, 0, 1, Btn_Offs_TableToText, 'Convert', true).OnClick := DoTableToTextButton
  else
    button(FCurrentMenu, 0, 1, Btn_Offs_TextToTable, 'Convert', FWordProcessor.PrimaryRange.SelectedText <> '').OnClick := DoTextToTableButton;
  button(FCurrentMenu, 0, 3, Btn_Offs_TableProps, 'Properties', canTableProps in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoTablePropsButton;

  button(FCurrentMenu, 1, 0, Btn_Offs_SelRow, 'Select Row', canSelectRow in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSelRowButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_SelTable, 'Select Table', canSelectTable in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSelTableButton;
  button(FCurrentMenu, 1, 2, Btn_Offs_Sort, 'Sort Table', canSortTable in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoSortTableButton;

  button(FCurrentMenu, 2, 0, Btn_Offs_InsColLeft, 'Insert Left', canInsertColumn in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsertLeftButton;
  button(FCurrentMenu, 2, 1, Btn_Offs_InsColRight, 'Insert Right', canInsertColumn in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsertRightButton;
  button(FCurrentMenu, 2, 2, Btn_Offs_InsRowAbove, 'Insert Above', canInsertRowAbove in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsertAboveButton;
  button(FCurrentMenu, 2, 3, Btn_Offs_InsRowBelow, 'Insert Below', canInsertRowBelow in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoInsertBelowButton;

  button(FCurrentMenu, 3, 0, Btn_Offs_DelCol, 'Delete Column', canRemoveColumn in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoDelColButton;
  button(FCurrentMenu, 3, 1, Btn_Offs_DelRow, 'Delete Row', canRemoveRow in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoDelRowButton;
  button(FCurrentMenu, 3, 2, Btn_Offs_DelTable, 'Delete Table', canRemoveTable in FWordProcessor.PrimaryRange.Capabilities).OnClick := DoDelTableButton;
end;

procedure TWordProcessorTouchToolbar.DoToolsMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 8;
  FCurrentMenu.Width := LEFT_INC_H * 1 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 3 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_Spell, 'Spell Check', true).OnClick := DoSpellButton;
  button(FCurrentMenu, 1, 0, Btn_Offs_Styles, 'Style Editor', true).OnClick := DoStylesButton;
  button(FCurrentMenu, 2, 0, Btn_Offs_Options, 'Options', true).OnClick := DoOptionsButton;
end;

procedure TWordProcessorTouchToolbar.DoUndoOperation(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.Undo;
end;

Procedure TWordProcessorTouchToolbar.Build;
var
  left : integer;
  bev  : TUixBevel;
Begin
  Height := 60;
  Left := 0;
  Color := clWhite;
  ParentColor := false;
  ParentBackground := false;
  bev := TUixBevel.Create(self);
  bev.ShapeBottomLine;
  bev.AlignBottom;

  FOperationsButton := Button(self, 0, 0, Btn_Offs_Cat_Operations, 'File', true);
  FOperationsButton.OnClick := DoOperationsMenu;
  FEditButton :=       Button(self, 0, 1, Btn_Offs_Cat_Edit, 'Edit', true);
  FEditButton.OnClick := DoEditMenu;
  FFontButton :=       button(self, 0, 2, Btn_Offs_Cat_Font, 'Font', true);
  FFontButton.OnClick := DoFontMenu;
  FParagraphButton :=  button(self, 0, 3, Btn_Offs_Cat_Paragraph, 'Paragraph', true);
  FParagraphButton.OnClick := DoParagraphMenu;
  FInsertButton :=     button(self, 0, 4, Btn_Offs_Cat_Insert, 'Insert', true);
  FInsertButton.OnClick := DoInsertMenu;
  FImageButton :=      button(self, 0, 5, Btn_Offs_Cat_Image, 'Image', true);
  FImageButton.OnClick := DoImageMenu;
  FFieldButton :=      button(self, 0, 6, Btn_Offs_Cat_Field, 'Field', true);
  FFieldButton.OnClick := DoFieldMenu;
  FTableButton :=      button(self, 0, 7, Btn_Offs_Cat_Table, 'Table', true);
  FTableButton.OnClick := DoTableMenu;
  FToolsButton :=      button(self, 0, 8, Btn_Offs_Cat_Tools, 'Tools', true);
  FToolsButton.OnClick := DoToolsMenu;
  FHelpButton :=      button(self, 0, 9, Btn_Offs_cat_Help, 'Help', true);
  FHelpButton.OnClick := DoHelpMenu;
End;


Procedure TWordProcessorTouchToolbar.LoadWordProcessorData;
Begin
End;


Procedure TWordProcessorTouchToolbar.WordProcessorObserverNotificationHandler(oSender : TObject);
Begin
  updateStatus;
End;


Procedure TWordProcessorTouchToolbar.UpdateStatus;
Begin
End;


Procedure TWordProcessorTouchToolbar.Refresh;
Begin
  UpdateStatus;
End;


Procedure TWordProcessorTouchToolbar.SetWordProcessor(Const Value: TWordProcessor);
Begin
  FWordProcessor := Value;

  If Assigned(FWordProcessor) Then
  Begin
    Build;
    LoadWordProcessorData;

    FWordProcessor.RegisterObserver(Self, WordProcessorObserverNotificationHandler);
  End;
End;

Procedure TWordProcessorTouchToolbar.DoBoldButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplyBold(FWordProcessor.PrimaryRange.Font.HasBold);
end;

Procedure TWordProcessorTouchToolbar.DoItalicButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplyItalic(FWordProcessor.PrimaryRange.Font.HasItalic);
end;

Procedure TWordProcessorTouchToolbar.DoUnderlineButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplyUnderline(FWordProcessor.PrimaryRange.Font.HasUnderline);
end;

Procedure TWordProcessorTouchToolbar.DoColourButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FColourDialog.Color := FWordProcessor.PrimaryRange.Font.Foreground;
  if FColourDialog.Execute then
    FWordProcessor.PrimaryRange.ApplyForeground(FColourDialog.Color);
end;

Procedure TWordProcessorTouchToolbar.DoBackgroundButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FColourDialog.Color := FWordProcessor.PrimaryRange.Font.Background;
  if FColourDialog.Execute then
    FWordProcessor.PrimaryRange.ApplyBackground(FColourDialog.Color);
end;

Procedure TWordProcessorTouchToolbar.DoSuperscriptButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplySuperscript(not (FWordProcessor.PrimaryRange.Font.State = fsSuperscript));
end;

Procedure TWordProcessorTouchToolbar.DoSubscriptButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplySubscript(not (FWordProcessor.PrimaryRange.Font.State = fsSubscript));
end;

Procedure TWordProcessorTouchToolbar.DoFontPropertiesButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.FontDialog;
end;

procedure TWordProcessorTouchToolbar.DoHelpHomeButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnHelpHome(self);
end;

procedure TWordProcessorTouchToolbar.DoHelpIndexButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnHelpIndex(self);
end;

procedure TWordProcessorTouchToolbar.DoHelpMenu(sender: TObject);
begin
  FCurrentMenu := FWordProcessor.CreateTouchMenu;
  FCurrentMenu.Left := 4 + LEFT_INC_H * 9;
  FCurrentMenu.Width := LEFT_INC_H * 2 + 6;
  FCurrentMenu.Height := LEFT_INC_V * 2 + 4;

  button(FCurrentMenu, 0, 0, Btn_Offs_HelpIndex, 'Index', true).OnClick := DoHelpIndexButton;
  button(FCurrentMenu, 1, 0, Btn_Offs_HelpHome, 'Web Page', true).OnClick := DoHelpHomeButton;
  button(FCurrentMenu, 0, 1, Btn_Offs_HelpVersion, 'Upgrade? ', true).OnClick := DoHelpVersionButton;
  button(FCurrentMenu, 1, 1, Btn_Offs_HelpSupportCase, 'Bug Case', true).OnClick := DoHelpSupportCaseButton;
end;

procedure TWordProcessorTouchToolbar.DoHelpSupportCaseButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnHelpSupportCase(self);
end;

procedure TWordProcessorTouchToolbar.DoHelpVersionButton(sender: TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  OnHelpVersion(self);
end;

Procedure TWordProcessorTouchToolbar.DoLeftButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.AlignLeft;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;
end;

Procedure TWordProcessorTouchToolbar.DoCenterButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.AlignCentre;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;
end;

Procedure TWordProcessorTouchToolbar.DoRightButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.AlignRight;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;
end;

Procedure TWordProcessorTouchToolbar.DoJustifyButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.AlignJustify;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;
end;

Procedure TWordProcessorTouchToolbar.DoNormalButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.ListType := WPSParagraphListTypeNone;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;

end;

Procedure TWordProcessorTouchToolbar.DoBulletsButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.ListType := WPSParagraphListTypeBullets;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;

end;

Procedure TWordProcessorTouchToolbar.DoNumbersButton(sender : TObject);
var
  para : TWPSParagraphDetails;
begin
  FWordProcessor.WantCloseTouchMenu;
  para := TWPSParagraphDetails.Create;
  try
    para.ListType := WPSParagraphListTypeNumbers;
    FWordProcessor.PrimaryRange.ApplyParagraph(para);
  finally
    para.Free;
  end;

end;

Procedure TWordProcessorTouchToolbar.DoIndentButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplyLeftIndent(1);
end;

Procedure TWordProcessorTouchToolbar.DoOutdentButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ApplyLeftIndent(-1);
end;

Procedure TWordProcessorTouchToolbar.DoParaPropsButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ParaDialog;
end;


Procedure TWordProcessorTouchToolbar.DoSymbolButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.InsertSymbol;
end;


Procedure TWordProcessorTouchToolbar.DoTemplateButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.InsertTemplate;
end;

Procedure TWordProcessorTouchToolbar.DoBreakButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertPageBreak;
end;

Procedure TWordProcessorTouchToolbar.DoPictureButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.InsertImageDialog;
end;

Procedure TWordProcessorTouchToolbar.DoInsTableButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.InsertTableDialog;
end;

Procedure TWordProcessorTouchToolbar.DoInsLineButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertLine;
end;

Procedure TWordProcessorTouchToolbar.DoSelectButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itSelect;
end;

Procedure TWordProcessorTouchToolbar.DoLineButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itLine;
end;

Procedure TWordProcessorTouchToolbar.DoRectButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itRectangle;
end;

Procedure TWordProcessorTouchToolbar.DoCircleButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itCircle;
end;

Procedure TWordProcessorTouchToolbar.DoMarkButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itMark;
end;

Procedure TWordProcessorTouchToolbar.DoZoomButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.ImageTool := itZoom;
end;

Procedure TWordProcessorTouchToolbar.DoTableToTextButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  ShowMessage('todo');
end;

Procedure TWordProcessorTouchToolbar.DoTextToTableButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.ConvertTextToTable;
end;

Procedure TWordProcessorTouchToolbar.DoTablePropsButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.TablePropertiesDialog;
end;

Procedure TWordProcessorTouchToolbar.DoSelRowButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.SelectRow;
end;

Procedure TWordProcessorTouchToolbar.DoSelTableButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.SelectTable;
end;

Procedure TWordProcessorTouchToolbar.DoSortTableButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.SortTableDialog;
end;

Procedure TWordProcessorTouchToolbar.DoInsertLeftButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertColumnLeft;
end;

Procedure TWordProcessorTouchToolbar.DoInsertRightButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertColumnRight;
end;

Procedure TWordProcessorTouchToolbar.DoInsertAboveButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertRowAbove;
end;

Procedure TWordProcessorTouchToolbar.DoInsertBelowButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.InsertRowBelow;
end;

Procedure TWordProcessorTouchToolbar.DoDelColButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.DeleteColumn;
end;

Procedure TWordProcessorTouchToolbar.DoDelRowButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.DeleteRow;
end;

Procedure TWordProcessorTouchToolbar.DoDelTableButton(sender : TObject);
begin
  FWordProcessor.WantCloseTouchMenu;
  FWordProcessor.PrimaryRange.DeleteTable;
end;



End.
