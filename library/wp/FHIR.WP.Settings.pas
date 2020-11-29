Unit FHIR.WP.Settings;

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
  SysUtils, Graphics,
  fsl_utilities, fsl_base,
  dicom_Dictionary,
  wp_types, wp_definers;


Type
  TWPSettingsProperty = (
     wpspEditHints, wpspFieldWrappers, wpspHotspots, wpspSpellingErrors,
     wpspScale, wpspBackground, wpspLinkColour, wpspHoverColour,
     wpspMargin, wpspTableBorders, wpspImages, wpspFormat, wpspReadOnly,
     wpspAutosavePath, wpspAutosaveFrequency, wpspAutosaveId,
     wpspLowLight, wpspInteractive, wpspBlinking, wpspSearch,
     wpspAllowPopup, wpspImageMapEditing, wpspSelecting,
     wpspShowVerticalScrollbar, wpspPagination,
     wpspNestingIndent, wpspNoSelectReadOnly, wpspInsertTemplates,
     wpspConsoleMode, wpspNoParagraphs, wpspPrintBackgrounds,
     wpspSpellCheckUppercaseWords, wpspAllowSpecialSymbols, wpspFormsMode,
     wpspAnnotationWidth, wpspManageAnnotations, wpspCapitaliseFirstInSentence,
     wpspSmartParagraphs, wpspFieldHints, wpspTouchMode
  );

  TWPSettingsProperties = Set Of TWPSettingsProperty;


Const
  WPSETTINGS_PROPERTIES_ALL = [Low(TWPSettingsProperty)..High(TWPSettingsProperty)];


Type
  TWPSettingsPropertyEvent = Procedure (Const aProperties :  TWPSettingsProperties) Of Object;

  TWPFieldPresentation = (wpfpSquareBrackets, wpfpNone, wpfpHints, wpfpInvisible);
  TWPBackgroundPrintingOption = (wpbpAlways, wpbpInColour, wpbpNever);
  TWPHotspotMode = (wphmNone, wphmAltKey, wphmAlways);
  TWPSettingsTouchMode = (wptmOnTouchDevice, wptmNever, wptmAlways);

  TWPSettings = Class (TFslObject)
    Private
      FOnChange : TWPSettingsPropertyEvent;
      FFieldDefinitions : TWPFieldDefinitionProviderList;
      FAnnotationDefinitions : TWPAnnotationDefinitionProviderList;
      FHold : Boolean;
      FChanged : TWPSettingsProperties;
      FShowDocumentInspector : Boolean;
      FSnapshotEmail : String;

      FEditHints : Boolean;
      FFieldWrappers : TWPFieldPresentation;
      FHotspots : TWPHotspotMode;
      FSpellingErrors : Boolean;

      FScale : Real;
      FBackground : TColour;
      FLinkColour : TColour;
      FHoverColour : TColour;
      FHorizontalMargin : Integer;
      FVerticalMargin : Integer;
      FTableBorders : Boolean;

      FImages : Boolean;
      FImageMapEditing : Boolean;
      FFormat : Boolean;
      FReadOnly : Boolean;

      FAutosavePath : String;
      FAutosaveFrequency : Integer;
      FAutosaveId : String;
      FLowLight : Boolean;
      FInteractive : Boolean;
      FShowVerticalScrollbar : Boolean;
      FBlinking : Boolean;
      FSelecting : Boolean;
      FSearch : Boolean;
      FAllowPopup : Boolean;
      FPagination : Boolean;
      FNestingIndent : Integer;
      FNoSelectReadOnly : Boolean;
      FInsertTemplates : Boolean;
      FConsoleMode: Boolean;
      FNoParagraphs: Boolean;
      FPrintBackgrounds: TWPBackgroundPrintingOption;
      FSpellCheckUppercaseWords: Boolean;
      FAllowSpecialSymbols: Boolean;
      FFormsMode: Boolean;
      FTextWrapWidth: Integer;

      FAnnotationWidth : Integer;
      FManageAnnotations : Boolean;
      FCapitaliseFirstInSentence: Boolean;
      FSmartParagraphs : Boolean;
      FFieldHints: Boolean;
      FTouchMode: TWPSettingsTouchMode;
      FDicomDictionary: TDicomDictionary;

      Function GetMargin : Integer;
      Procedure SetMargin(Const Value : Integer);
      Procedure SetHorizontalMargin(Const Value : Integer);
      Procedure SetVerticalMargin(Const Value : Integer);

      Procedure SetEditHints(Const Value: Boolean);
      Procedure SetFieldWrappers(Const Value: TWPFieldPresentation);
      Procedure SetHotspots(Const Value: TWPHotspotMode);
      Procedure SetSpellingErrors(Const Value: Boolean);
      Procedure SetScale(Const Value: Real);
      Procedure SetBackground(Const Value : TColour);
      Procedure SetHoverColour(Const Value : TColour);
      Procedure SetLinkColour(Const Value : TColour);
      Procedure SetTableBorders(Const Value : Boolean);
      Procedure SetImages(Const Value: Boolean);
      Procedure SetImageMapEditing(Const Value: Boolean);
      Procedure SetFormat(Const Value: Boolean);
      Procedure SetReadOnly (Const Value: Boolean);
      Procedure SetAutosavePath(Const Value : String);
      Procedure SetAutosaveFrequency(Const Value : Integer);
      Procedure SetAutosaveId(Const Value : String);
      Procedure SetLowLight(Const Value : Boolean);
      Procedure SetInteractive(Const Value : Boolean);
      Procedure SetBlinking(Const Value : Boolean);
      Procedure SetSearch(Const Value : Boolean);
      Procedure SetSelecting(Const Value : Boolean);
      Procedure SetShowVerticalScrollbar(Const Value : Boolean);
      Procedure SetAllowPopup(Const Value : Boolean);
      Procedure SetPagination(Const Value : Boolean);
      Procedure SetNestingIndent(Const Value : Integer);
      Procedure SetNoSelectReadOnly(Const Value : Boolean);
      Procedure SetInsertTemplates(Const Value : Boolean);

      Procedure Change(Const aProperty :  TWPSettingsProperty);
      procedure SetConsoleMode(const Value: Boolean);
      procedure SetNoParagraphs(const Value: Boolean);
      procedure SetPrintBackgrounds(const Value: TWPBackgroundPrintingOption);
      procedure SetSpellCheckUppercaseWords(const Value: Boolean);
      procedure SetAllowSpecialSymbols(const Value: Boolean);
      procedure SetFormsMode(const Value: Boolean);
      Procedure SetAnnotationWidth(Const Value : Integer);
      Procedure SetManageAnnotations(Const Value : Boolean);
      procedure SetCapitaliseFirstInSentence(const Value: Boolean);
      procedure SetSmartParagraphs(const Value: Boolean);
      procedure SetFieldHints(const Value: Boolean);
      procedure SetTouchMode(const Value: TWPSettingsTouchMode);
      procedure SetDicomDictionary(const Value: TDicomDictionary);
  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Overload; Override;

      Function Link : TWPSettings;
      Function Clone : TWPSettings;

      Procedure Assign(oSource : TFslObject); Overload; Override;

      Procedure Defaults;

      // common settings combinations
      Procedure ModeWordProcessor; // normal WP editor mode, no editing fields
      Procedure ModeTextEditor; // restricted text only mode
      Procedure ModeReadOnly; // just displaying a WP document
      Procedure ModeBrowser; // behaving like a browser (hotspots etc)

      Procedure Hold;
      Procedure Release;

      Property OnChange : TWPSettingsPropertyEvent Read FOnChange Write FOnChange;
      Property DicomDictionary : TDicomDictionary read FDicomDictionary write SetDicomDictionary;

    // view
      // background document color (current problem: this overlaps with the color of the WP, but eventually color will be irrelevant)
      Property Background : TColour Read FBackground Write SetBackground;

      // if true, the WP is drawn for low light conditions all color constrast is greatly reduced
      Property LowLight : Boolean Read FLowLight Write SetLowLight;

      // margin around edges of WP
      Property Margin : Integer Read GetMargin Write SetMargin;
      Property HorizontalMargin : Integer Read FHorizontalMargin Write SetHorizontalMargin;
      Property VerticalMargin : Integer Read FVerticalMargin Write SetVerticalMargin;

      // internal scale applied to everything
      Property Scale : Real Read FScale Write SetScale;

      // if true then table borders are shown (when the table has no borders of it's own)
      Property TableBorders : Boolean Read FTableBorders Write SetTableBorders;

      // how far nested table rows are indented
      Property NestingIndent : Integer Read FNestingIndent Write SetNestingIndent;

      // if true, then pagination hints will be displayed in the left margin when printer etc are supplied
      Property Pagination : Boolean Read FPagination Write SetPagination;

    // hotspots
      // default Link color (will be overriden by other colours specified in field or content)
      Property LinkColour : TColour Read FLinkColour Write SetLinkColour;

      // default Hover color (will be overriden by colour specified in field)
      Property HoverColour : TColour Read FHoverColour Write SetHoverColour;

      // whether hotspots are associated with a finger cursor and user can click them (no, if-ctrl-button dowm, yes)
      Property Hotspots : TWPHotspotMode Read FHotspots Write SetHotspots;

    // misc view properties
      // If true then Hints (6 etc) are shown
      Property EditHints : Boolean Read FEditHints Write SetEditHints;

      // if true, then a little hint window comes up whenever the mouse is over a field showing what type of field it is
      Property FieldHints : Boolean read FFieldHints write SetFieldHints;

      // if true then spelling errors are shown
      Property SpellingErrors : Boolean Read FSpellingErrors Write SetSpellingErrors;

    // fields
      // How fields are shown.
      Property FieldWrappers : TWPFieldPresentation Read FFieldWrappers Write SetFieldWrappers;

    // misc user authorities
      // Theres's complex relationships between the various settings that control
      // user authorities. Rationalising these is being considered, but a time for this
      // is unknown
      //  Proprty        default         Meaning
      //  Interactive    true            whether the WP responds to the user at all
      //  ReadOnly       false           whether any changes to the content through editing interface are allowed at all (either user or programmatic)
      //  Selecting      true            Whether user can select text at all

      // if true, the WP will actually work
      Property Interactive : Boolean Read FInteractive Write SetInteractive;

      // if true, content is readonly
      Property ReadOnly : Boolean Read FReadOnly Write SetReadOnly;

      // if true, images can be added
      Property Images : Boolean Read FImages Write SetImages;

       // if true, images can be added
      Property ImageMapEditing : Boolean Read FImageMapEditing Write SetImageMapEditing;

      // if true, format can be changed (else is text only)
      Property Format : Boolean Read FFormat Write SetFormat;

      // if true, the WP will allow the user to select using the mouse or holding
      // shift down and moving around the document
      Property Selecting : Boolean Read FSelecting Write SetSelecting;

      // NOTE: it really doesn't make sense to turn this off unless blinking is false and read-only is true
      Property Blinking : Boolean Read FBlinking Write SetBlinking;

      // whether to show vertical scroll bar all the time
      Property ShowVerticalScrollbar : Boolean Read FShowVerticalScrollbar Write SetShowVerticalScrollbar;

      // if true, the WP will support search
      Property Search : Boolean Read FSearch Write SetSearch;

      // if true, the WP will show the popup menu on right click or context key stroke.
      Property AllowPopup : Boolean Read FAllowPopup Write SetAllowPopup;

      // Whether to show the support tools menu
      Property SnapshotEmail : String Read FSnapshotEmail Write FSnapshotEmail;

      // Whether to show document inspector on the support tools menu
      Property ShowDocumentInspector : Boolean Read FShowDocumentInspector Write FShowDocumentInspector;

      // if this is true, the cursor will only be in non-readonly content
      Property NoSelectReadOnly : Boolean Read FNoSelectReadOnly Write SetNoSelectReadOnly;

      Property InsertTemplates : Boolean Read FInsertTemplates Write SetInsertTemplates;

      // if this is true, words that are all uppercase will not be ignored by the spell checker
      Property SpellCheckUppercaseWords : Boolean read FSpellCheckUppercaseWords write SetSpellCheckUppercaseWords;

    // autosave is not yet implemented
      Property AutosavePath : String Read FAutosavePath Write SetAutosavePath;
      Property AutosaveFrequency : Integer Read FAutosaveFrequency Write SetAutosaveFrequency; {seconds}
      Property AutosaveId : String Read FAutosaveId Write SetAutosaveId;

      Property FieldDefinitions : TWPFieldDefinitionProviderList Read FFieldDefinitions;
      Property AnnotationDefinitions : TWPAnnotationDefinitionProviderList Read FAnnotationDefinitions;

      // Console Mode
      Property ConsoleMode : Boolean read FConsoleMode write SetConsoleMode;

      // No paragraphs - user is not allowed to insert any paragraphs.
      // inserted/pasted paragraphs are ignored
      Property NoParagraphs : Boolean read FNoParagraphs write SetNoParagraphs;

      Property PrintBackgrounds : TWPBackgroundPrintingOption read FPrintBackgrounds write SetPrintBackgrounds;

      Property AllowSpecialSymbols : Boolean read FAllowSpecialSymbols write SetAllowSpecialSymbols;

      // in this mode, all the content is readonly except for what's in fields.
      // fields themselves are readonly too.
      Property FormsMode : Boolean read FFormsMode write SetFormsMode;

      // How much space for annotations. Values = 0 means don't show them. Otherwise value should be > 40
      Property AnnotationWidth : Integer Read FAnnotationWidth Write SetAnnotationWidth;
      Property ManageAnnotations : Boolean Read FManageAnnotations Write SetManageAnnotations;


      Property TextWrapWidth : Integer read FTextWrapWidth write FTextWrapWidth;

      // if this is true, then when a sentence is started (first in paragraph, or following a '.'), the first letter is automatically capitalised.
      // if you don't want a capital, then you have to go back and replace it with the lower case letter after typing the second letter (Doesn't work
      // for I)!
      Property CapitaliseFirstInSentence : Boolean read FCapitaliseFirstInSentence write SetCapitaliseFirstInSentence;

      // if this is true, then deleting a paragaph will adopt the formatting of the deleted paragraph, not simply adopt the formatting of the following paragraph
      // also, double enter ends a list, and backspace breaks the list
      Property SmartParagraphs : Boolean read FSmartParagraphs write SetSmartParagraphs;

      // whether touchis supported never, all the time, or only on a touch screen
      Property TouchMode : TWPSettingsTouchMode read FTouchMode write SetTouchMode;
  End;

Const
  NAMES_WPFieldPresentation : Array [TWPFieldPresentation] Of String = ('SquareBrackets', 'None', 'Hints', 'Invisible');
  NAMES_WPBackgroundPrintingOption : Array [TWPBackgroundPrintingOption] Of String = ('Always', 'In Colour', 'Never');
  NAMES_WPHotspotMode : Array [TWPHotspotMode] Of String = ('None', 'AltKey', 'Always');

Type
  TWPSettable = Class (TFslObject)
    Private
      FSettings : TWPSettings;

      Function GetSettings : TWPSettings;
    Protected
      Procedure SetSettings(Const Value : TWPSettings); Virtual;
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Overload; Override;
      destructor Destroy; Override;

      Property Settings : TWPSettings Read GetSettings Write SetSettings;
  End;

Implementation



Constructor TWPSettings.Create;
Begin
  Inherited;
  FFieldDefinitions := TWPFieldDefinitionProviderList.Create;
  FAnnotationDefinitions := TWPAnnotationDefinitionProviderList.Create;
  Defaults;
End;

Destructor TWPSettings.Destroy;
Begin
  FDicomDictionary.Free;
  FFieldDefinitions.Free;
  FAnnotationDefinitions.Free;
  Inherited;
End;


Procedure TWPSettings.Defaults;
Begin
  Hold;
  Try
    Scale := 1;
    SpellingErrors := True;
    TableBorders := True;
    HorizontalMargin := 20;
    VerticalMargin := 20;
    Images := True;
    ImageMapEditing := True;
    Format := True;
    Interactive := True;
    Background := clWhite;
    LinkColour := clNavy;
    HoverColour := clRed;
    Blinking := True;
    Search := True;
    FieldWrappers := wpfpSquareBrackets;
    AllowPopup := True;
    Selecting := True;
    ShowVerticalScrollbar := False;
    FNestingIndent := NESTED_TABLE_INDENT;
    NoSelectReadOnly := False;
    PrintBackgrounds := wpbpAlways;
    AnnotationWidth := 0;
    ManageAnnotations := False;
    TextWrapWidth := 90;
    FieldHints := False;
  Finally
    Release;
  End;
End;

Procedure TWPSettings.ModeWordProcessor;
Begin
  Hold;
  Try
    FieldWrappers := wpfpSquareBrackets;
    Hotspots := wphmAltKey;
    SpellingErrors := True;
    TableBorders := True;
    Images := True;
    ImageMapEditing := True;
    Format := True;
    ReadOnly := False;
    Interactive := True;
    Blinking := True;
    Search := True;
    AllowPopup := True;
    Selecting := True;
    NoSelectReadOnly := False;
    PrintBackgrounds := wpbpInColour;
    AnnotationWidth := DEF_ANNOTATION_WIDTH;
    ManageAnnotations := True;
    CapitaliseFirstInSentence := true;
    FieldHints := True;
  Finally
    Release;
  End;
End;


Procedure TWPSettings.ModeTextEditor;
Begin
  Hold;
  Try
    EditHints := False;
    FieldWrappers := wpfpNone; // ?
    Hotspots := wphmNone;
    SpellingErrors := True;
    TableBorders := False;
    Images := False;
    ImageMapEditing := False;
    Format := False;
    ReadOnly := False;
    Interactive := True;
    Blinking := True;
    Search := True;
    AllowPopup := True;
    Selecting := True;
    NoSelectReadOnly := False;
    PrintBackgrounds := wpbpNever;
    AnnotationWidth := 0;
    ManageAnnotations := False;
    CapitaliseFirstInSentence := true;
    FieldHints := False;
  Finally
    Release;
  End;
End;


Procedure TWPSettings.ModeReadOnly;
Begin
  Hold;
  Try
    EditHints := False;
    FieldWrappers := wpfpSquareBrackets;
    Hotspots := wphmAlways;
    SpellingErrors := False;
    TableBorders := False;
    Images := False;
    ImageMapEditing := False;
    Format := False;
    ReadOnly := True;
    Interactive := True;
    Blinking := True;
    Search := True;
    AllowPopup := True;
    Selecting := True;
    NoSelectReadOnly := False;
    PrintBackgrounds := wpbpInColour;
    AnnotationWidth := 0;
    ManageAnnotations := False;
    CapitaliseFirstInSentence := False;
    FieldHints := False;
  Finally
    Release;
  End;
End;


Procedure TWPSettings.ModeBrowser;
Begin
  Hold;
  Try
    EditHints := False;
    FieldWrappers := wpfpNone;
    Hotspots := wphmAlways;
    SpellingErrors := False;
    TableBorders := False;
    Images := False;
    ImageMapEditing := False;
    Format := False;
    ReadOnly := True;
    Interactive := True;
    Blinking := False;
    Search := False;
    AllowPopup := False;
    Selecting := True;
    ShowVerticalScrollbar := False;
    NoSelectReadOnly := False; //should be true?
    PrintBackgrounds := wpbpAlways;
    AnnotationWidth := 0;
    ManageAnnotations := False;
    CapitaliseFirstInSentence := False;
    FieldHints := True;
  Finally
    Release;
  End;
End;


Function TWPSettings.Link : TWPSettings;
Begin
  Result := TWPSettings(Inherited Link);
End;


Function TWPSettings.Clone : TWPSettings;
Begin
  Result := TWPSettings(Inherited Clone);
End;


Procedure TWPSettings.Assign(oSource : TFslObject);
Var
  oSrc : TWPSettings;
Begin
  Inherited;
  oSrc := TWPSettings(oSource);

  FFieldDefinitions.Assign(oSrc.FFieldDefinitions);
  FAnnotationDefinitions.Assign(oSrc.FAnnotationDefinitions);
  FEditHints := oSrc.FEditHints;
  FFieldWrappers := oSrc.FFieldWrappers;
  FHotspots := oSrc.FHotspots;
  FSpellingErrors := oSrc.FSpellingErrors;
  FScale := oSrc.FScale;
  FBackground := oSrc.FBackground;
  FLinkColour := oSrc.FLinkColour;
  FHoverColour := oSrc.FHoverColour;
  FHorizontalMargin := oSrc.FHorizontalMargin;
  FVerticalMargin := oSrc.FVerticalMargin;
  FTableBorders := oSrc.FTableBorders;
  FImages := oSrc.FImages;
  FImageMapEditing := oSrc.FImageMapEditing;
  FFormat := oSrc.FFormat;
  FReadOnly := oSrc.FReadOnly;
  FAutosavePath := oSrc.FAutosavePath;
  FAutosaveFrequency := oSrc.FAutosaveFrequency;
  FAutosaveId := oSrc.FAutosaveId;
  FLowLight := oSrc.FLowLight;
  FInteractive := oSrc.FInteractive;
  FBlinking := oSrc.FBlinking;
  FSearch := oSrc.FBlinking;
  FAllowPopup := oSrc.AllowPopup;
  FSelecting := oSrc.FSelecting;
  FShowVerticalScrollbar := oSrc.FShowVerticalScrollbar;
  FPagination := oSrc.FPagination;
  FNestingIndent := oSrc.FNestingIndent;
  FNoSelectReadOnly := oSrc.FNoSelectReadOnly;
  FAnnotationWidth := oSrc.FAnnotationWidth;
  FManageAnnotations := oSrc.FManageAnnotations;

End;


Procedure TWPSettings.Change(Const aProperty :  TWPSettingsProperty);
Begin
  If FHold Then
    FChanged := FChanged + [aProperty]
  Else If Assigned(FOnChange) Then
    FOnChange([aProperty]);
End;


Procedure TWPSettings.Hold;
Begin
  FHold := True;
  FChanged := [];
End;


Procedure TWPSettings.Release;
Begin
  FHold := False;
  If Assigned(FOnChange) Then
    FOnChange(FChanged);
  FChanged := [];
End;


Procedure TWPSettings.SetScale(Const Value: Real);
Begin
  If (FScale <> Value) Then
  Begin
    FScale := Value;
    Change(wpspScale);
  End;
End;


Procedure TWPSettings.SetEditHints(Const Value: Boolean);
Begin
  If FEditHints <> Value Then
  Begin
    FEditHints := Value;
    Change(wpspEditHints);
  End;
End;


Procedure TWPSettings.SetFieldWrappers(Const Value: TWPFieldPresentation);
Begin
  If FFieldWrappers <> Value Then
  Begin
    FFieldWrappers := Value;
    Change(wpspFieldWrappers);
  End;
End;


Procedure TWPSettings.SetHotspots(Const Value: TWPHotspotMode);
Begin
  If FHotspots <> Value Then
  Begin
    FHotspots := Value;
    Change(wpspHotspots);
  End;
End;


Procedure TWPSettings.SetSpellingErrors(Const Value: Boolean);
Begin
  If FSpellingErrors <> Value Then
  Begin
    FSpellingErrors := Value;
    Change(wpspSpellingErrors);
  End;
End;


Procedure TWPSettings.SetBackground(Const Value : TColour);
Begin
  If FBackground <> Value Then
  Begin
    FBackground := Value;
    Change(wpspBackground);
  End;
End;


Procedure TWPSettings.SetLinkColour(Const Value : TColour);
Begin
  If FLinkColour <> Value Then
  Begin
    FLinkColour := Value;
    Change(wpspLinkColour);
  End;
End;


Procedure TWPSettings.SetHoverColour(Const Value : TColour);
Begin
  If FHoverColour <> Value Then
  Begin
    FHoverColour := Value;
    Change(wpspHoverColour);
  End;
End;


Function TWPSettings.GetMargin : Integer;
Begin
  Assert(CheckCondition(FHorizontalMargin = FVerticalMargin, 'GetMargin', 'Horizontal margin and vertical margin are not equal ('+inttostr(FHorizontalMargin)+'/'+inttostr(FVerticalMargin)+').'));

  Result := FHorizontalMargin;
End;


Procedure TWPSettings.SetMargin(Const Value : Integer);
Begin
  If (FHorizontalMargin <> Value) Or (FVerticalMargin <> Value) Then
  Begin
    FHorizontalMargin := Value;
    FVerticalMargin := Value;

    Change(wpspMargin);
  End;
End;


Procedure TWPSettings.SetHorizontalMargin(Const Value: Integer);
Begin
  If FHorizontalMargin <> Value Then
  Begin
    FHorizontalMargin := Value;
    Change(wpspMargin);
  End;
End;


Procedure TWPSettings.SetVerticalMargin(Const Value: Integer);
Begin
  If FVerticalMargin <> Value Then
  Begin
    FVerticalMargin := Value;
    Change(wpspMargin);
  End;
End;


Procedure TWPSettings.SetTableBorders(Const Value : Boolean);
Begin
  If FTableBorders <> Value Then
  Begin
    FTableBorders := Value;
    Change(wpspTableBorders);
  End;
End;


procedure TWPSettings.SetTouchMode(const Value: TWPSettingsTouchMode);
begin
  If (FTouchMode <> Value) Then
  Begin
    FTouchMode := Value;
    Change(wpspTouchMode);
  End;
end;

Procedure TWPSettings.SetImages(Const Value: Boolean);
Begin
  If FImages <> Value Then
  Begin
    FImages := Value;
    Change(wpspImages);
  End;
End;


Procedure TWPSettings.SetImageMapEditing(Const Value: Boolean);
Begin
  If FImageMapEditing <> Value Then
  Begin
    FImageMapEditing := Value;
    Change(wpspImageMapEditing);
  End;
End;


Procedure TWPSettings.SetFormat(Const Value: Boolean);
Begin
  If FFormat <> Value Then
  Begin
    FFormat := Value;
    Change(wpspFormat);
  End;
End;


Procedure TWPSettings.SetReadOnly (Const Value: Boolean);
Begin
  If FReadOnly  <> Value Then
  Begin
    FReadOnly  := Value;
    Change(wpspReadOnly );
  End;
End;


Procedure TWPSettings.SetAutosavePath(Const Value : String);
Begin
  If AutosavePath <> Value Then
  Begin
    FAutosavePath := Value;
    Change(wpspAutosavePath);
  End;
End;


Procedure TWPSettings.SetAutosaveFrequency(Const Value : Integer);
Begin
  If AutosaveFrequency <> Value Then
  Begin
    FAutosaveFrequency := Value;
    Change(wpspAutosaveFrequency);
  End;
End;


Procedure TWPSettings.SetAutosaveId(Const Value : String);
Begin
  If AutosaveId <> Value Then
  Begin
    FAutosaveId := Value;
    Change(wpspAutosaveId);
  End;
End;


Procedure TWPSettings.SetLowLight(Const Value : Boolean);
Begin
  If LowLight <> Value Then
  Begin
    FLowLight := Value;
    Change(wpspLowLight);
  End;
End;


Procedure TWPSettings.SetInteractive(Const Value : Boolean);
Begin
  If Interactive <> Value Then
  Begin
    FInteractive := Value;
    Change(wpspInteractive);
  End;
End;


Procedure TWPSettings.SetBlinking(Const Value : Boolean);
Begin
  If Blinking <> Value Then
  Begin
    FBlinking := Value;
    Change(wpspBlinking);
  End;
End;


Procedure TWPSettings.SetSearch(Const Value : Boolean);
Begin
  If Search <> Value Then
  Begin
    FSearch := Value;
    Change(wpspSearch);
  End;
End;


Procedure TWPSettings.SetAllowPopup(Const Value: Boolean);
Begin
  If AllowPopup <> Value Then
  Begin
    FAllowPopup := Value;
    Change(wpspAllowPopup);
  End;
End;


Procedure TWPSettings.SetSelecting(Const Value: Boolean);
Begin
  If Selecting <> Value Then
  Begin
    FSelecting := Value;
    Change(wpspSelecting);
  End;
End;


Procedure TWPSettings.SetShowVerticalScrollbar(Const Value: Boolean);
Begin
  If ShowVerticalScrollbar <> Value Then
  Begin
    FShowVerticalScrollbar := Value;
    Change(wpspShowVerticalScrollbar);
  End;
End;

Procedure TWPSettings.SetPagination(Const Value: Boolean);
Begin
  If (FPagination <> Value) Then
  Begin
    FPagination := Value;
    Change(wpspPagination);
  End;
End;


Procedure TWPSettings.SetNestingIndent(Const Value : Integer);
Begin
  If (FNestingIndent <> Value) Then
  Begin
    FNestingIndent := Value;
    Change(wpspNestingIndent);
  End;
End;

Procedure TWPSettings.SetNoSelectReadOnly(Const Value : Boolean);
Begin
  If (FNoSelectReadOnly <> Value) Then
  Begin
    FNoSelectReadOnly := Value;
    Change(wpspNoSelectReadOnly);
  End;
End;

Procedure TWPSettings.SetInsertTemplates(Const Value : Boolean);
Begin
  If (FInsertTemplates <> Value) Then
  Begin
    FInsertTemplates := Value;
    Change(wpspInsertTemplates);
  End;
End;


procedure TWPSettings.SetConsoleMode(const Value: Boolean);
begin
  If (FConsoleMode <> Value) Then
  Begin
    FConsoleMode := Value;
    Change(wpspConsoleMode);
  End;
end;

procedure TWPSettings.SetDicomDictionary(const Value: TDicomDictionary);
begin
  FDicomDictionary.Free;
  FDicomDictionary := Value;
end;

procedure TWPSettings.SetNoParagraphs(const Value: Boolean);
begin
  If (FNoParagraphs <> Value) Then
  Begin
    FNoParagraphs := Value;
    Change(wpspNoParagraphs);
  End;
end;

procedure TWPSettings.SetPrintBackgrounds(const Value: TWPBackgroundPrintingOption);
begin
  If (FPrintBackgrounds <> Value) Then
  Begin
    FPrintBackgrounds := Value;
    Change(wpspPrintBackgrounds);
  End;
end;

procedure TWPSettings.SetSpellCheckUppercaseWords(const Value: Boolean);
begin
  If (FSpellCheckUppercaseWords <> Value) Then
  Begin
    FSpellCheckUppercaseWords := Value;
    Change(wpspSpellCheckUppercaseWords);
  End;
end;

procedure TWPSettings.SetAllowSpecialSymbols(const Value: Boolean);
begin
  If (FAllowSpecialSymbols <> Value) Then
  Begin
    FAllowSpecialSymbols := Value;
    Change(wpspAllowSpecialSymbols);
  End;
end;


procedure TWPSettings.SetFormsMode(const Value: Boolean);
begin
  if FFormsMode <> Value Then
  Begin
    FFormsMode := Value;
    Change(wpspFormsMode);
  End;
end;

Procedure TWPSettings.SetAnnotationWidth(Const Value : Integer);
Begin
  If (FAnnotationWidth <> Value) Then
  Begin
    FAnnotationWidth := Value;
    Change(wpspAnnotationWidth);
  End;
End;

Procedure TWPSettings.SetManageAnnotations(Const Value : Boolean);
Begin
  If (FManageAnnotations <> Value) Then
  Begin
    FManageAnnotations := Value;
    Change(wpspManageAnnotations);
  End;
End;



procedure TWPSettings.SetCapitaliseFirstInSentence(const Value: Boolean);
begin
  If (FManageAnnotations <> Value) Then
  Begin
    FCapitaliseFirstInSentence := Value;
    Change(wpspCapitaliseFirstInSentence);
  End;
end;

procedure TWPSettings.SetSmartParagraphs(const Value: Boolean);
begin
  If (FManageAnnotations <> Value) Then
  Begin
    FSmartParagraphs := Value;
    Change(wpspSmartParagraphs);
  End;
end;

procedure TWPSettings.SetFieldHints(const Value: Boolean);
begin
  If (FFieldHints <> Value) Then
  Begin
    FFieldHints := Value;
    Change(wpspFieldHints);
  End;
end;



function TWPSettings.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FFieldDefinitions.sizeInBytes);
  inc(result, FAnnotationDefinitions.sizeInBytes);
  inc(result, (FSnapshotEmail.length * sizeof(char)) + 12);
  inc(result, (FAutosavePath.length * sizeof(char)) + 12);
  inc(result, (FAutosaveId.length * sizeof(char)) + 12);
  inc(result, FDicomDictionary.sizeInBytes);
end;

Constructor TWPSettable.Create;
Begin
  Inherited;
  FSettings := TWPSettings.Create;
End;


Destructor TWPSettable.Destroy;
Begin
  FSettings.Free;
  Inherited;
End;


Function TWPSettable.GetSettings : TWPSettings;
Begin
  Result := FSettings;
End;


Procedure TWPSettable.SetSettings(Const Value : TWPSettings);
Begin
  FSettings.Free;
  FSettings := Value;
End;


function TWPSettable.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FSettings.sizeInBytes);
end;

End.
