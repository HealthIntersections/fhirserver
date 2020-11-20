Unit FHIR.WP.Icons;

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
  Windows, SysUtils, Vcl.Graphics, Forms,
  fsl_base, fsl_utilities,
  wp_graphics, fui_vclx_images, FHIR.WP.Widgets;


Type
  TWPIconModule = Class(TFslObject)
    Private
      FImages : TUixImages;
      FToolbarWidgetImageIndexArray : TWPToolbarWidgetImageArray;
      FCURSOR_PEN : Integer;
      FCURSOR_BOX : Integer;
      FCURSOR_MARK : Integer;
      FCURSOR_ZOOM : Integer;
      FCURSOR_CIRCLE : Integer;
      FCheck_Unchecked : TFslBitmapGraphic;
      FCheck_Checked : TFslBitmapGraphic;
      FCheck_Unchecked_High : TFslBitmapGraphic;
      FCheck_Checked_High : TFslBitmapGraphic;

    {$IFOPT C+}
      Function INVALID : Integer; Overload;
    {$ENDIF}

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function NONE : Integer; Overload;
      Function CUT : Integer; Overload;
      Function Copy : Integer; Overload;
      Function PASTE : Integer; Overload;
      Function UNDO : Integer; Overload;
      Function REDO : Integer; Overload;
      Function SEARCH : Integer; Overload;
      Function MACRO : Integer; Overload;
      Function BOLD : Integer; Overload;
      Function ITALIC : Integer; Overload;
      Function UNDERLINE : Integer; Overload;
      Function SUPERSCRIPT : Integer; Overload;
      Function SUBSCRIPT : Integer; Overload;
      Function ALIGN_LEFT : Integer; Overload;
      Function ALIGN_CENTRE : Integer; Overload;
      Function ALIGN_RIGHT : Integer; Overload;
      Function ALIGN_JUSTIFY : Integer; Overload;
      Function NUMBERS : Integer; Overload;
      Function BULLETS : Integer; Overload;
      Function INDENT : Integer; Overload;
      Function OUTDENT : Integer; Overload;
      Function INSERT_TABLE : Integer; Overload;
      Function INSERT_IMAGE : Integer; Overload;
      Function INSERT_PDF : Integer; Overload;
      Function INSERT_TEMPLATE : Integer; Overload;
      Function INSERT_PAGEBREAK : Integer; Overload;
      Function INSERT_LINE : Integer; Overload;
      Function SHOW_HINTS : Integer; Overload;
      Function SPELLING : Integer; Overload;
      Function FORMAT_FONT : Integer; Overload;
      Function FORMAT_PARA : Integer; Overload;
      Function PASTE_SPECIAL : Integer; Overload;
      Function INSERT_FIELD : Integer; Overload;
      Function INSERT_SECTION_FIELD : Integer; Overload;
      Function INSERT_SYMBOL : Integer; Overload;
      Function OBJECT_PROPERTIES : Integer; Overload;
      Function FIELD_NEXT : Integer; Overload;
      Function FIELD_PREVIOUS : Integer; Overload;
      Function FIELD_DELETE : Integer; Overload;
      Function COPIED_ROW_ABOVE : Integer; Overload;
      Function COPIED_ROW_BELOW : Integer; Overload;
      Function ROW_ABOVE : Integer; Overload;
      Function ROW_BELOW : Integer; Overload;
      Function SORT_TABLE : Integer; Overload;
      Function COLUMN_LEFT : Integer; Overload;
      Function COLUMN_RIGHT : Integer; Overload;
      Function SELECT_ROW : Integer; Overload;
      Function SELECT_TABLE : Integer; Overload;
      Function DELETE_TABLE : Integer; Overload;
      Function DELETE_ROW : Integer; Overload;
      Function DELETE_COLUMN : Integer; Overload;
      Function VOICE : Integer; Overload;
      Function CONVERTTOTABLE : Integer; Overload;
      Function CHANGE_CASE : Integer; Overload;
      Function INSERT_HOTSPOT : Integer; Overload;
      Function IMAGE_SELECT : Integer; Overload;
      Function IMAGE_PEN : Integer; Overload;
      Function IMAGE_BOX : Integer; Overload;
      Function IMAGE_MARK : Integer; Overload;
      Function IMAGE_ZOOM : Integer; Overload;
      Function IMAGE_SELECT_ON: Integer; Overload;
      Function IMAGE_PEN_ON: Integer; Overload;
      Function IMAGE_BOX_ON: Integer; Overload;
      Function IMAGE_MARK_ON: Integer; Overload;
      Function IMAGE_ZOOM_ON: Integer; Overload;
      Function IMAGE_CIRCLE: Integer; Overload;
      Function IMAGE_CIRCLE_ON: Integer; Overload;
      Function INSERT_COMMENT : Integer; Overload;
      Function FIRST : Integer; Overload;
      Function PREV : Integer; Overload;
      Function NEXT : Integer; Overload;
      Function LAST : Integer; Overload;
      Function SNOMED : Integer; Overload;
      Function EXT_NEW : Integer; Overload;
      Function EXT_OPEN : Integer; Overload;
      Function EXT_SAVE : Integer; Overload;
      Function EXT_PRINT : Integer; Overload;
      Function EXT_IMPORT : Integer; Overload;
      Function EXT_EXPORT : Integer; Overload;
      Function EXT_PAGEDESIGN : Integer; Overload;
      Function EXT_EMAIL : Integer; Overload;

      Property Images : TUixImages Read FImages;
      Property TOOLBARWIDGET : TWPToolbarWidgetImageArray Read FToolbarWidgetImageIndexArray;

      Function CURSOR_PEN : Integer;
      Function CURSOR_BOX : Integer;
      Function CURSOR_MARK : Integer;
      Function CURSOR_ZOOM : Integer;
      Function CURSOR_CIRCLE : Integer;
      Property Check_Unchecked : TFslBitmapGraphic read FCheck_Unchecked;
      Property Check_Checked : TFslBitmapGraphic read FCheck_Checked;
      Property Check_Unchecked_High : TFslBitmapGraphic read FCheck_Unchecked_High;
      Property Check_Checked_High : TFslBitmapGraphic read FCheck_Checked_High;
  End;


Function WPIconModule : TWPIconModule;


Implementation


{$R resources\FHIR.WP.Icons.RES}


Constructor TWPIconModule.Create;
{$IFOPT C+}
Var
  aIndex : TWPToolbarWidget;
{$ENDIF}
Begin
  Inherited;

  FImages := TUixImages.Create(Nil);
  FImages.LoadBitmapFromResource('WPIconModuleBMP', clFuchsia);
  FImages.Name := 'WPToolbarImageList';

{$IFOPT C+}
  For aIndex := Low(FToolbarWidgetImageIndexArray) To High(FToolbarWidgetImageIndexArray) Do
    FToolbarWidgetImageIndexArray[aIndex] := INVALID;
{$ENDIF}

  FToolbarWidgetImageIndexArray[tbwCut] := CUT;
  FToolbarWidgetImageIndexArray[tbwCopy] := Copy;
  FToolbarWidgetImageIndexArray[tbwCopyAll] := None;
  FToolbarWidgetImageIndexArray[tbwPaste] := PASTE;
  FToolbarWidgetImageIndexArray[tbwPasteSpecial] := PASTE_SPECIAL;
  FToolbarWidgetImageIndexArray[tbwUndo] := UNDO;
  FToolbarWidgetImageIndexArray[tbwRedo] := REDO;
  FToolbarWidgetImageIndexArray[tbwSearch] := SEARCH;
  FToolbarWidgetImageIndexArray[tbwMacro] := MACRO;
  FToolbarWidgetImageIndexArray[tbwPlayback] := VOICE;
  FToolbarWidgetImageIndexArray[tbwSpelling] := SPELLING;
  FToolbarWidgetImageIndexArray[tbwStyle] := NONE;
  FToolbarWidgetImageIndexArray[tbwFontName] := NONE;
  FToolbarWidgetImageIndexArray[tbwFontSize] := NONE;
  FToolbarWidgetImageIndexArray[tbwFontColour] := NONE;
  FToolbarWidgetImageIndexArray[tbwBackColor] := NONE;
  FToolbarWidgetImageIndexArray[tbwBold] := BOLD;
  FToolbarWidgetImageIndexArray[tbwItalic] := ITALIC;
  FToolbarWidgetImageIndexArray[tbwUnderline] := UNDERLINE;
  FToolbarWidgetImageIndexArray[tbwSuperscript] := SUPERSCRIPT;
  FToolbarWidgetImageIndexArray[tbwChangeCase] := CHANGE_CASE;
  FToolbarWidgetImageIndexArray[tbwSubscript] := SUBSCRIPT;
  FToolbarWidgetImageIndexArray[tbwLeft] := ALIGN_LEFT;
  FToolbarWidgetImageIndexArray[tbwCentre] := ALIGN_CENTRE;
  FToolbarWidgetImageIndexArray[tbwRight] := ALIGN_RIGHT;
  FToolbarWidgetImageIndexArray[tbwJustify] := ALIGN_JUSTIFY;
  FToolbarWidgetImageIndexArray[tbwBullet] := BULLETS;
  FToolbarWidgetImageIndexArray[tbwNumber] := NUMBERS;
  FToolbarWidgetImageIndexArray[tbwIndent] := INDENT;
  FToolbarWidgetImageIndexArray[tbwOutdent] := OUTDENT;
  FToolbarWidgetImageIndexArray[tbwInsertTable] := INSERT_TABLE;
  FToolbarWidgetImageIndexArray[tbwInsertImage] := INSERT_IMAGE;
  FToolbarWidgetImageIndexArray[tbwInsertLine] := INSERT_LINE;
  FToolbarWidgetImageIndexArray[tbwInsertPageBreak] := INSERT_PAGEBREAK;
  FToolbarWidgetImageIndexArray[tbwEditHints] := SHOW_HINTS;
  FToolbarWidgetImageIndexArray[tbwInsertField] := INSERT_FIELD;
  FToolbarWidgetImageIndexArray[tbwInsertTemplate] := INSERT_TEMPLATE;
  FToolbarWidgetImageIndexArray[tbwInsertSymbol] := INSERT_SYMBOL;

  FToolbarWidgetImageIndexArray[tbwNew] := EXT_New;
  FToolbarWidgetImageIndexArray[tbwOpen] := EXT_Open;
  FToolbarWidgetImageIndexArray[tbwSave] := EXT_Save;
  FToolbarWidgetImageIndexArray[tbwSaveAs] := EXT_Save;
  FToolbarWidgetImageIndexArray[tbwPrint] := EXT_Print;
  FToolbarWidgetImageIndexArray[tbwImport] := EXT_Import;
  FToolbarWidgetImageIndexArray[tbwExport] := EXT_Export;
  FToolbarWidgetImageIndexArray[tbwPageDesign] := EXT_PageDesign;

{$IFOPT C+}
  For aIndex := Low(FToolbarWidgetImageIndexArray) To High(FToolbarWidgetImageIndexArray) Do
  Begin
    If FToolbarWidgetImageIndexArray[aIndex] = INVALID Then
      Invariant('Create', StringFormat('Toolbar Widget ''%s'' does not have a specified image index.', [CAPTIONS_TOOLBARWIDGET[aIndex]]));
  End;
{$ENDIF}

  FCURSOR_PEN := 1001;
  FCURSOR_BOX := FCURSOR_PEN + 1;
  FCURSOR_MARK := FCURSOR_BOX + 1;
  FCURSOR_ZOOM := FCURSOR_MARK + 1;
  FCURSOR_CIRCLE := FCURSOR_ZOOM + 1;

  Screen.Cursors[FCURSOR_PEN] := LoadCursor(hInstance, 'CURSOR_PEN');
  Screen.Cursors[FCURSOR_BOX] := LoadCursor(hInstance, 'CURSOR_BOX');
  Screen.Cursors[FCURSOR_MARK] := LoadCursor(hInstance, 'CURSOR_MARK');
  Screen.Cursors[FCURSOR_ZOOM] := LoadCursor(hInstance, 'CURSOR_ZOOM');
  Screen.Cursors[FCURSOR_CIRCLE] := LoadCursor(hInstance, 'CURSOR_CIRCLE');

  FCheck_Unchecked := TFslBitmapGraphic.Create;
  FCheck_Unchecked.LoadFromResource('check_unchecked');
  FCheck_Unchecked.Transparent := true;
  FCheck_Checked := TFslBitmapGraphic.Create;
  FCheck_Checked.LoadFromResource('check_checked');
  FCheck_Checked.Transparent := true;

  FCheck_Unchecked_High := TFslBitmapGraphic.Create;
  FCheck_Unchecked_High.LoadFromResource('check_unchecked_high');
  FCheck_Unchecked_High.Transparent := true;
  FCheck_Checked_High := TFslBitmapGraphic.Create;
  FCheck_Checked_High.LoadFromResource('check_checked_high');
  FCheck_Checked_High.Transparent := true;
End;


Destructor TWPIconModule.Destroy;
Begin
  FCheck_Checked.Free;
  FCheck_UnChecked.Free;
  FCheck_Checked_High.Free;
  FCheck_UnChecked_High.Free;
  FImages.Free;

  Inherited;
End;


{$IFOPT C+}
Function TWPIconModule.INVALID: Integer;
Begin
  Result := -2;
End;
{$ENDIF}


Function TWPIconModule.NONE : Integer;
Begin
  Result := -1;
End;


Function TWPIconModule.CUT : Integer;
Begin
  Result := 0;
End;


Function TWPIconModule.Copy : Integer;
Begin
  Result := 1;
End;


Function TWPIconModule.PASTE : Integer;
Begin
  Result := 2;
End;


Function TWPIconModule.UNDO : Integer;
Begin
  Result := 3;
End;


Function TWPIconModule.REDO : Integer;
Begin
  Result := 4;
End;


Function TWPIconModule.SEARCH : Integer;
Begin
  Result := 5;
End;


Function TWPIconModule.BOLD : Integer;
Begin
  Result := 6;
End;


Function TWPIconModule.ITALIC : Integer;
Begin
  Result := 7;
End;


Function TWPIconModule.UNDERLINE : Integer;
Begin
  Result := 8;
End;


Function TWPIconModule.SUPERSCRIPT : Integer;
Begin
  Result := 9;
End;


Function TWPIconModule.SUBSCRIPT : Integer;
Begin
  Result := 10;
End;


Function TWPIconModule.ALIGN_LEFT : Integer;
Begin
  Result := 11;
End;


Function TWPIconModule.ALIGN_CENTRE : Integer;
Begin
  Result := 12;
End;


Function TWPIconModule.ALIGN_RIGHT : Integer;
Begin
  Result := 13;
End;


Function TWPIconModule.ALIGN_JUSTIFY : Integer;
Begin
  Result := 14;
End;


Function TWPIconModule.NUMBERS : Integer;
Begin
  Result := 15;
End;


Function TWPIconModule.BULLETS : Integer;
Begin
  Result := 16;
End;


Function TWPIconModule.INDENT : Integer;
Begin
  Result := 17;
End;


Function TWPIconModule.OUTDENT : Integer;
Begin
  Result := 18;
End;


Function TWPIconModule.INSERT_TABLE : Integer;
Begin
  Result := 19;
End;


Function TWPIconModule.INSERT_IMAGE : Integer;
Begin
  Result := 20;
End;


Function TWPIconModule.INSERT_PAGEBREAK : Integer;
Begin
  Result := 21;
End;


function TWPIconModule.INSERT_PDF: Integer;
begin
  result := 79;
end;

Function TWPIconModule.INSERT_LINE : Integer;
Begin
  Result := 22;
End;


Function TWPIconModule.SHOW_HINTS : Integer;
Begin
  Result := 23;
End;


Function TWPIconModule.SPELLING : Integer;
Begin
  Result := 24;
End;


Function TWPIconModule.FORMAT_FONT : Integer;
Begin
  Result := 25;
End;


Function TWPIconModule.FORMAT_PARA : Integer;
Begin
  Result := 26;
End;


Function TWPIconModule.PASTE_SPECIAL : Integer;
Begin
  Result := 27;
End;


Function TWPIconModule.INSERT_FIELD : Integer;
Begin
  Result := 28;
End;


Function TWPIconModule.INSERT_SYMBOL : Integer;
Begin
  Result := 45;
End;


Function TWPIconModule.OBJECT_PROPERTIES : Integer;
Begin
  Result := 29;
End;


Function TWPIconModule.FIELD_NEXT : Integer;
Begin
  Result := 30;
End;


Function TWPIconModule.FIELD_PREVIOUS : Integer;
Begin
  Result := 31;
End;


Function TWPIconModule.FIELD_DELETE : Integer;
Begin
  Result := 32;
End;


Function TWPIconModule.COPIED_ROW_ABOVE : Integer;
Begin
  Result := 33;
End;


Function TWPIconModule.COPIED_ROW_BELOW : Integer;
Begin
  Result := 34;
End;

Function TWPIconModule.ROW_ABOVE : Integer;
Begin
  Result := 35;
End;


Function TWPIconModule.ROW_BELOW : Integer;
Begin
  Result := 36;
End;

Function TWPIconModule.SORT_TABLE : Integer;
Begin
  Result := 37;
End;

Function TWPIconModule.COLUMN_LEFT : Integer;
Begin
  Result := 38;
End;


Function TWPIconModule.COLUMN_RIGHT : Integer;
Begin
  Result := 39;
End;


Function TWPIconModule.SELECT_ROW : Integer;
Begin
  Result := 40;
End;


Function TWPIconModule.SELECT_TABLE : Integer;
Begin
  Result := 41;
End;


Function TWPIconModule.DELETE_TABLE : Integer;
Begin
  Result := 42;
End;


Function TWPIconModule.DELETE_ROW : Integer;
Begin
  Result := 43;
End;


Function TWPIconModule.DELETE_COLUMN : Integer;
Begin
  Result := 44;
End;


Function TWPIconModule.VOICE : Integer;
Begin
  Result := 45;
End;


Function TWPIconModule.CONVERTTOTABLE : Integer;
Begin
  Result := 46;
End;


Function TWPIconModule.MACRO : Integer;
Begin
  Result := 47;
End;


Function TWPIconModule.INSERT_SECTION_FIELD : Integer;
Begin
  Result := 48;
End;

Function TWPIconModule.INSERT_TEMPLATE: Integer;
Begin
  Result := 49;
End;

Function TWPIconModule.CHANGE_CASE: Integer;
Begin
  Result := 50;
End;

Function TWPIconModule.INSERT_HOTSPOT: Integer;
Begin
  Result := 51;
End;

Function TWPIconModule.IMAGE_SELECT : Integer;
Begin
  Result := 52;
End;

Function TWPIconModule.IMAGE_PEN : Integer;
Begin
  Result := 53;
End;

Function TWPIconModule.IMAGE_BOX : Integer;
Begin
  Result := 54;
End;

Function TWPIconModule.IMAGE_MARK : Integer;
Begin
  Result := 55;
End;

Function TWPIconModule.IMAGE_ZOOM : Integer;
Begin
  Result := 56;
End;

Function TWPIconModule.IMAGE_SELECT_ON: Integer;
Begin
  Result := 57;
End;

Function TWPIconModule.IMAGE_PEN_ON: Integer;
Begin
  Result := 58;
End;

Function TWPIconModule.IMAGE_BOX_ON: Integer;
Begin
  Result := 59;
End;

Function TWPIconModule.IMAGE_MARK_ON: Integer;
Begin
  Result := 60;
End;

Function TWPIconModule.IMAGE_ZOOM_ON: Integer;
Begin
  Result := 61;
End;

function TWPIconModule.IMAGE_CIRCLE: Integer;
begin
  Result := 62;
end;

function TWPIconModule.IMAGE_CIRCLE_ON: Integer;
begin
  Result := 63;
end;

function TWPIconModule.INSERT_COMMENT: Integer;
begin
  Result := 64;
end;

function TWPIconModule.FIRST: Integer;
begin
  Result := 65;
end;

function TWPIconModule.LAST: Integer;
begin
  Result := 66;
end;

function TWPIconModule.NEXT: Integer;
begin
  Result := 67;
end;

function TWPIconModule.PREV: Integer;
begin
  Result := 68;
end;

function TWPIconModule.SNOMED: Integer;
begin
  Result := 69;
end;

Function TWPIconModule.EXT_NEW : Integer;
Begin
  Result := 70;
End;

Function TWPIconModule.EXT_OPEN : Integer;
Begin
  Result := 71;
End;

Function TWPIconModule.EXT_SAVE : Integer;
Begin
  Result := 72;
End;

Function TWPIconModule.EXT_PRINT : Integer;
Begin
  Result := 73;
End;

Function TWPIconModule.EXT_IMPORT : Integer;
Begin
  Result := 74;
End;

Function TWPIconModule.EXT_EXPORT : Integer;
Begin
  Result := 75;
End;

Function TWPIconModule.EXT_PAGEDESIGN : Integer;
Begin
  Result := 76;
End;

Function TWPIconModule.EXT_EMAIL : Integer;
Begin
  Result := 77;
End;


Var
  gWPIconModule : TWPIconModule = Nil;


Function WPIconModule : TWPIconModule;
Begin
  If Not Assigned(gWPIconModule) Then
    gWPIconModule := TWPIconModule.Create;

  Result := gWPIconModule;
End;



function TWPIconModule.CURSOR_BOX: Integer;
begin
  Result := FCURSOR_BOX;
end;

function TWPIconModule.CURSOR_MARK: Integer;
begin
  result := FCURSOR_MARK;
end;

function TWPIconModule.CURSOR_PEN: Integer;
begin
  result := FCURSOR_PEN;
end;

function TWPIconModule.CURSOR_ZOOM: Integer;
begin
  result := FCURSOR_ZOOM;
end;

function TWPIconModule.CURSOR_CIRCLE: Integer;
begin
  result := FCURSOR_CIRCLE;
end;

Initialization
Finalization
  gWPIconModule.Free;
  gWPIconModule:= Nil;
End.
