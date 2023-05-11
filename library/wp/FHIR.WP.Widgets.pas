unit FHIR.WP.Widgets;

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

interface

Type
  TWPToolbarWidget =
    (
     tbwCut, tbwCopy, tbwCopyAll, tbwPaste, tbwPasteSpecial,
     tbwUndo, tbwRedo,
     tbwSearch, tbwPlayback, tbwSpelling,
     tbwStyle, tbwFontName, tbwFontSize,
     tbwFontColour, tbwBackColor,
     tbwBold, tbwItalic, tbwUnderline,
     tbwSuperscript, tbwSubscript, tbwChangeCase,
     tbwLeft, tbwCentre, tbwRight, tbwJustify, tbwBullet, tbwNumber, tbwIndent, tbwOutdent,
     tbwInsertTable, tbwInsertImage, tbwInsertLine, tbwInsertPageBreak,
     tbwEditHints, tbwMacro, tbwInsertField, tbwInsertTemplate, tbwInsertSymbol,
     tbwNew, tbwOpen, tbwSave, tbwSaveAs, tbwPrint, tbwImport, tbwExport, tbwPageDesign
    );

  TWPToolbarWidgets = Set Of TWPToolbarWidget;

  TWPToolbarWidgetGroup = (tbgExternals, tbgOperations, tbgHistoryOperations, tbgExtOperations, tbgStyle, tbgExtStyle, tbgFont, tbgExtFont, tbgPara, tbgInsert, tbgMisc);
  TWPToolbarWidgetGroups = Set Of TWPToolbarWidgetGroup;

  TWPToolbarWidgetImageArray = Array[TWPToolbarWidget] Of Integer;


Const
  NAMES_TOOLBARWIDGET : Array [TWPToolbarWidget] Of String = (
        'Cut', 'Copy', 'CopyAll', 'Paste', 'PasteSpecial',
        'Undo', 'Redo', 'Search', 'Playback', 'Spelling', 'Style', 'FontName', 'FontSize',
        'FontColour', 'BackColor', 'Bold', 'Italic', 'Underline', 'Superscript', 'Subscript', 'ChangeCase',
        'Left', 'Centre', 'Right', 'Justify', 'Bullet', 'Number', 'Indent', 'Outdent',
        'InsertTable', 'InsertImage', 'InsertLine', 'InsertPageBreak', 'EditHints', 'Macro', 'InsertField', 'InsertTemplate', 'InsertSymbol',
        'New', 'Open', 'Save', 'SaveAs', 'Print', 'Import', 'Export', 'PageDesign');

  CAPTIONS_TOOLBARWIDGET : Array [TWPToolbarWidget] Of String = (
        'Cut', 'Copy', 'Copy All', 'Paste', 'Paste Special',
        'Undo', 'Redo', 'Search', 'Replay Dictation', 'Spelling', 'Style', 'Font Name', 'Font Size',
        'Font Colour', 'Background Color', 'Bold', 'Italic', 'Underline', 'Superscript', 'Subscript', 'ChangeCase',
        'Left Justify', 'Centre Justify', 'Right Justify', 'Block Justify', 'Bulleted', 'Numbered', 'Indent Paragraph', 'Unindent Paragraph',
        'Insert Table', 'Insert Image', 'Insert Horizontal Line', 'Insert Page Break', 'Show Paragraph markers', 'Expand as a Macro', 'Insert Field', 'Insert Template', 'Insert Symbol',
        'New', 'Open', 'Save', 'Save As', 'Print', 'Import', 'Export', 'Page Design');

  NAMES_TOOLBARWIDGETGROUP : Array [TWPToolbarWidgetGroup] Of String = ('Externals', 'Operations', 'HistoryOperations', 'ExtOperations', 'Style', 'ExtStyle', 'Font', 'ExtFont', 'Paragraph', 'Insert', 'Misc');

  TITLES_TOOLBARWIDGETGROUP : Array [TWPToolbarWidgetGroup] Of String = ('Externals', 'Operations', 'History Operations', 'Extended Operations', 'Style Details', 'Extended Style Details', 'Font Details', 'Extended Font Details', 'Paragraph Details', 'Insertion Operations', 'Miscellaineous');

  TOOLBARWIDGET_GROUPS : Array [TWPToolbarWidgetGroup] Of TWPToolbarWidgets =
    (
     [tbwNew, tbwOpen, tbwSave, tbwSaveAs, tbwPrint, tbwImport, tbwExport, tbwPageDesign],
     [tbwCut, tbwCopy, tbwCopyAll, tbwPaste, tbwPasteSpecial],
     [tbwUndo, tbwRedo],
     [tbwSearch, tbwPlayback, tbwSpelling, tbwMacro],
     [tbwStyle, tbwFontName, tbwFontSize],
     [tbwFontColour, tbwBackColor],
     [tbwBold, tbwItalic, tbwUnderline],
     [tbwSuperscript, tbwSubscript, tbwChangeCase],
     [tbwLeft, tbwCentre, tbwRight, tbwJustify, tbwBullet, tbwNumber, tbwIndent, tbwOutdent],
     [tbwInsertTable, tbwInsertImage, tbwInsertLine, tbwInsertPageBreak, tbwInsertField, tbwInsertTemplate, tbwInsertSymbol],
     [tbwEditHints]
    );


implementation

end.
