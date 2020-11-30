Unit fui_vclx_Dialogs;

{
Copyright (c) 2010+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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
  Classes, Dialogs, ExtDlgs,
  fsl_utilities;

Type
  TUixOpenFileDialog = Class(TOpenDialog)
    Private
      Function GetFolder : String;
      Procedure SetFolder(Const Value : String);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

    Public
      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure ApplyFilter(Const sName, sExtension : String); Overload; Virtual;

      Property Folder : String Read GetFolder Write SetFolder;
  End;

  TUixSaveFileDialog = Class(TSaveDialog)
    Private
      Function GetFolder : String;
      Procedure SetFolder(Const Value : String);

      Function GetAllowReadOnly: Boolean;
      Procedure SetAllowReadOnly(Const Value: Boolean);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

    Public
      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure ApplyFilter(Const sName, sExtension : String); Overload; Virtual;
      
      Property Folder : String Read GetFolder Write SetFolder;
      Property AllowReadOnly : Boolean Read GetAllowReadOnly Write SetAllowReadOnly;
  End;

  TUixOpenPictureDialog = Class(TOpenPictureDialog);

  TUixSavePictureDialog = Class(TSavePictureDialog);

  TUixOpenWordDocumentDialog = Class(TUixOpenFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixSaveWordDocumentDialog = Class(TUixSaveFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixOpenCSVDocumentDialog = Class(TUixOpenFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixSaveCSVDocumentDialog = Class(TUixSaveFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixOpenTextDialog = Class(TUixOpenFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixSaveTextDialog = Class(TUixSaveFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;

  TUixExportDocumentDialog = Class(TUixSaveFileDialog)
    Protected
      Procedure Initialise; Overload; Override;
  End;


Const
  FILTER_ANYFILE = 'Any file (*.*)|*.*';
  FILTER_WORDDOCUMENT = 'Microsoft Word Document (*.DOC)|*.DOC';
  FILTER_RICHTEXTFILE = 'Rich Text Document (*.RTF)|*.RTF';
  FILTER_EXCELSPREADSHEET = 'Microsoft Excel Spreadsheet (*.XLS)|*.XLS';
  FILTER_CSVDOCUMENT = 'CSV File (*.CSV)|*.CSV';
  FILTER_TEXTDOCUMENT = 'Text Document (*.TXT)|*.TXT';
  FILTER_WEBDOCUMENT = 'Web Document (*.HTM)|*.HTM';

  EXTENSION_RICHTEXTFILE = 'RTF';
  EXTENSION_WORDDOCUMENT = 'DOC';
  EXTENSION_EXCELSPREADSHEET = 'XLS';
  EXTENSION_CSVDOCUMENT = 'CSV';
  EXTENSION_TEXTDOCUMENT = 'TXT';
  EXTENSION_WEBDOCUMENT = 'HTM';


Procedure Register;


Implementation


Procedure Register;
Begin
  RegisterComponents('Uix', [TUixOpenFileDialog, TUixSaveFileDialog, TUixOpenPictureDialog, TUixSavePictureDialog]);
End;


Procedure TUixOpenFileDialog.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixOpenFileDialog.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixOpenFileDialog.Initialise;
Begin
  Options := Options + [ofFileMustExist, ofPathMustExist, ofEnableSizing];
End;


Procedure TUixOpenFileDialog.Finalise;
Begin
End;


Function TUixOpenFileDialog.GetFolder : String;
Begin
  Result := InitialDir;
End;


Procedure TUixOpenFileDialog.SetFolder(Const Value : String);
Begin
  InitialDir := Value;
End;


Procedure TUixOpenFileDialog.ApplyFilter(Const sName, sExtension: String);
Begin
  Filter := StringFormat('%0:s (*.%1:s)|*.%1:s|%2:s', [sName, sExtension, FILTER_ANYFILE]);
  DefaultExt := sExtension;
End;


Procedure TUixSaveFileDialog.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixSaveFileDialog.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixSaveFileDialog.Initialise;
Begin
  Options := Options + [ofPathMustExist, ofOverwritePrompt, ofEnableSizing];
End;


Procedure TUixSaveFileDialog.Finalise;
Begin
End;


Function TUixSaveFileDialog.GetAllowReadOnly: Boolean;
Begin
  Result := Options * [ofHideReadOnly, ofNoReadOnlyReturn] = [];
End;


Procedure TUixSaveFileDialog.SetAllowReadOnly(Const Value: Boolean);
Begin
  If Value Then
    Options := Options + [ofHideReadOnly, ofNoReadOnlyReturn]
  Else
    Options := Options - [ofHideReadOnly, ofNoReadOnlyReturn];
End;


Function TUixSaveFileDialog.GetFolder : String;
Begin
  Result := InitialDir;
End;


Procedure TUixSaveFileDialog.SetFolder(Const Value : String);
Begin
  InitialDir := Value;
End;


Procedure TUixOpenWordDocumentDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_WORDDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_WORDDOCUMENT;
End;


Procedure TUixSaveWordDocumentDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_WORDDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_WORDDOCUMENT;
End;


Procedure TUixOpenCSVDocumentDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_CSVDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_CSVDOCUMENT;
End;


Procedure TUixSaveCSVDocumentDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_CSVDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_CSVDOCUMENT;
End;


Procedure TUixExportDocumentDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_CSVDOCUMENT + '|' + FILTER_WORDDOCUMENT + '|' + FILTER_RICHTEXTFILE + '|' + FILTER_WEBDOCUMENT + '|' + FILTER_TEXTDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_CSVDOCUMENT;
End;


Procedure TUixOpenTextDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_TEXTDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_TEXTDOCUMENT;
End;


Procedure TUixSaveTextDialog.Initialise;
Begin
  Inherited;

  Filter := FILTER_TEXTDOCUMENT + '|' + FILTER_ANYFILE;
  DefaultExt := EXTENSION_TEXTDOCUMENT;
End;


Procedure TUixSaveFileDialog.ApplyFilter(Const sName, sExtension: String);
Begin
  Filter := StringFormat('%0:s (*.%1:s)|*.%1:s|%2:s', [sName, sExtension, FILTER_ANYFILE]);
  DefaultExt := sExtension;
End;


End.
