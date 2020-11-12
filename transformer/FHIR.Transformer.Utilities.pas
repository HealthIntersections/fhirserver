unit FHIR.Transformer.Utilities;

{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  WinApi.Windows, SysUtils, Messages, Forms, Classes, Dialogs, Graphics, Controls, StdCtrls, Consts,
  ScintEdit, ScintInt,
  fsl_base, fsl_utilities, fsl_stream;

const
  { Memo marker numbers }
  mmIconHasEntry = 0;        { grey dot }
  mmIconEntryProcessed = 1;  { green dot }
  mmIconBreakpoint = 2;      { stop sign }
  mmIconBreakpointGood = 3;  { stop sign + check }
  mmIconBreakpointBad = 4;   { stop sign + X }
  mmLineError = 10;          { red line highlight }
  mmLineBreakpoint = 11;     { red line highlight }
  mmLineBreakpointBad = 12;  { ugly olive line highlight }
  mmLineStep = 13;           { blue line highlight }
  inSquiggly = 0;
  inPendingSquiggly = 1;


type
  TTreeDataPointer = record
    obj : TFslObject;
    obj2 : TFslObject;
  end;
  PTreeDataPointer = ^TTreeDataPointer;

  TPathSelection = class (TFslObject)
  private
    FcolStart: integer;
    FlineStart: integer;
    FcolEnd: integer;
    Fcaption: String;
    FlineEnd: integer;
  public
    constructor create(caption : String; lineStart, colStart, lineEnd, colEnd : integer);
    property caption : String read Fcaption write Fcaption;
    property lineStart : integer read FlineStart write FlineStart;
    property colStart : integer read FcolStart write FcolStart;
    property lineEnd : integer read FlineEnd write FlineEnd;
    property colEnd : integer read FcolEnd write FcolEnd;
  end;

  TIDEScintEdit = class(TScintEdit)
  protected
    procedure CreateWnd; override;
  end;

  TMsgBoxType = (mbInformation, mbConfirmation, mbError, mbCriticalError);

function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
function MsgBoxFmt(const Text: String; const Args: array of const; const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
function isXml(s : String) : boolean;

implementation

var
  MessageBoxCaptions: array[TMsgBoxType] of PChar;

function isXml(s : String) : boolean;
begin
  if not s.Contains('<') then
    result := false
  else if not s.Contains('{') then
    result := true
  else
    result := s.IndexOf('<') < s.IndexOf('{');
end;


function MsgBoxFmt(const Text: String; const Args: array of const; const Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
begin
  Result := MsgBox(Format(Text, Args), Caption, Typ, Buttons);
end;

function MsgBox(const Text, Caption: String; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
begin
  Result := MsgBoxP(PChar(Text), PChar(Caption), Typ, Buttons);
end;

function MsgBoxP(const Text, Caption: PChar; const Typ: TMsgBoxType; const Buttons: Cardinal): Integer;
const
  IconFlags: array[TMsgBoxType] of Cardinal = (MB_ICONINFORMATION, MB_ICONQUESTION, MB_ICONEXCLAMATION, MB_ICONSTOP);
//  DefaultCaptions: array[TMsgBoxType] of Word = (SMsgDlgInformation, SMsgDlgConfirm, SMsgDlgError, SMsgDlgError);
var
  C: PChar;
  NewCaption: String;
begin
  C := Caption;
  if (C = nil) or (C[0] = #0) then begin
    C := MessageBoxCaptions[Typ];
    if C = nil then begin
//      NewCaption := LoadStr(DefaultCaptions[Typ]);
      C := PChar(NewCaption);
    end;
  end;
//  Result := AppMessageBox(Text, C, Buttons or IconFlags[Typ]);
end;


{ TPathSelection }

constructor TPathSelection.create(caption: String; lineStart, colStart, lineEnd, colEnd: integer);
begin
  Inherited Create;
  FCaption := caption;
  FlineStart := lineStart;
  FcolStart := colStart;
  FlineEnd := lineEnd;
  FcolEnd := colEnd;
end;


{ TIDEScintEdit }

procedure TIDEScintEdit.CreateWnd;
const
  PixmapHasEntry: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #808080',
    '. c #c0c0c0',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapEntryProcessed: array[0..8] of PAnsiChar = (
    '5 5 2 1',
    'o c #008000',
    '. c #00ff00',
    'ooooo',
    'o...o',
    'o...o',
    'o...o',
    'ooooo',
    nil);
  PixmapBreakpoint: array[0..14] of PAnsiChar = (
    '9 10 3 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    'o.......o',
    '=o.....o=',
    '==ooooo==',
    nil);
  PixmapBreakpointGood: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #00ff00',
    '======oo=',
    '==oooo**o',
    '=o....*o=',
    'o....**.o',
    'o....*..o',
    'o...**..o',
    'o**.*...o',
    'o.***...o',
    '=o.*...o=',
    '==ooooo==',
    nil);
  PixmapBreakpointBad: array[0..15] of PAnsiChar = (
    '9 10 4 1',
    '= c none',
    'o c #000000',
    '. c #ff0000',
    '* c #ffff00',
    '=========',
    '==ooooo==',
    '=o.....o=',
    'o.*...*.o',
    'o.**.**.o',
    'o..***..o',
    'o.**.**.o',
    'o.*...*.o',
    '=o.....o=',
    '==ooooo==',
    nil);
//const
//  SC_MARK_BACKFORE = 3030;  { new marker type added in my Scintilla build }
begin
  inherited;

  Call(SCI_SETCARETWIDTH, 2, 0);
  Call(SCI_AUTOCSETAUTOHIDE, 0, 0);
  Call(SCI_AUTOCSETCANCELATSTART, 0, 0);
  Call(SCI_AUTOCSETDROPRESTOFWORD, 1, 0);
  Call(SCI_AUTOCSETIGNORECASE, 1, 0);
  Call(SCI_AUTOCSETMAXHEIGHT, 7, 0);

  Call(SCI_ASSIGNCMDKEY, Ord('Z') or ((SCMOD_SHIFT or SCMOD_CTRL) shl 16), SCI_REDO);

  Call(SCI_SETSCROLLWIDTH, 1024 * CallStr(SCI_TEXTWIDTH, 0, 'X'), 0);

//  Call(SCI_INDICSETSTYLE, inSquiggly, INDIC_SQUIGGLE);
//  Call(SCI_INDICSETFORE, inSquiggly, clRed);
//  Call(SCI_INDICSETSTYLE, inPendingSquiggly, INDIC_HIDDEN);

  Call(SCI_SETMARGINTYPEN, 1, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINWIDTHN, 1, 21);
  Call(SCI_SETMARGINSENSITIVEN, 1, 1);
  Call(SCI_SETMARGINCURSORN, 1, SC_CURSORARROW);
  Call(SCI_SETMARGINTYPEN, 2, SC_MARGIN_BACK);
  Call(SCI_SETMARGINMASKN, 2, 0);
  Call(SCI_SETMARGINWIDTHN, 2, 1);
  Call(SCI_SETMARGINTYPEN, 3, SC_MARGIN_SYMBOL);
  Call(SCI_SETMARGINMASKN, 3, 0);
  Call(SCI_SETMARGINWIDTHN, 3, 1);
  Call(SCI_SETMARGINLEFT, 0, 2);

  Call(SCI_MARKERDEFINEPIXMAP, mmIconHasEntry, LPARAM(@PixmapHasEntry));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconEntryProcessed, LPARAM(@PixmapEntryProcessed));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpoint, LPARAM(@PixmapBreakpoint));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointGood, LPARAM(@PixmapBreakpointGood));
  Call(SCI_MARKERDEFINEPIXMAP, mmIconBreakpointBad, LPARAM(@PixmapBreakpointBad));
  Call(SCI_MARKERDEFINE, mmLineError, SC_MARK_BACKGROUND);
  Call(SCI_MARKERSETFORE, mmLineError, clWhite);
  Call(SCI_MARKERSETBACK, mmLineError, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpoint, SC_MARK_CIRCLE);
  Call(SCI_MARKERSETFORE, mmLineBreakpoint, clWhite);
  Call(SCI_MARKERSETBACK, mmLineBreakpoint, clRed);
  Call(SCI_MARKERDEFINE, mmLineBreakpointBad, SC_MARK_CIRCLE);
  Call(SCI_MARKERSETFORE, mmLineBreakpointBad, clLime);
  Call(SCI_MARKERSETBACK, mmLineBreakpointBad, clOlive);
  Call(SCI_MARKERDEFINE, mmLineStep, SC_MARK_BACKGROUND);
  Call(SCI_MARKERSETFORE, mmLineStep, clWhite);
  Call(SCI_MARKERSETBACK, mmLineStep, clLime);
  TabWidth := 4;
end;


end.
