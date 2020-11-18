Unit fui_vclx_base;

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
  Windows, Graphics, Math, StdCtrls, ExtCtrls, Dialogs, SysUtils, Controls, Forms, Classes, Menus,
  fsl_utilities, fsl_collections,
  wp_graphics;


Type
  TRect = wp_graphics.TRect;
  TPoint = wp_graphics.TPoint;
  TColour = fsl_utilities.TColour;
  TUixIdentifier = Integer;
  TUixEvent = Procedure (oSender : TObject) Of Object;


Const
  POINT_NULL : TPoint = (X : -1; Y : -1);
  RECT_NULL : TRect = (Left : -1; Top : -1; Right : -1; Bottom : -1);


Function PointEqual(Const aA, aB : TPoint) : Boolean; Overload;

Function PointInRect(Const aPoint : TPoint; Const aRect : TRect) : Boolean; Overload;

Function HorizontalScrollBarHeight : Integer; Overload;
Function VerticalScrollBarWidth : Integer; Overload;

Function MouseWheelLines : Integer; Overload;
Function TaskBarHeight : Integer;
Function TaskBarWidth : Integer;
Function TitleBarHeight : Integer; Overload;
Function VerticalBorderWidth : Integer; Overload;
Function HorizontalBorderWidth : Integer; Overload;

Procedure ColourGradientHorizontal(aHandle : LongWord; Const aRect : TRect; Const aLeft, aRight : TColour); Overload;
Procedure ColourGradientVertical(aHandle : LongWord; Const aRect : TRect; Const aTop, aBottom : TColour); Overload;

Procedure RoundWindow(aHandle : THandle; Const iHeight : Integer; Const bTitlebar : Boolean); Overload;

Function SupportsShadows : Boolean;
Function MakeWindowTransparent(Wnd : HWND; iAlpha : Integer = 10) : Boolean;


Const
  ssCtrl = Classes.ssCtrl;
  ssAlt = Classes.ssAlt;
  ssShift = Classes.ssShift;

  vkShiftLeft = Windows.VK_LSHIFT;
  vkShiftRight = Windows.VK_RSHIFT;
  vkControlLeft = Windows.VK_LCONTROL;
  vkControlRight = Windows.VK_RCONTROL;
  vkAltLeft = Windows.VK_LMENU;
  vkAltRight = Windows.VK_RMENU;

  vkShift = Windows.VK_SHIFT;
  vkControl = Windows.VK_CONTROL;
  vkAlt = Windows.VK_MENU;
  vkEscape = Windows.VK_ESCAPE;
  vkEnter = Windows.VK_RETURN;
  vkTab = Windows.VK_TAB;
  vkLeft = Windows.VK_LEFT;
  vkRight = Windows.VK_RIGHT;
  vkUp = Windows.VK_UP;
  vkDown = Windows.VK_DOWN;
  vkBack = Windows.VK_BACK;
  vkDel = Windows.VK_DELETE;
  vkSpace = Windows.VK_SPACE;
  vkCapital = Windows.VK_CAPITAL;
  vkEnd = Windows.VK_END;
  vkHome = Windows.VK_HOME;
  vkPageUp = Windows.VK_PRIOR;
  vkPageDown = Windows.VK_NEXT;
  vkInsert = Windows.VK_INSERT;

  vk0 = Ord('0');
  vk1 = Ord('1');
  vk2 = Ord('2');
  vk3 = Ord('3');
  vk4 = Ord('4');
  vk5 = Ord('5');
  vk6 = Ord('6');
  vk7 = Ord('7');
  vk8 = Ord('8');
  vk9 = Ord('9');
  vkA = Ord('A');
  vkB = Ord('B');
  vkC = Ord('C');
  vkD = Ord('D');
  vkE = Ord('E');
  vkF = Ord('F');
  vkG = Ord('G');
  vkH = Ord('H');
  vkI = Ord('I');
  vkJ = Ord('J');
  vkK = Ord('K');
  vkL = Ord('L');
  vkM = Ord('M');
  vkN = Ord('N');
  vkO = Ord('O');
  vkP = Ord('P');
  vkQ = Ord('Q');
  vkR = Ord('R');
  vkS = Ord('S');
  vkT = Ord('T');
  vkU = Ord('U');
  vkV = Ord('V');
  vkW = Ord('W');
  vkX = Ord('X');
  vkY = Ord('Y');
  vkZ = Ord('Z');

  vkF1 = VK_F1;
  vkF2 = VK_F2;
  vkF3 = VK_F3;
  vkF4 = VK_F4;
  vkF5 = VK_F5;
  vkF6 = VK_F6;
  vkF7 = VK_F7;
  vkF8 = VK_F8;
  vkF9 = VK_F9;
  vkF10 = VK_F10;
  vkF11 = VK_F11;
  vkF12 = VK_F12;

  vkSemiColon = 186;
  vkEquals = 187;
  vkComma = 188;
  vkMinus = 189;
  vkFullStop = 190;
  vkSlash = 191;
  vkLeftQuote = 192;
  vkSquareLeft = 219;
  vkSquareRight = 221;
  vkSingleQuote = 222;
  vkBackSlash = 220;
  vkRightMouse = 250 Shl 8;

  vkNum0 = Windows.VK_NUMPAD0;
  vkNum1 = Windows.VK_NUMPAD1;
  vkNum2 = Windows.VK_NUMPAD2;
  vkNum3 = Windows.VK_NUMPAD3;
  vkNum4 = Windows.VK_NUMPAD4;
  vkNum5 = Windows.VK_NUMPAD5;
  vkNum6 = Windows.VK_NUMPAD6;
  vkNum7 = Windows.VK_NUMPAD7;
  vkNum8 = Windows.VK_NUMPAD8;
  vkNum9 = Windows.VK_NUMPAD9;
  vkNumDecimal = Windows.VK_DECIMAL;
  vkNumDivide = Windows.VK_DIVIDE;
  vkNumMultiply = Windows.VK_MULTIPLY;
  vkNumSubtract = Windows.VK_SUBTRACT;
  vkNumAdd = Windows.VK_ADD;

  vkContextMenu = 93;


Type
  TShiftState = Classes.TShiftState;

  TShortcut = Classes.TShortcut;


Function IsCapitalKeyToggled : Boolean; Overload;
Function IsControlKeyPressed : Boolean; Overload;
Function IsShiftKeyPressed : Boolean; Overload;
Function IsAltKeyPressed : Boolean; Overload;

Function VirtualKeyToShortcut(Const iKey : Word; Const aShift : TShiftState) : TShortcut; Overload;
Procedure ShortcutToVirtualKey(Const aShortcut : TShortcut; Out iKey : Word; Out aShift : TShiftState); Overload;

Function StringToShortcut(Const sText : String) : TShortcut; Overload;
Function ShortcutToString(Const aShortcut : TShortcut) : String; Overload;

Function CurrentShiftState : TShiftState; Overload;


Type
{$IFNDEF VER130}
  TModalResult = Controls.TModalResult;
{$ELSE}
  TModalResult = Forms.TModalResult;
{$ENDIF}


Const
  mrNone = Controls.mrNone;
  mrOK = Controls.mrOK;
  mrCancel = Controls.mrCancel;
  mrAbort = Controls.mrAbort;
  mrRetry = Controls.mrRetry;
  mrIgnore = Controls.mrIgnore;
  mrYes = Controls.mrYes;
  mrNo = Controls.mrNo;
  mrAll = Controls.mrAll;


Function DialogExecute(oForm : TForm) : TModalResult; Overload;
Procedure DialogStatement(Const sText : String; iContext : Integer = 0); Overload;
Procedure DialogException(E : Exception); Overload;
Procedure DialogError(Const sText : String; iContext : Integer = 0); Overload;
Procedure DialogWarning(Const sText : String; iContext : Integer = 0); Overload;
Function DialogQuestion(Const sText : String; iContext : Integer = 0) : Boolean; Overload;
Function DialogOption(Const sText : String; iContext : Integer = 0) : TModalResult; Overload;

Function DialogCustom(Const sText : String; aType : TMsgDlgType; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Function DialogCustomWarning(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Function DialogCustomInformation(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Function DialogCustomConfirmation(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Function DialogCustomError(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;


Type
  TUixKey =
    (kk_NULL,
    kk_CTRL_A,
    kk_CTRL_B,
    kk_CTRL_C,
    kk_CTRL_D,
    kk_CTRL_E,
    kk_CTRL_F,
    kk_CTRL_G,
    kk_BS,
    kk_TAB,
    kk_LF,
    kk_CTRL_K,
    kk_CTRL_L,
    kk_CR,
    kk_CTRL_N,
    kk_CTRL_O,
    kk_CTRL_P,
    kk_CTRL_Q,
    kk_CTRL_R,
    kk_CTRL_S,
    kk_CTRL_T,
    kk_CTRL_U,
    kk_CTRL_V,
    kk_CTRL_W,
    kk_CTRL_X,
    kk_CTRL_Y,
    kk_CTRL_Z,
    kk_ESCAPE,
    kk_FS,
    kk_GS,
    kk_RS,
    kk_US,
    kk_SPACE,
    kk_EXCLAMATION,
    kk_DOUBLEQUOTE,
    kk_HASH,
    kk_DOLLAR,
    kk_PERCENT,
    kk_AMPERSAND,
    kk_SINGLEQUOTE,
    kk_LEFTBRACKET,
    kk_RIGHTBRACKET,
    kk_ASTERISK,
    kk_PLUS,
    kk_COMMA,
    kk_MINUS,
    kk_DOT,
    kk_SLASH,
    kk_0,
    kk_1,
    kk_2,
    kk_3,
    kk_4,
    kk_5,
    kk_6,
    kk_7,
    kk_8,
    kk_9,
    kk_COLON,
    kk_SEMICOLON,
    kk_LESSTHAN,
    kk_EQUAL,
    kk_GREATERTHAN,
    kk_QUESTION,
    kk_AT,
    kk_A,
    kk_B,
    kk_C,
    kk_D,
    kk_E,
    kk_F,
    kk_G,
    kk_H,
    kk_I,
    kk_J,
    kk_K,
    kk_L,
    kk_M,
    kk_N,
    kk_O,
    kk_P,
    kk_Q,
    kk_R,
    kk_S,
    kk_T,
    kk_U,
    kk_V,
    kk_W,
    kk_X,
    kk_Y,
    kk_Z,
    kk_LEFTSQUAREBRACKET,
    kk_BACKSLASH,
    kk_RIGHTSQUAREBRACKET,
    kk_CARRET,
    kk_UNDERSCORE,
    kk_INVERTEDQUOTE,
    kk_LOWERCASE_A,
    kk_LOWERCASE_B,
    kk_LOWERCASE_C,
    kk_LOWERCASE_D,
    kk_LOWERCASE_E,
    kk_LOWERCASE_F,
    kk_LOWERCASE_G,
    kk_LOWERCASE_H,
    kk_LOWERCASE_I,
    kk_LOWERCASE_J,
    kk_LOWERCASE_K,
    kk_LOWERCASE_L,
    kk_LOWERCASE_M,
    kk_LOWERCASE_N,
    kk_LOWERCASE_O,
    kk_LOWERCASE_P,
    kk_LOWERCASE_Q,
    kk_LOWERCASE_R,
    kk_LOWERCASE_S,
    kk_LOWERCASE_T,
    kk_LOWERCASE_U,
    kk_LOWERCASE_V,
    kk_LOWERCASE_W,
    kk_LOWERCASE_X,
    kk_LOWERCASE_Y,
    kk_LOWERCASE_Z,
    kk_LEFTBRACE,
    kk_VERTICALBAR,
    kk_RIGHTBRACE,
    kk_TILDE,
    kk_DELETE,
    kk_128,
    kk_129,
    kk_130,
    kk_131,
    kk_132,
    kk_133,
    kk_134,
    kk_135,
    kk_136,
    kk_137,
    kk_138,
    kk_139,
    kk_140,
    kk_141,
    kk_142,
    kk_143,
    kk_144,
    kk_145,
    kk_146,
    kk_147,
    kk_148,
    kk_149,
    kk_150,
    kk_151,
    kk_152,
    kk_153,
    kk_154,
    kk_155,
    kk_156,
    kk_157,
    kk_158,
    kk_159,
    kk_160,
    kk_161,
    kk_162,
    kk_163,
    kk_164,
    kk_165,
    kk_166,
    kk_167,
    kk_168,
    kk_169,
    kk_170,
    kk_171,
    kk_172,
    kk_173,
    kk_174,
    kk_175,
    kk_176,
    kk_177,
    kk_178,
    kk_179,
    kk_180,
    kk_181,
    kk_182,
    kk_183,
    kk_184,
    kk_185,
    kk_186,
    kk_187,
    kk_188,
    kk_189,
    kk_190,
    kk_191,
    kk_192,
    kk_193,
    kk_194,
    kk_195,
    kk_196,
    kk_197,
    kk_198,
    kk_199,
    kk_200,
    kk_201,
    kk_202,
    kk_203,
    kk_204,
    kk_205,
    kk_206,
    kk_207,
    kk_208,
    kk_209,
    kk_210,
    kk_211,
    kk_212,
    kk_213,
    kk_214,
    kk_215,
    kk_216,
    kk_217,
    kk_218,
    kk_219,
    kk_220,
    kk_221,
    kk_222,
    kk_223,
    kk_224,
    kk_225,
    kk_226,
    kk_227,
    kk_228,
    kk_229,
    kk_230,
    kk_231,
    kk_232,
    kk_233,
    kk_234,
    kk_235,
    kk_236,
    kk_237,
    kk_238,
    kk_239,
    kk_240,
    kk_241,
    kk_242,
    kk_243,
    kk_244,
    kk_245,
    kk_246,
    kk_247,
    kk_248,
    kk_249,
    kk_250,
    kk_251,
    kk_252,
    kk_253,
    kk_254,
    kk_255,

    kk_CTRL_I,
    kk_INS,
    kk_HOME,
    kk_END,
    kk_PAGE_UP,
    kk_PAGE_DOWN,
    kk_UP,
    kk_DOWN,
    kk_LEFT,
    kk_RIGHT,
    kk_SHIFT_TAB,

    kk_HELP,
    kk_F2,
    kk_F3,
    kk_F4,
    kk_F5,
    kk_F6,
    kk_F7,
    kk_F8,
    kk_F9,
    kk_F10,
    kk_F11,
    kk_F12,

    kk_SHIFT_F1,
    kk_SHIFT_F2,
    kk_SHIFT_F3,
    kk_SHIFT_F4,
    kk_SHIFT_F5,
    kk_SHIFT_F6,
    kk_SHIFT_F7,
    kk_SHIFT_F8,
    kk_SHIFT_F9,
    kk_SHIFT_F10,
    kk_SHIFT_F11,
    kk_SHIFT_F12,

    kk_CTRL_F1,
    kk_CTRL_F2,
    kk_CTRL_F3,
    kk_CTRL_F4,
    kk_CTRL_F5,
    kk_CTRL_F6,
    kk_CTRL_F7,
    kk_CTRL_F8,
    kk_CTRL_F9,
    kk_CTRL_F10,
    kk_CTRL_F11,
    kk_CTRL_F12,

    kk_ALT_F1,
    kk_ALT_F2,
    kk_ALT_F3,
    kk_ALT_F4,
    kk_ALT_F5,
    kk_ALT_F6,
    kk_ALT_F7,
    kk_ALT_F8,
    kk_ALT_F9,
    kk_ALT_F10,
    kk_ALT_F11,
    kk_ALT_F12,

    kk_ALT_SPACE,
    kk_ALT_0,
    kk_ALT_1,
    kk_ALT_2,
    kk_ALT_3,
    kk_ALT_4,
    kk_ALT_5,
    kk_ALT_6,
    kk_ALT_7,
    kk_ALT_8,
    kk_ALT_9,
    kk_ALT_A,
    kk_ALT_B,
    kk_ALT_C,
    kk_ALT_D,
    kk_ALT_E,
    kk_ALT_F,
    kk_ALT_G,
    kk_ALT_H,
    kk_ALT_I,
    kk_ALT_J,
    kk_ALT_K,
    kk_ALT_L,
    kk_ALT_M,
    kk_ALT_N,
    kk_ALT_O,
    kk_ALT_P,
    kk_ALT_Q,
    kk_ALT_R,
    kk_ALT_S,
    kk_ALT_T,
    kk_ALT_U,
    kk_ALT_V,
    kk_ALT_W,
    kk_ALT_X,
    kk_ALT_Y,
    kk_ALT_Z,

    kk_CTRL_0,
    kk_CTRL_1,
    kk_CTRL_2,
    kk_CTRL_3,
    kk_CTRL_4,
    kk_CTRL_5,
    kk_CTRL_6,
    kk_CTRL_7,
    kk_CTRL_8,
    kk_CTRL_9,

    kk_Shift_Ctrl_F1,
    kk_Shift_Ctrl_F2,
    kk_Shift_Ctrl_F3,
    kk_Shift_Ctrl_F4,
    kk_Shift_Ctrl_F5,
    kk_Shift_Ctrl_F6,
    kk_Shift_Ctrl_F7,
    kk_Shift_Ctrl_F8,
    kk_Shift_Ctrl_F9,
    kk_Shift_Ctrl_F10,

    kk_RIGHT_MOUSE,

    kk_ALT_LEFT,
    kk_ALT_RIGHT,
    kk_ALT_HOME,
    kk_ALT_END,
    kk_CTRL_SPACE);


  TUixKeys = Class(TFslIntegerList)
    Private
      Function GetKey(Const iIndex: Integer): TUixKey;
      Procedure SetKey(Const iIndex: Integer; Const Value: TUixKey);

    Public
      Function Add(Const aKey : TUixKey) : Integer; Overload;
      Function Exists(Const aKey : TUixKey) : Boolean; Overload;
      Procedure Remove(Const aKey : TUixKey); Overload;

      Property Key[Const iIndex : Integer] : TUixKey Read GetKey Write SetKey; Default;
  End;


Const
  kk_Insert = kk_INS;

  kk_F1 = kk_HELP;
  kk_CTRL_H = kk_BS;
  kk_CTRL_J = kk_LF;
  kk_CTRL_M = kk_CR;
  kk_ENTER = kk_CR;
  kk_BACKSPACE = kk_BS;
  kk_ESC = kk_ESCAPE;

  UixKeyNameArray : Array [TUixKey] Of String =
    ('',
    'Ctrl+A',
    'Ctrl+B',
    'Ctrl+C',
    'Ctrl+D',
    'Ctrl+E',
    'Ctrl+F',
    'Ctrl+G',
    'BkSp',
    'Tab',
    'LF',
    'Ctrl+K',
    'Ctrl+L',
    'Enter',
    'Ctrl+N',
    'Ctrl+O',
    'Ctrl+P',
    'Ctrl+Q',
    'Ctrl+R',
    'Ctrl+S',
    'Ctrl+T',
    'Ctrl+U',
    'Ctrl+V',
    'Ctrl+W',
    'Ctrl+X',
    'Ctrl+Y',
    'Ctrl+Z',
    'Esc',
    'FS',
    'GS',
    'RS',
    'US',
    'Space',
    '!',
    '"',
    '#',
    '$',
    '%',
    '&',
    '''',
    '(',
    ')',
    '*',
    '+',
    ',',
    '-',
    '.',
    '/',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    ':',
    ';',
    '<',
    '=',
    '>',
    '?',
    '@',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '[',
    '\',
    ']',
    '^',
    '_',
    '`',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '{',
    '|',
    '}',
    '~',
    'Del',
    '128',
    '129',
    '130',
    '131',
    '132',
    '133',
    '134',
    '135',
    '136',
    '137',
    '138',
    '139',
    '140',
    '141',
    '142',
    '143',
    '144',
    '145',
    '146',
    '147',
    '148',
    '149',
    '150',
    '151',
    '152',
    '153',
    '154',
    '155',
    '156',
    '157',
    '158',
    '159',
    '160',
    '161',
    '162',
    '163',
    '164',
    '165',
    '166',
    '167',
    '168',
    '169',
    '170',
    '171',
    '172',
    '173',
    '174',
    '175',
    '176',
    '177',
    '178',
    '179',
    '180',
    '181',
    '182',
    '183',
    '184',
    '185',
    '186',
    '187',
    '188',
    '189',
    '190',
    '191',
    '192',
    '193',
    '194',
    '195',
    '196',
    '197',
    '198',
    '199',
    '200',
    '201',
    '202',
    '203',
    '204',
    '205',
    '206',
    '207',
    '208',
    '209',
    '210',
    '211',
    '212',
    '213',
    '214',
    '215',
    '216',
    '217',
    '218',
    '219',
    '220',
    '221',
    '222',
    '223',
    '224',
    '225',
    '226',
    '227',
    '228',
    '229',
    '230',
    '231',
    '232',
    '233',
    '234',
    '235',
    '236',
    '237',
    '238',
    '239',
    '240',
    '241',
    '242',
    '243',
    '244',
    '245',
    '246',
    '247',
    '248',
    '249',
    '250',
    '251',
    '252',
    '253',
    '254',
    '255',

    'Ctrl+I',
    'Ins',
    'Home',
    'End',
    'PgUp',
    'PgDn',
    'Up',
    'Down',
    'Left',
    'Right',
    'Shift+Tab',

    'F1',
    'F2',
    'F3',
    'F4',
    'F5',
    'F6',
    'F7',
    'F8',
    'F9',
    'F10',
    'F11',
    'F12',

    'Shift+F1',
    'Shift+F2',
    'Shift+F3',
    'Shift+F4',
    'Shift+F5',
    'Shift+F6',
    'Shift+F7',
    'Shift+F8',
    'Shift+F9',
    'Shift+F10',
    'Shift+F11',
    'Shift+F12',

    'Ctrl+F1',
    'Ctrl+F2',
    'Ctrl+F3',
    'Ctrl+F4',
    'Ctrl+F5',
    'Ctrl+F6',
    'Ctrl+F7',
    'Ctrl+F8',
    'Ctrl+F9',
    'Ctrl+F10',
    'Ctrl+F11',
    'Ctrl+F12',

    'Alt+F1',
    'Alt+F2',
    'Alt+F3',
    'Alt+F4',
    'Alt+F5',
    'Alt+F6',
    'Alt+F7',
    'Alt+F8',
    'Alt+F9',
    'Alt+F10',
    'Alt+F11',
    'Alt+F12',

    'Alt+Space',
    'Alt+0',
    'Alt+1',
    'Alt+2',
    'Alt+3',
    'Alt+4',
    'Alt+5',
    'Alt+6',
    'Alt+7',
    'Alt+8',
    'Alt+9',
    'Alt+A',
    'Alt+B',
    'Alt+C',
    'Alt+D',
    'Alt+E',
    'Alt+F',
    'Alt+G',
    'Alt+H',
    'Alt+I',
    'Alt+J',
    'Alt+K',
    'Alt+L',
    'Alt+M',
    'Alt+N',
    'Alt+O',
    'Alt+P',
    'Alt+Q',
    'Alt+R',
    'Alt+S',
    'Alt+T',
    'Alt+U',
    'Alt+V',
    'Alt+W',
    'Alt+X',
    'Alt+Y',
    'Alt+Z',
    'Ctrl+0',
    'Ctrl+1',
    'Ctrl+2',
    'Ctrl+3',
    'Ctrl+4',
    'Ctrl+5',
    'Ctrl+6',
    'Ctrl+7',
    'Ctrl+8',
    'Ctrl+9',
    'Shift+Ctrl+F1',
    'Shift+Ctrl+F2',
    'Shift+Ctrl+F3',
    'Shift+Ctrl+F4',
    'Shift+Ctrl+F5',
    'Shift+Ctrl+F6',
    'Shift+Ctrl+F7',
    'Shift+Ctrl+F8',
    'Shift+Ctrl+F9',
    'Shift+Ctrl+F10',
    'Menu',
    'Alt+Left',
    'Alt+Right',
    'Alt+Home',
    'Alt+End',
    'Ctrl+Space');

  UixKEY_CHARACTERS : Array [TUixKey] Of Char =
    (#0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    ' ',
    '!',
    '"',
    '#',
    '$',
    '%',
    '&',
    '''',
    '(',
    ')',
    '*',
    '+',
    ',',
    '-',
    '.',
    '/',
    '0',
    '1',
    '2',
    '3',
    '4',
    '5',
    '6',
    '7',
    '8',
    '9',
    ':',
    ';',
    '<',
    '=',
    '>',
    '?',
    '@',
    'A',
    'B',
    'C',
    'D',
    'E',
    'F',
    'G',
    'H',
    'I',
    'J',
    'K',
    'L',
    'M',
    'N',
    'O',
    'P',
    'Q',
    'R',
    'S',
    'T',
    'U',
    'V',
    'W',
    'X',
    'Y',
    'Z',
    '[',
    '\',
    ']',
    '^',
    '_',
    '`',
    'a',
    'b',
    'c',
    'd',
    'e',
    'f',
    'g',
    'h',
    'i',
    'j',
    'k',
    'l',
    'm',
    'n',
    'o',
    'p',
    'q',
    'r',
    's',
    't',
    'u',
    'v',
    'w',
    'x',
    'y',
    'z',
    '{',
    '|',
    '}',
    '~',
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,

    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0,
    #0);


Function NullKey : TUixKey; Overload;

Function UixKeyIsNull(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsWhitespace(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsSign(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsNumber(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsCapital(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsLowercase(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsVowel(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsAlphabet(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsConsonant(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsAlphanumeric(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsCardinal(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsInteger(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsReal(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsHexadecimal(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsCurrency(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsSpecial(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsSystem(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsFilename(Const aKey : TUixKey) : Boolean; Overload;
Function UixKeyIsKeyboard(Const aKey : TUixKey) : Boolean; Overload;

Function ToUixKey(aKey: Word; Const aShift: TShiftState): TUixKey; Overload;
Function UixKeyToShortCut(Const aKey: TUixKey): TShortCut; Overload;
Function ShortCutToUixKey(Const aShortCut : TShortCut): TUixKey; Overload;
Function CharacterToUixKey(Const sValue : Char): TUixKey; Overload;
Function StringToUixKey(Const sValue : String): TUixKey; Overload;
Function UixKeyToString(Const aKey: TUixKey): String; Overload;
Function UixKeyToChar(Const aKey: TUixKey): Char; Overload;
Function UixKeyUpper(Const aKey : TUixKey) : TUixKey; Overload;

Function NumberToUixKey(Const iValue : Integer): TUixKey; Overload;
Function UixKeyToNumber(Const aKey : TUixKey) : Integer; Overload;

Function UixKeyEquals(Const aA, aB : TUixKey) : Boolean; Overload;
Function UixKeyCompare(Const aA, aB : TUixKey) : Integer; Overload;
Function UixKeyCompareInsensitive(Const aA, aB : TUixKey) : Integer; Overload;
Function UixKeyCompareSensitive(Const aA, aB : TUixKey) : Integer; Overload;
Function UixKeyBetween(Const aLeft, aCheck, aRight : TUixKey) : Boolean; Overload;


Implementation


Const
  MSIMG32_DLL = 'msimg32.dll';


Type
  TColour16 = Word;

  PTriVertex = ^TTriVertex;
  TTriVertex = Packed Record
    X     : LongInt;
    Y     : LongInt;
    Red   : TColour16;
    Green : TColour16;
    Blue  : TColour16;
    Alpha : TColour16;
  End; { TTriVertex }

  TTriVertices = Array[0..MaxInt Div SizeOf(TTriVertex) - 1] Of TTriVertex;
  PTriVertices = ^TTriVertices;

  PGradientRect = ^TGradientRect;
  TGradientRect = Packed Record
    UpperLeft  : Cardinal;
    LowerRight : Cardinal;
  End; { TGradientRect }

  TColourGradientMode = (cgmHorizontal, cgmVertical);

  TGradientFill = Function (aHandle : LongWord; Var aVertices : TTriVertex; iVertices : ULONG; pGradients : Pointer; iCount, iMode : ULONG): BOOL; Stdcall;
  PGradientFill = ^TGradientFill;


Var
  GradientFill : TGradientFill;


Const
  GRADIENT_MODE : Array[TColourGradientMode] Of Cardinal = (GRADIENT_FILL_RECT_H, GRADIENT_FILL_RECT_V);


Function PointEqual(Const aA, aB : TPoint) : Boolean;
Begin
  Result := (aA.X = aB.X) And (aA.Y = aB.Y);
End;


Function HorizontalScrollBarHeight : Integer;
Begin
  Result := GetSystemMetrics(SM_CYHSCROLL);
End;


Function VerticalScrollBarWidth : Integer;
Begin
  Result := GetSystemMetrics(SM_CXVSCROLL);
End;


Function MouseWheelLines : Integer;
Begin
  If Not SystemParametersInfo(SPI_GETWHEELSCROLLLINES, 0, @Result, 0) Then
    Result := 3;
End;


Function PointInRect(Const aPoint : TPoint; Const aRect : TRect) : Boolean;
Begin
  Result := (aPoint.X >= aRect.Left) And (aPoint.X <= aRect.Right) And
            (aPoint.Y >= aRect.Top) And (aPoint.Y <= aRect.Bottom);
End;


Procedure ColourGradient(aHandle : LongWord; Const aRect : TRect; Const aLower, aUpper : TColour; Const aMode : TColourGradientMode);
Var
  aVertices : Array[0..1] Of TTriVertex;
  aGradient : TGradientRect;
Begin
  aVertices[0].X := aRect.Left;
  aVertices[0].Y := aRect.Top;
  aVertices[0].Red := TColourParts(aLower).Red Shl 8;
  aVertices[0].Green := TColourParts(aLower).Green Shl 8;
  aVertices[0].Blue := TColourParts(aLower).Blue Shl 8;
  aVertices[0].Alpha := 0;

  aVertices[1].X := aRect.Right;
  aVertices[1].Y := aRect.Bottom;
  aVertices[1].Red := TColourParts(aUpper).Red Shl 8;
  aVertices[1].Green := TColourParts(aUpper).Green Shl 8;
  aVertices[1].Blue := TColourParts(aUpper).Blue Shl 8;
  aVertices[1].Alpha := 0;

  aGradient.UpperLeft := 0;
  aGradient.LowerRight := 1;

  GradientFill(aHandle, aVertices[0], Length(aVertices), @aGradient, 1, GRADIENT_MODE[aMode]);
End;


Procedure ColourGradientHorizontal(aHandle : LongWord; Const aRect : TRect; Const aLeft, aRight : TColour);
Begin
  ColourGradient(aHandle, aRect, aLeft, aRight, cgmHorizontal);
End;


Procedure ColourGradientVertical(aHandle : LongWord; Const aRect : TRect; Const aTop, aBottom : TColour);
Begin
  ColourGradient(aHandle, aRect, aTop, aBottom, cgmVertical);
End;


Function DefaultGradientFill(aHandle : LongWord; Var aVertices : TTriVertex; iVertices : ULONG; pGradients : Pointer; iCount, iMode : ULONG): BOOL; Stdcall;
Var
  hBrush : THandle;
Begin
  hBrush := CreateSolidBrush(DWord(ColourCompose(aVertices.Red Shr 8, aVertices.Green Shr 8, aVertices.Blue Shr 8, aVertices.Alpha Shr 8)));
  Try
    Result := FillRect(aHandle, Classes.Rect(PTriVertices(@aVertices)[0].X, PTriVertices(@aVertices)[0].Y, PTriVertices(@aVertices)[1].X, PTriVertices(@aVertices)[1].Y), hBrush) <> 0;
  Finally
    DeleteObject(hBrush);
  End;
End;


Function HorizontalBorderWidth : Integer;
Begin
  Result := GetSystemMetrics(SM_CXFRAME);
End;


Function VerticalBorderWidth : Integer;
Begin
  Result := GetSystemMetrics(SM_CYFRAME);
End;


Function TaskBarHeight : Integer;
Var
  aRect : TRect;
  aHandle : HWND;
Begin
  aHandle := FindWindow('Shell_TrayWnd', '');

  If aHandle = 0 Then
  Begin
    Result := 0;
  End
  Else
  Begin
    GetWindowRect(aHandle, aRect);
    Result := aRect.Bottom - aRect.Top;
  End;
End;


Function TaskBarWidth : Integer;
Var
  aRect : TRect;
  aHandle : HWND;
Begin
  aHandle := FindWindow('Shell_TrayWnd', '');

  If aHandle = 0 Then
  Begin
    Result := 0;
  End
  Else
  Begin
    GetWindowRect(aHandle, aRect);
    Result := aRect.Right - aRect.Left;
  End;
End;


Function TitleBarHeight : Integer;
Begin
  Result := GetSystemMetrics(SM_CYCAPTION);
End;


Procedure RoundWindow(aHandle : THandle; Const iHeight : Integer; Const bTitlebar : Boolean);
Var
  aForm    : HRgn;
  aPolygon : HRgn;
  iX       : Integer;
  iY       : Integer;
  aPoints  : Array[0..27] Of TPoint;
Begin
  iX := HorizontalBorderWidth;
  iY := TitleBarHeight + VerticalBorderWidth;

  If bTitleBar Then
    aForm := CreateRectRgn(0, 0, iHeight + (2 * iX), iY)
  Else
    aForm := CreateRectRgn(0, 0, 0, 0);

  aPoints[0] := Point(3 + iX,63 + iY);
  aPoints[1] := Point(3 + iX,60 + iY);
  aPoints[2] := Point(6 + iX,57 + iY);
  aPoints[3] := Point(8 + iX,57 + iY);
  aPoints[4] := Point(8 + iX,56 + iY);
  aPoints[5] := Point(10 + iX,56 + iY);
  aPoints[6] := Point(519 + iX,56 + iY);
  aPoints[7] := Point(521 + iX,58 + iY);
  aPoints[8] := Point(522 + iX,58 + iY);
  aPoints[9] := Point(523 + iX,59 + iY);
  aPoints[10] := Point(523 + iX,60 + iY);
  aPoints[11] := Point(524 + iX,61 + iY);
  aPoints[12] := Point(524 + iX,438 + iY);
  aPoints[13] := Point(522 + iX,440 + iY);
  aPoints[14] := Point(519 + iX,443 + iY);
  aPoints[15] := Point(518 + iX,443 + iY);
  aPoints[16] := Point(517 + iX,444 + iY);
  aPoints[17] := Point(10 + iX,444 + iY);
  aPoints[18] := Point(9 + iX,442 + iY);
  aPoints[19] := Point(8 + iX,443 + iY);
  aPoints[20] := Point(7 + iX,443 + iY);
  aPoints[21] := Point(6 + iX,441 + iY);
  aPoints[22] := Point(3 + iX,438 + iY);
  aPoints[23] := Point(3 + iX,436 + iY);
  aPoints[24] := Point(2 + iX,435 + iY);
  aPoints[25] := Point(2 + iX,63 + iY);
  aPoints[26] := Point(3 + iX,63 + iY);

  aPolygon := CreatePolygonRgn(aPoints[0], 27, WINDING);

  CombineRgn(aForm, aForm, aPolygon, RGN_XOR);

  SetWindowRgn(aHandle, aForm, True);
End;


Function SupportsShadows : Boolean;
Var
  aInfo : TOSVersionInfo;
Begin
  FillChar(aInfo, SizeOf(aInfo), 0);

  aInfo.dwOSVersionInfoSize := SizeOf(aInfo);

  GetVersionEx(aInfo);

  Result := (aInfo.dwPlatformId >= VER_PLATFORM_WIN32_NT) And ((aInfo.dwMajorVersion > 5) Or ((aInfo.dwMajorVersion = 5) And (aInfo.dwMinorVersion >= 1)));
End;


Function MakeWindowTransparent(Wnd : HWND; iAlpha : Integer = 10) : Boolean;
Type
  TSetLayeredWindowAttributes = Function(hwnd: HWND; crKey: COLORREF; bAlpha: Byte; dwFlags: LongInt): LongInt; Stdcall;
Const
  LWA_ALPHA = 2;
  WS_EX_LAYERED = $80000;
Var
  aUser32 : HMODULE;
  SetLayeredWindowAttributes: TSetLayeredWindowAttributes;
Begin
  Result := False;

  aUser32 := GetModuleHandle('USER32.DLL');

  If aUser32 <> 0 Then
  Begin
    @SetLayeredWindowAttributes := GetProcAddress(aUser32, 'SetLayeredWindowAttributes');

    Result := Assigned(@SetLayeredWindowAttributes);

    If Result Then
    Begin
      SetWindowLong(Wnd, GWL_EXSTYLE, GetWindowLong(Wnd, GWL_EXSTYLE) Or WS_EX_LAYERED);

      SetLayeredWindowAttributes(Wnd, 0, Trunc((255 / 100) * (100 - iAlpha)), LWA_ALPHA);

      Result := True;
    End;
  End;
End;


Function IsCapitalKeyToggled : Boolean;
Begin
  Result := Lo(Windows.GetKeyState(vkCapital)) <> 0;
End;


Function IsControlKeyPressed : Boolean;
Begin
  Result := Hi(Windows.GetKeyState(vkControl)) <> 0;
End;


Function IsShiftKeyPressed : Boolean;
Begin
  Result := Hi(Windows.GetKeyState(vkShift)) <> 0;
End;


Function IsAltKeyPressed : Boolean;
Begin
  Result := Hi(Windows.GetKeyState(vkAlt)) <> 0;
End;


Function CurrentShiftState : TShiftState;
Begin
  Result := [];

  If IsControlKeyPressed Then
    Include(Result, ssCtrl);

  If IsShiftKeyPressed Then
    Include(Result, ssShift);

  If IsAltKeyPressed Then
    Include(Result, ssAlt);
End;


Function VirtualKeyToShortcut(Const iKey : Word; Const aShift : TShiftState) : TShortcut;
Begin
  Result := Menus.Shortcut(iKey, aShift);
End;


Procedure ShortcutToVirtualKey(Const aShortcut : TShortcut; Out iKey : Word; Out aShift : TShiftState);
Begin
  Menus.ShortcutToKey(aShortcut, iKey, aShift);
End;


Function StringToShortcut(Const sText : String) : TShortcut;
Begin
  Result := Menus.TextToShortcut(sText);
End;


Function ShortcutToString(Const aShortcut : TShortcut) : String;
Begin
  Result := Menus.ShortcutToText(aShortcut);
End;


Function DialogExecute(oForm : TForm) : TModalResult;
Var
  hActive : HWND;
Begin 
  hActive := GetActiveWindow;
  oForm.Show;
  Try
    If oForm.Owner Is TControl Then
      TControl(oForm.Owner).Enabled := False;

    oForm.Refresh;

    oForm.ModalResult := mrNone;

    While (oForm.ModalResult = mrNone) Do
    Begin
      Application.HandleMessage;

      If Application.Terminated Then
        oForm.ModalResult := mrCancel;

      If (oForm.ModalResult <> mrNone) Or Not oForm.Visible Then
      Begin
        If Not oForm.Visible Then
          oForm.ModalResult := mrCancel
        Else If Not oForm.CloseQuery Then
          oForm.ModalResult := mrNone;
      End;
    End;

    Result := oForm.ModalResult;
  Finally
    If oForm.Owner Is TControl Then
      TControl(oForm.Owner).Enabled := True;

    If hActive <> 0 Then
      SetActiveWindow(hActive);
  End;
End;


Function DialogMessage(Const sMessage : String; aType : TMsgDlgType; aButtons: TMsgDlgButtons; iHelp : LongInt): Integer;
Begin 
  Result := MessageDlg(sMessage, aType, aButtons, iHelp);
End;  


Procedure DialogStatement(Const sText : String; iContext : Integer);
Begin 
  DialogMessage(sText, mtInformation, [mbOK], iContext);
End;  


Function DialogQuestion(Const sText : String; iContext : Integer) : Boolean;
Begin 
  Result := DialogMessage(sText, mtConfirmation, [mbYes, mbNo], iContext) = mrYes;
End;


Function DialogOption(Const sText : String; iContext : Integer) : TModalResult;
Begin 
  Result := DialogMessage(sText, mtConfirmation, [mbYes, mbNo, mbCancel], iContext);
End;  


Procedure DialogError(Const sText : String; iContext : Integer);
Begin 
  DialogMessage(sText, mtError, [mbOk], iContext);
End;  


Procedure DialogWarning(Const sText : String; iContext : Integer = 0); Overload;
Begin 
  DialogMessage(sText, mtWarning, [mbOK], iContext);  
End;


Procedure DialogException(E : Exception);
Begin 
  DialogError(E.Message);
End;  


Type
  TDialogForm = Class(TForm)
    Procedure DoButtonClick(Sender : TObject);
  Private
    FSelected : TButton;
  Protected
  Public
    constructor CreateNew(AOwner: TComponent); Reintroduce;

    Property Selected : TButton Read FSelected Write FSelected;
  End;


Constructor TDialogForm.CreateNew(AOwner: TComponent);
Var
  aNonClientMetrics: TNonClientMetrics;
Begin
  Inherited CreateNew(AOwner);

  aNonClientMetrics.cbSize := SizeOf(aNonClientMetrics);

  If SystemParametersInfo(SPI_GETNONCLIENTMETRICS, 0, @aNonClientMetrics, 0) Then
    Font.Handle := CreateFontIndirect(aNonClientMetrics.lfMessageFont);
End;


Procedure TDialogForm.DoButtonClick(Sender: TObject);
Begin
  FSelected := TButton(Sender);
End;


Function DialogCustom(Const sText : String; aType : TMsgDlgType; Const aButtons : Array Of String; iContext : Integer; Const sCaption : String) : String;

  Function GetAveCharSize(Canvas: TCanvas): TPoint;
  Var
    I: Integer;
    Buffer: Array[0..51] Of Char;
  Begin
    For I := 0 To 25 Do
      Buffer[I] := Chr(I + Ord('A'));

    For I := 0 To 25 Do
      Buffer[I + 26] := Chr(I + Ord('a'));

    GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));

    Result.X := Result.X Div 52;
  End;

ResourceString
  SMsgDlgWarning = 'Warning';
  SMsgDlgError = 'Error';
  SMsgDlgInformation = 'Information';
  SMsgDlgConfirm = 'Confirm';

Const
  DIALOG_CAPTIONS : Array[TMsgDlgType] Of Pointer = (@SMsgDlgWarning, @SMsgDlgError, @SMsgDlgInformation, @SMsgDlgConfirm, Nil);
  ICON_IDs : Array[TMsgDlgType] Of PChar = (IDI_EXCLAMATION, IDI_HAND, IDI_ASTERISK, IDI_QUESTION, Nil);

Const
  mcHorzMargin = 8;
  mcVertMargin = 8;
  mcHorzSpacing = 10;
  mcVertSpacing = 10;
  mcButtonWidth = 50;
  mcButtonHeight = 14;
  mcButtonSpacing = 4;
Var
  oDialog : TDialogForm;
  oButton : TButton;
  oImage : TImage;
  oLabel : TLabel;
  aDialogUnits: TPoint;
  iHorzMargin, iVertMargin, iHorzSpacing, iVertSpacing : Integer;
  iButtonWidth, iButtonHeight, iButtonSpacing, iButtonCount, iButtonGroupWidth : Integer;
  iX, iLeft : Integer;
  iIconTextWidth, iIconTextHeight : Integer;
  iLoop : Integer;
  pIconID: PChar;
  aTextRect : TRect;
  aButtonWidths : Array Of Integer;  // initialized to zero
Begin 
  oDialog := TDialogForm.CreateNew(Application);

  If Assigned(Application) Then
    oDialog.BiDiMode := Application.BiDiMode;

  oDialog.BorderStyle := bsDialog;
  oDialog.Canvas.Font := oDialog.Font;

  aDialogUnits := GetAveCharSize(oDialog.Canvas);
  iHorzMargin := MulDiv(mcHorzMargin, aDialogUnits.X, 4);
  iVertMargin := MulDiv(mcVertMargin, aDialogUnits.Y, 8);
  iHorzSpacing := MulDiv(mcHorzSpacing, aDialogUnits.X, 4);
  iVertSpacing := MulDiv(mcVertSpacing, aDialogUnits.Y, 8);
  iButtonWidth := MulDiv(mcButtonWidth, aDialogUnits.X, 4);

  SetLength(aButtonWidths, Length(aButtons));

  For iLoop := Low(aButtons) To High(aButtons) Do
  Begin
    aTextRect := wp_graphics.Rect(0, 0, 0, 0);
    Windows.DrawText(oDialog.Canvas.Handle, PChar(aButtons[iLoop]), -1, aTextRect, DT_CALCRECT Or DT_LEFT Or DT_SINGLELINE Or oDialog.DrawTextBiDiModeFlagsReadingOnly);
    aButtonWidths[iLoop] := aTextRect.Right - aTextRect.Left + 8;

    // Calculate max button width.
    If aButtonWidths[iLoop] > iButtonWidth Then
      iButtonWidth := aButtonWidths[iLoop];
  End;

  iButtonHeight := MulDiv(mcButtonHeight, aDialogUnits.Y, 8);
  iButtonSpacing := MulDiv(mcButtonSpacing, aDialogUnits.X, 4);
  SetRect(aTextRect, 0, 0, Screen.Width Div 2, 0);
  DrawText(oDialog.Canvas.Handle, PChar(sText), Length(sText) + 1, aTextRect, DT_EXPANDTABS Or DT_CALCRECT Or DT_WORDBREAK Or oDialog.DrawTextBiDiModeFlagsReadingOnly);

  pIconID := ICON_IDS[aType];
  iIconTextWidth := aTextRect.Right;
  iIconTextHeight := aTextRect.Bottom;

  If Assigned(pIconID) Then
  Begin
    Inc(iIconTextWidth, 32 + iHorzSpacing);
    If iIconTextHeight < 32 Then
      iIconTextHeight := 32;
  End;

  iButtonCount := Length(aButtons);

  iButtonGroupWidth := 0;
  If iButtonCount <> 0 Then
    iButtonGroupWidth := (iButtonWidth * iButtonCount) + (iButtonSpacing * (iButtonCount - 1));

  oDialog.ClientWidth := IntegerMax(iIconTextWidth, iButtonGroupWidth) + (iHorzMargin * 2);
  oDialog.ClientHeight := iIconTextHeight + iButtonHeight + iVertSpacing + (iVertMargin * 2);

  oDialog.Left := (Screen.Width Div 2) - (oDialog.Width Div 2);
  oDialog.Top := (Screen.Height Div 2) - (oDialog.Height Div 2);

  If sCaption <> '' Then
    oDialog.Caption := sCaption
  Else If aType <> mtCustom Then
    oDialog.Caption := LoadResString(DIALOG_CAPTIONS[aType])
  Else If Assigned(Application) Then
    oDialog.Caption := Application.Title
  Else
    oDialog.Caption := '';

  If pIconID <> Nil Then
  Begin
    oImage := TImage.Create(oDialog);
    oImage.Name := 'Image';
    oImage.Parent := oDialog;
    oImage.Picture.Icon.Handle := LoadIcon(0, pIconID);
    oImage.SetBounds(iHorzMargin, iVertMargin, 32, 32);
  End;

  oLabel := TLabel.Create(oDialog);
  oLabel.Name := 'Message';
  oLabel.Parent := oDialog;
  oLabel.WordWrap := True;
  oLabel.Caption := sText;
  oLabel.BoundsRect := aTextRect;
  oLabel.BiDiMode := oDialog.BiDiMode;

  iLeft := iIconTextWidth - aTextRect.Right + iHorzMargin;
  If oLabel.UseRightToLeftAlignment Then
    iLeft := oDialog.ClientWidth - iLeft - oLabel.Width;

  oLabel.SetBounds(iLeft, iVertMargin, aTextRect.Right, aTextRect.Bottom);

  iX := (oDialog.ClientWidth - iButtonGroupWidth) Div 2;

  For iLoop := Low(aButtons) To High(aButtons) Do
  Begin
    oButton := TButton.Create(oDialog);
    oButton.Parent := oDialog;
    oButton.Caption := aButtons[iLoop];
    oButton.OnClick := oDialog.DoButtonClick;
    oButton.ModalResult := mrOk;

    oButton.SetBounds(iX, iIconTextHeight + iVertMargin + iVertSpacing, iButtonWidth, iButtonHeight);

    Inc(iX, iButtonWidth + iButtonSpacing);
  End;

  oDialog.HelpContext := iContext;
  oDialog.Position := poScreenCenter;

  oDialog.ShowModal;

  If Assigned(oDialog.Selected) Then
    Result := oDialog.Selected.Caption
  Else
    Result := '';
End;  


Function DialogCustomWarning(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Begin 
  Result := DialogCustom(sText, mtWarning, aButtons, iContext, sCaption);
End;  


Function DialogCustomInformation(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Begin 
  Result := DialogCustom(sText, mtInformation, aButtons, iContext, sCaption);
End;  


Function DialogCustomConfirmation(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Begin
  Result := DialogCustom(sText, mtConfirmation, aButtons, iContext, sCaption);
End;


Function DialogCustomError(Const sText : String; Const aButtons : Array Of String; iContext : Integer = 0; Const sCaption : String = '') : String;
Begin
  Result := DialogCustom(sText, mtError, aButtons, iContext, sCaption);
End;

Const
  VK_0 = Ord('0');
  VK_1 = Ord('1');
  VK_2 = Ord('2');
  VK_3 = Ord('3');
  VK_4 = Ord('4');
  VK_5 = Ord('5');
  VK_6 = Ord('6');
  VK_7 = Ord('7');
  VK_8 = Ord('8');
  VK_9 = Ord('9');
  VK_A = Ord('A');
  VK_B = Ord('B');
  VK_C = Ord('C');
  VK_D = Ord('D');
  VK_E = Ord('E');
  VK_F = Ord('F');
  VK_G = Ord('G');
  VK_H = Ord('H');
  VK_I = Ord('I');
  VK_J = Ord('J');
  VK_K = Ord('K');
  VK_L = Ord('L');
  VK_M = Ord('M');
  VK_N = Ord('N');
  VK_O = Ord('O');
  VK_P = Ord('P');
  VK_Q = Ord('Q');
  VK_R = Ord('R');
  VK_S = Ord('S');
  VK_T = Ord('T');
  VK_U = Ord('U');
  VK_V = Ord('V');
  VK_W = Ord('W');
  VK_X = Ord('X');
  VK_Y = Ord('Y');
  VK_Z = Ord('Z');
  VK_RIGHT_MOUSE = 250 Shl 8;


Function WordToUixKey(AWord: Word): TUixKey;
Begin
  If AWord < 256 Then
    Result := TUixKey(AWord)
  Else
  Begin
    Case AWord Of
      146*256 : Result := kk_Ctrl_0;
      117*256 : Result := kk_Ctrl_1;
      145*256 : Result := kk_Ctrl_2;
      118*256 : Result := kk_Ctrl_3;
      115*256 : Result := kk_Ctrl_4;
      143*256 : Result := kk_Ctrl_5;
      116*256 : Result := kk_Ctrl_6;
      119*256 : Result := kk_Ctrl_7;
      141*256 : Result := kk_Ctrl_8;
      132*256 : Result := kk_Ctrl_9;

      200*256:     Result := kk_CTRL_I;
      82*256:      Result := kk_INS;
      83*256:      Result := kk_DELETE;
      71*256:      Result := kk_HOME;
      79*256:      Result := kk_END;
      73*256:      Result := kk_PAGE_UP;
      81*256:      Result := kk_PAGE_DOWN;
      72*256:      Result := kk_UP;
      80*256:      Result := kk_DOWN;
      75*256:      Result := kk_LEFT;
      77*256:      Result := kk_RIGHT;
      251*256:     Result := kk_ALT_HOME;
      159*256:     Result := kk_ALT_END;
      155*256:     Result := kk_ALT_LEFT;
      157*256:     Result := kk_ALT_RIGHT;

      59*256:      Result := kk_F1;
      60*256:      Result := kk_F2;
      61*256:      Result := kk_F3;
      62*256:      Result := kk_F4;
      63*256:      Result := kk_F5;
      64*256:      Result := kk_F6;
      65*256:      Result := kk_F7;
      66*256:      Result := kk_F8;
      67*256:      Result := kk_F9;
      68*256:      Result := kk_F10;
      133*256:     Result := kk_F11;
      134*256:     Result := kk_F12;

      94*256:      Result := kk_CTRL_F1;
      95*256:      Result := kk_CTRL_F2;
      96*256:      Result := kk_CTRL_F3;
      97*256:      Result := kk_CTRL_F4;
      98*256:      Result := kk_CTRL_F5;
      99*256:      Result := kk_CTRL_F6;
      100*256:     Result := kk_CTRL_F7;
      101*256:     Result := kk_CTRL_F8;
      102*256:     Result := kk_CTRL_F9;
      103*256:     Result := kk_CTRL_F10;
      137*256:     Result := kk_CTRL_F11;
      138*256:     Result := kk_CTRL_F12;

      104*256:     Result := kk_ALT_F1;
      105*256:     Result := kk_ALT_F2;
      106*256:     Result := kk_ALT_F3;
      107*256:     Result := kk_ALT_F4;
      108*256:     Result := kk_ALT_F5;
      109*256:     Result := kk_ALT_F6;
      110*256:     Result := kk_ALT_F7;
      111*256:     Result := kk_ALT_F8;
      112*256:     Result := kk_ALT_F9;
      113*256:     Result := kk_ALT_F10;
      139*256:     Result := kk_ALT_F11;
      140*256:     Result := kk_ALT_F12;

      84*256:      Result := kk_SHIFT_F1;
      85*256:      Result := kk_SHIFT_F2;
      86*256:      Result := kk_SHIFT_F3;
      87*256:      Result := kk_SHIFT_F4;
      88*256:      Result := kk_SHIFT_F5;
      89*256:      Result := kk_SHIFT_F6;
      90*256:      Result := kk_SHIFT_F7;
      91*256:      Result := kk_SHIFT_F8;
      92*256:      Result := kk_SHIFT_F9;
      93*256:      Result := kk_SHIFT_F10;
      135*256:     Result := kk_SHIFT_F11;
      136*256:     Result := kk_SHIFT_F12;

      129*256:     Result := kk_ALT_0;
      120*256:     Result := kk_ALT_1;
      121*256:     Result := kk_ALT_2;
      122*256:     Result := kk_ALT_3;
      123*256:     Result := kk_ALT_4;
      124*256:     Result := kk_ALT_5;
      125*256:     Result := kk_ALT_6;
      126*256:     Result := kk_ALT_7;
      127*256:     Result := kk_ALT_8;
      128*256:     Result := kk_ALT_9;

      30*256:      Result := kk_ALT_A;
      48*256:      Result := kk_ALT_B;
      46*256:      Result := kk_ALT_C;
      32*256:      Result := kk_ALT_D;
      18*256:      Result := kk_ALT_E;
      33*256:      Result := kk_ALT_F;
      34*256:      Result := kk_ALT_G;
      35*256:      Result := kk_ALT_H;
      23*256:      Result := kk_ALT_I;
      36*256:      Result := kk_ALT_J;
      37*256:      Result := kk_ALT_K;
      38*256:      Result := kk_ALT_L;
      50*256:      Result := kk_ALT_M;
      49*256:      Result := kk_ALT_N;
      24*256:      Result := kk_ALT_O;
      25*256:      Result := kk_ALT_P;
      16*256:      Result := kk_ALT_Q;
      19*256:      Result := kk_ALT_R;
      31*256:      Result := kk_ALT_S;
      20*256:      Result := kk_ALT_T;
      22*256:      Result := kk_ALT_U;
      47*256:      Result := kk_ALT_V;
      17*256:      Result := kk_ALT_W;
      45*256:      Result := kk_ALT_X;
      21*256:      Result := kk_ALT_Y;
      44*256:      Result := kk_ALT_Z;
      201*256:     Result := kk_CTRL_0;
      202*256:     Result := kk_CTRL_1;
      3*256:       Result := kk_CTRL_2;
      203*256:     Result := kk_CTRL_3;
      204*256:     Result := kk_CTRL_4;
      205*256:     Result := kk_CTRL_5;
      206*256:     Result := kk_CTRL_6;
      207*256:     Result := kk_CTRL_7;
      208*256:     Result := kk_CTRL_8;
      209*256:     Result := kk_CTRL_9;
      15*256:      Result := kk_SHIFT_TAB;
      VK_RIGHT_MOUSE: Result := KK_RIGHT_MOUSE;   // derrived from VK_APP
    Else
      Result := kk_NULL;
    End;
  End;
End;


Const
   ScanKeyCount = 130;
   ScanXLate : Array [1..5 * ScanKeyCount] Of Word = (
      VK_LBUTTON,0,0,0,0,     { vkcode,base,ctrl,alt,shift }
      VK_RBUTTON,0,0,0,0,
      VK_CANCEL,0,0,0,0,
      VK_MBUTTON,0,0,0,0,
      VK_BACK,8,127,14*256,8,
      VK_TAB,9,148*256,165*256,15*256,
      VK_CLEAR,0,0,0,0,
      VK_RETURN,13,10,28*256,13,
      VK_SHIFT,0,0,0,0,
      VK_CONTROL,0,0,0,0,
      VK_MENU,0,0,0,0,
      VK_PAUSE,0,0,0,0,
      VK_CAPITAL,0,0,0,0,
      VK_ESCAPE,27,27,1*256,27,
      VK_SPACE,32,65,32,32,
      VK_PRIOR,73*256,132*256,153*256,73*256,
      VK_NEXT,81*256,118*256,161*256,81*256,
      VK_END,79*256,117*256,159*256,79*256,
      VK_HOME,71*256,119*256,251*256,71*256,
      VK_LEFT,75*256,115*256,155*256,75*256,
      VK_UP,72*256,141*256,152*256,72*256,
      VK_RIGHT,77*256,116*256,157*256,77*256,
      VK_DOWN,80*256,145*256,160*256,80*256,
      VK_SELECT,0,0,0,0,
      VK_EXECUTE,0,0,0,0,
      VK_SNAPSHOT,0,0,0,0,
      VK_INSERT,82*256,146*256,162*256,0,
      VK_DELETE,83*256,147*256,163*256,0,
      VK_HELP,0,0,0,0,
      VK_0,ord('0'),201*256,129*256,ord(')'),
      VK_1,ord('1'),202*256,120*256,ord('!'),
      VK_2,ord('2'),3*256,121*256,ord('@'),
      VK_3,ord('3'),203*256,122*256,ord('#'),
      VK_4,ord('4'),204*256,123*256,ord('$'),
      VK_5,ord('5'),205*256,124*256,ord('%'),
      VK_6,ord('6'),206*256,125*256,ord('^'),
      VK_7,ord('7'),207*256,126*256,ord('&'),
      VK_8,ord('8'),208*256,127*256,ord('*'),
      VK_9,ord('9'),209*256,128*256,ord('('),
      VK_A,ord('a'),1,30*256,ord('A'),
      VK_B,ord('b'),2,48*256,ord('B'),
      VK_C,ord('c'),3,46*256,ord('C'),
      VK_D,ord('d'),4,32*256,ord('D'),
      VK_E,ord('e'),5,18*256,ord('E'),
      VK_F,ord('f'),6,33*256,ord('F'),
      VK_G,ord('g'),7,34*256,ord('G'),
      VK_H,ord('h'),8,35*256,ord('H'),
      VK_I,ord('i'),200*256,23*256,ord('I'),
      VK_J,ord('j'),10,36*256,ord('J'),
      VK_K,ord('k'),11,37*256,ord('K'),
      VK_L,ord('l'),12,38*256,ord('L'),
      VK_M,ord('m'),13,50*256,ord('M'),
      VK_N,ord('n'),14,49*256,ord('N'),
      VK_O,ord('o'),15,24*256,ord('O'),
      VK_P,ord('p'),16,25*256,ord('P'),
      VK_Q,ord('q'),17,16*256,ord('Q'),
      VK_R,ord('r'),18,19*256,ord('R'),
      VK_S,ord('s'),19,31*256,ord('S'),
      VK_T,ord('t'),20,20*256,ord('T'),
      VK_U,ord('u'),21,22*256,ord('U'),
      VK_V,ord('v'),22,47*256,ord('V'),
      VK_W,ord('w'),23,17*256,ord('W'),
      VK_X,ord('x'),24,45*256,ord('X'),
      VK_Y,ord('y'),25,21*256,ord('Y'),
      VK_Z,ord('z'),26,44*256,ord('Z'),
      VK_LWIN,0,0,0,0,
      VK_RWIN,0,0,0,0,
      VK_APPS,0,0,0,0,
      VK_NUMPAD0,ord('0'),146*256,0,82*256,
      VK_NUMPAD1,ord('1'),117*256,0,79*256,
      VK_NUMPAD2,ord('2'),145*256,0,80*256,
      VK_NUMPAD3,ord('3'),118*256,0,81*256,
      VK_NUMPAD4,ord('4'),115*256,0,75*256,
      VK_NUMPAD5,ord('5'),143*256,0,76*256,
      VK_NUMPAD6,ord('6'),116*256,0,77*256,
      VK_NUMPAD7,ord('7'),119*256,0,71*256,
      VK_NUMPAD8,ord('8'),141*256,0,72*256,
      VK_NUMPAD9,ord('9'),132*256,0,73*256,
      VK_MULTIPLY,ord('*'),150*256,55*256,ord('*'),
      VK_ADD,ord('+'),144*256,78*256,ord('+'),
      VK_SEPARATOR,0,0,0,0,
      VK_SUBTRACT,ord('-'),142*256,74*256,ord('-'),
      VK_DECIMAL,ord('.'),147*256,0,83*256,
      VK_DIVIDE,ord('/'),149*256,164*256,ord('/'),
      VK_F1,59*256,94*256,104*256,84*256,
      VK_F2,60*256,95*256,105*256,85*256,
      VK_F3,61*256,96*256,106*256,86*256,
      VK_F4,62*256,97*256,107*256,87*256,
      VK_F5,63*256,98*256,108*256,88*256,
      VK_F6,64*256,99*256,109*256,89*256,
      VK_F7,65*256,100*256,110*256,90*256,
      VK_F8,66*256,101*256,111*256,91*256,
      VK_F9,67*256,102*256,112*256,92*256,
      VK_F10,68*256,103*256,113*256,93*256,
      VK_F11,133*256,137*256,139*256,135*256,
      VK_F12,134*256,138*256,140*256,136*256,
      VK_F13,0,0,0,0,
      VK_F14,0,0,0,0,
      VK_F15,0,0,0,0,
      VK_F16,0,0,0,0,
      VK_F17,0,0,0,0,
      VK_F18,0,0,0,0,
      VK_F19,0,0,0,0,
      vk_F20,0,0,0,0,
      vk_F21,0,0,0,0,
      vk_F22,0,0,0,0,
      vk_F23,0,0,0,0,
      vk_F24,0,0,0,0,
      vk_NUMLOCK,0,0,0,0,
      vk_SCROLL,0,0,0,0,
      186,ord(';'),0,0,ord(':'),
      187,ord('='),0,131*256,ord('+'),
      188,ord(','),0,0,ord('<'),
      189,ord('-'),0,130*256,ord('_'),
      190,ord('.'),0,0,ord('>'),
      191,ord('/'),0,0,ord('?'),
      192,ord('`'),0,0,ord('~'),
      219,ord('['),0,0,ord('{'),
      220,ord('\'),0,0,ord('|'),
      221,ord(']'),0,0,ord('}'),
      222,ord(''''),0,0,ord('"'),
      vk_ATTN,0,0,0,0,
      vk_CRSEL,0,0,0,0,
      vk_EXSEL,0,0,0,0,
      vk_EREOF,0,0,0,0,
      vk_PLAY,0,0,0,0,
      vk_ZOOM,0,0,0,0,
      vk_NONAME,0,0,0,0,
      vk_PA1,0,0,0,0,
      vk_OEM_CLEAR,0,0,0,0
   );

Function ToUixKey(aKey: Word; Const aShift: TShiftState): TUixKey;
Var
  LStates: TKeyboardState;
  Mid,Top,Bot,w: Integer;
  Caps: Boolean;
Begin
  Result := kk_NULL;

  Top := ScanKeyCount;
  Bot := 1;

  // WIN95/WIN98 bug fix for "." vs DEL
  If (AKey = 110) Then
    AKey := 190;

  w := 0;
  Mid := 1;

  // binary search
  While Top >= Bot Do
  Begin
    Mid := (Top + Bot) Div 2;
    w := ScanXLate[((Mid - 1) * 5) + 1];

    If AKey < w Then
      Top := Mid - 1
    Else If AKey > w Then
      Bot := Mid + 1
    Else
      Break;
  End;

  If w = AKey Then
  Begin
    w := ((Mid - 1) * 5) + 1;

    GetKeyboardState(LStates);

    Caps := ((LStates[20] And 1) <> 0) And (AKey In [ord('A')..ord('Z')]);

    If ssCtrl In AShift Then
      w := w + 2
    Else If ssAlt In AShift Then
      w := w + 3
    Else If Caps Xor (ssShift In AShift) Then
      w := w + 4
    Else
      w := w + 1;

    w := ScanXLate[w];

    If w = 0 Then
      Result := kk_NULL
    Else
      Result := WordToUixKey(w);
  End;
End;


Function UixKeyToString(Const aKey: TUixKey): String;
Begin
  Result := UixKeyNameArray[aKey];
End;


Function StringToUixKey(Const sValue : String): TUixKey;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(UixKeyNameArray, sValue);

  If iIndex < 0 Then
    Result := kk_Null
  Else
    Result := TUixKey(iIndex);
End;


Function CharacterToUixKey(Const sValue : Char): TUixKey;
Var
  iIndex : Integer;
Begin
  iIndex := CharArrayIndexOf(UixKEY_CHARACTERS, sValue);

  If iIndex < 0 Then
    Result := kk_Null
  Else
    Result := TUixKey(iIndex);
End;


Function NumberToUixKey(Const iValue : Integer): TUixKey;
Begin
  If (iValue >= 0) And (iValue <= 9) Then
    Result := TUixKey(Integer(kk_0) + iValue)
  Else
    Result := kk_Null;
End;


Function UixKeyToChar(Const aKey: TUixKey): Char;
Begin
  // First 256 elements of TUixKey map onto ASCII Char.
  Result := Char(aKey);
End;


Function UixKeyToShortCut(Const aKey: TUixKey): TShortCut;
Begin
  Result := TextToShortCut(UixKeyToString(aKey));
End;


Function ShortCutToUixKey(Const aShortCut : TShortCut): TUixKey;
Begin
  Result := StringToUixKey(ShortCutToText(aShortCut));
End;


Function UixKeyIsNull(Const aKey : TUixKey) : Boolean;
Begin
  Result := aKey = kk_NULL;
End;


Function UixKeyIsWhitespace(Const aKey : TUixKey) : Boolean;
Begin
  Result := aKey = kk_SPACE;
End;


Function UixKeyIsSign(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey = kk_MINUS) Or (aKey = kk_PLUS);
End;


Function UixKeyIsNumber(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey >= kk_0) And (aKey <= kk_9);
End;


Function UixKeyIsCapital(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey >= kk_A) And (aKey <= kk_Z);
End;


Function UixKeyIsLowercase(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey >= kk_LOWERCASE_A) And (aKey <= kk_LOWERCASE_Z);
End;


Function UixKeyIsVowel(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey = kk_LOWERCASE_A) Or (aKey = kk_A) Or
            (aKey = kk_LOWERCASE_E) Or (aKey = kk_E) Or
            (aKey = kk_LOWERCASE_I) Or (aKey = kk_I) Or
            (aKey = kk_LOWERCASE_O) Or (aKey = kk_O) Or
            (aKey = kk_LOWERCASE_U) Or (aKey = kk_U);
End;


Function UixKeyIsAlphabet(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsCapital(aKey) Or UixKeyIsLowercase(aKey);
End;


Function UixKeyIsConsonant(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsAlphabet(aKey) And (Not UixKeyIsVowel(aKey));
End;


Function UixKeyIsAlphanumeric(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsAlphabet(aKey) Or UixKeyIsNumber(aKey);
End;


Function UixKeyIsCardinal(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsNumber(aKey);
End;


Function UixKeyIsInteger(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsCardinal(aKey) Or UixKeyIsSign(aKey);
End;


Function UixKeyIsReal(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsNumber(aKey) Or (aKey = kk_DOT);
End;


Function UixKeyIsHexadecimal(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsNumber(aKey) Or ((aKey >= kk_A) And (aKey <= kk_F)) Or ((aKey >= kk_LOWERCASE_A) And (aKey <= kk_LOWERCASE_F));
End;


Function UixKeyIsCurrency(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsNumber(aKey) Or (aKey In [kk_DOT, kk_MINUS]);
End;


Function UixKeyIsSpecial(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey = kk_TILDE) Or
            (aKey = kk_INVERTEDQUOTE) Or
            (aKey = kk_EXCLAMATION) Or
            (aKey = kk_AT) Or
            (aKey = kk_HASH) Or
            (aKey = kk_DOLLAR) Or
            (aKey = kk_PERCENT) Or
            (aKey = kk_CARRET) Or
            (aKey = kk_AMPERSAND) Or
            (aKey = kk_ASTERISK) Or
            (aKey = kk_LEFTBRACE) Or
            (aKey = kk_RIGHTBRACE) Or
            (aKey = kk_PLUS) Or
            (aKey = kk_EQUAL) Or
            (aKey = kk_BACKSLASH) Or
            (aKey = kk_QUESTION) Or
            (aKey = kk_VERTICALBAR) Or
            (aKey = kk_LESSTHAN) Or
            (aKey = kk_GREATERTHAN) Or
            (aKey = kk_COMMA) Or
            (aKey = kk_SEMICOLON) Or
            (aKey = kk_COLON) Or
            (aKey = kk_SLASH) Or
            (aKey = kk_DOUBLEQUOTE) Or
            (aKey = kk_DOT) Or
            (aKey = kk_LEFTSQUAREBRACKET) Or
            (aKey = kk_RIGHTSQUAREBRACKET) Or
            (aKey = kk_LEFTBRACKET) Or
            (aKey = kk_RIGHTBRACKET) Or
            (aKey = kk_MINUS) Or
            (aKey = kk_UNDERSCORE) Or
            (aKey = kk_SINGLEQUOTE);
End;


Function UixKeyIsFilename(Const aKey : TUixKey) : Boolean;
Begin
  Result := (UixKeyIsAlphanumeric(aKey) Or UixKeyIsSpecial(aKey) Or (aKey = kk_SPACE)) And
            (
             (aKey <> kk_LESSTHAN) And
             (aKey <> kk_GREATERTHAN) And
             (aKey <> kk_QUESTION) And
             (aKey <> kk_ASTERISK) And
             (aKey <> kk_VERTICALBAR) And
             (aKey <> kk_SLASH) And
             (aKey <> kk_BACKSLASH) And
             (aKey <> kk_COLON)
            );
End;


Function UixKeyIsSystem(Const aKey : TUixKey) : Boolean;
Begin
  Result := (aKey = kk_TAB) Or (aKey = kk_CR) Or (aKey = kk_BS) Or (aKey = kk_DELETE);
End;


Function UixKeyIsKeyboard(Const aKey : TUixKey) : Boolean;
Begin
  Result := UixKeyIsSpecial(aKey) Or UixKeyIsAlphanumeric(aKey);
End;


Function NullKey : TUixKey;
Begin
  Result := kk_NULL;
End;


Function UixKeyUpper(Const aKey : TUixKey) : TUixKey;
Begin
  If UixKeyIsLowercase(aKey) Then
    Result := TUixKey(Ord(aKey) - Ord(kk_LOWERCASE_A) + Ord(kk_A))
  Else
    Result := aKey;
End;


Function UixKeyEquals(Const aA, aB : TUixKey) : Boolean;
Begin
  Result := UixKeyCompare(aA, aB) = 0;
End;


Function UixKeyCompare(Const aA, aB : TUixKey) : Integer;
Begin
  Result := UixKeyCompareInsensitive(aA, aB);
End;


Function UixKeyCompareInsensitive(Const aA, aB : TUixKey) : Integer;
Begin
  Result := UixKeyCompareSensitive(UixKeyUpper(aA), UixKeyUpper(aB));
End;


Function UixKeyCompareSensitive(Const aA, aB : TUixKey) : Integer; Overload;
Begin
  Result := IntegerCompare(Integer(aA), Integer(aB));
End;


Function TUixKeys.Add(Const aKey: TUixKey): Integer;
Begin
  Result := Inherited Add(Integer(aKey));
End;


Function TUixKeys.Exists(Const aKey: TUixKey): Boolean;
Begin
  Result := Inherited ExistsByValue(Integer(aKey));
End;


Function TUixKeys.GetKey(Const iIndex: Integer): TUixKey;
Begin
  Result := TUixKey(Inherited IntegerByIndex[iIndex]);
End;


Procedure TUixKeys.Remove(Const aKey: TUixKey);
Begin
  Inherited DeleteByValue(Integer(aKey));
End;


Procedure TUixKeys.SetKey(Const iIndex: Integer; Const Value: TUixKey);
Begin
  Inherited IntegerByIndex[iIndex] := Integer(Value);
End;


Function UixKeyToNumber(Const aKey : TUixKey) : Integer; Overload;
Begin
  Assert(UixKeyIsNumber(aKey), 'aKey must be a number key.');

  Case aKey Of
    kk_0 : Result := 0;
    kk_1 : Result := 1;
    kk_2 : Result := 2;
    kk_3 : Result := 3;
    kk_4 : Result := 4;
    kk_5 : Result := 5;
    kk_6 : Result := 6;
    kk_7 : Result := 7;
    kk_8 : Result := 8;
    kk_9 : Result := 9;
  Else
    Result := -1;
  End;
End;


Function UixKeyBetween(Const aLeft, aCheck, aRight : TUixKey) : Boolean;
Begin
  Result := (aCheck >= aLeft) And (aCheck <= aRight);
End;


Var
  aUser32 : HMODULE;
  hMsImg32 : THandle;
  aDisableProcessWindowsGhosting : Procedure;
Initialization
  hMsImg32 := LoadLibrary(MSIMG32_DLL);

  GradientFill := GetProcAddress(hMsImg32, PChar('GradientFill'));

  If Not Assigned(GradientFill) Then
    GradientFill := DefaultGradientFill;

  aUser32 := GetModuleHandle('USER32.DLL');

  If aUser32 <> 0 Then
  Begin
    @aDisableProcessWindowsGhosting := GetProcAddress(aUser32, 'DisableProcessWindowsGhosting');

    If Assigned(@aDisableProcessWindowsGhosting) Then
      aDisableProcessWindowsGhosting;
  End;

Finalization
  If hMsImg32 <> 0 Then
    FreeLibrary(hMsImg32);
End.

