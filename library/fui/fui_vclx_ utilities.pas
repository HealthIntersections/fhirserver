Unit fui_vclx_Support;


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
{$IFNDEF VER130}
  System.UITypes,
{$ENDIF}
  Windows, Graphics, Math, StdCtrls, ExtCtrls, Dialogs, SysUtils, Controls, Forms, Classes, Menus,
  MathSupport, ColourSupport, CoordinateSupport, RectSupport;


Type
  TRect = RectSupport.TRect;
  TPoint = CoordinateSupport.TPoint;
  TColour = ColourSupport.TColour;
  THakIdentifier = Integer;
  THakEvent = Procedure (oSender : TObject) Of Object;


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
    aTextRect := RectSupport.Rect(0, 0, 0, 0);
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

