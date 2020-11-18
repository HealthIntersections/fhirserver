Unit fui_vclx_controls;

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
  Windows, SysUtils, Classes, Graphics, Controls, StdCtrls, Contnrs, ExtCtrls, Messages, Forms, Dialogs, Character,
  System.UITypes, UxTheme, ToolWin, ComCtrls, Menus, ImgList, Mask, CommCtrl, VirtualTrees, ActiveX,
  Winapi.GdipApi, WinApi.GdipObj,
  SynEdit, SynEditKeyCmds, SynEditHighlighter, SynEditTypes,
  fsl_base, fsl_utilities, fsl_stream, fsl_collections, fsl_threads,
  fui_vclx_base, fui_vclx_images,
  wp_graphics, wp_gdiplus;

  {$IFNDEF VER130}  {$ENDIF}


Type
  TUixControl = Class(TCustomControl)
    Protected
      Procedure Error(Const sMethod : String; Const sMessage : String);
      Function Condition(Const bTruth : Boolean; Const sMethod : String; Const sMessage : String) : Boolean;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure ShuffleTop;
      Procedure ShuffleBottom;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Property ParentColor;
  End;

  TComponent = Classes.TComponent;

  TUixComponentList = Class(TComponentList)
    Protected
      Function ItemClass : TComponentClass; Overload; Virtual;

      Function Invariant(Const sMethod, sMessage: String): Boolean; Overload;
      Function Invariants(Const sLocation: String; oObject: TObject; aClass: TClass; Const sObject: String): Boolean; Overload;

      Procedure Notify(Ptr: Pointer; Action: TListNotification); Override;
  End;

  TComponentClass = Classes.TComponentClass;


  TUixLabel = Class(TLabel)
    Private
      FLeftBorderWidth : Integer;
      FRightBorderWidth : Integer;
      FTopBorderWidth : Integer;
      FBottomBorderWidth : Integer;

      FAdjustBoundsOnResize : Boolean;

      Function GetBolded : Boolean;
      Procedure SetBolded(Const Value : Boolean);

      Function GetItalicised : Boolean;
      Procedure SetItalicised(Const Value : Boolean);

      Function GetUnderlined : Boolean;
      Procedure SetUnderlined(Const Value : Boolean);

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetBorderWidth : Integer;
      Procedure SetBorderWidth(Const Value : Integer);

      Function GetLeftBorderWidth : Integer;
      Procedure SetLeftBorderWidth(Const Value : Integer);

      Function GetRightBorderWidth : Integer;
      Procedure SetRightBorderWidth(Const Value : Integer);

      Function GetTopBorderWidth : Integer;
      Procedure SetTopBorderWidth(Const Value : Integer);

      Function GetBottomBorderWidth : Integer;
      Procedure SetBottomBorderWidth(Const Value : Integer);

    Protected
      Procedure AdjustBounds; Override;

      Procedure Paint; Override;

      Procedure Resize; Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Adjust;

      Procedure VerticalAlignCenter;
      Procedure VerticalAlignTop;
      Procedure VerticalAlignBottom;

      Procedure HorizontalAlignCenter;
      Procedure HorizontalAlignLeft;
      Procedure HorizontalAlignRight;

      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignClient;

      Procedure ShuffleTop;
      Procedure ShuffleBottom;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Function RequiredHeight : Integer;

      Property BorderWidth : Integer Read GetBorderWidth Write SetBorderWidth;
      Property LeftBorderWidth : Integer Read GetLeftBorderWidth Write SetLeftBorderWidth;
      Property RightBorderWidth : Integer Read GetRightBorderWidth Write SetRightBorderWidth;
      Property TopBorderWidth : Integer Read GetTopBorderWidth Write SetTopBorderWidth;
      Property BottomBorderWidth : Integer Read GetBottomBorderWidth Write SetBottomBorderWidth;

      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;

      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property Italicised : Boolean Read GetItalicised Write SetItalicised;
      Property Underlined : Boolean Read GetUnderlined Write SetUnderlined;
      Property AdjustBoundsOnResize : Boolean Read FAdjustBoundsOnResize Write FAdjustBoundsOnResize;
      Property OnResize;
  End;

  TUixLabels = Class(TComponentList)
    Private
      Function GetLabel(iIndex: Integer): TUixLabel;

    Public
      Property Labels[iIndex : Integer] : TUixLabel Read GetLabel; Default;
  End;

  TUixLabelClass = Class Of TUixLabel;


Type
  TUixPanelHoverEnterDelegate = Procedure(oSender : TObject; Const iX : Integer; Const iY : Integer) Of Object;
  TUixPanelHoverExitDelegate = TNotifyEvent;

  TUixPanelGradientType = (UixPanelGradientTypeNone, UixPanelGradientTypeVertical, UixPanelGradientTypeHorizontal);

  TUixPanel = Class(TPanel)
    Private
      FOnPaint : TNotifyEvent;
      FOnHoverEnter : TUixPanelHoverEnterDelegate;
      FOnHoverExit : TUixPanelHoverExitDelegate;

      FGradientType : TUixPanelGradientType;
      FGradientColour : TColour;
      FGradientDrawWhiteEdges : Boolean;

      FTransparent : Boolean;

      FRoundedEdges : Boolean;

      Function GetBolded : Boolean;
      Procedure SetBolded(Const Value : Boolean);

      Function GetAnchoredBottom : Boolean;
      Procedure SetAnchoredBottom(Const Value : Boolean);

      Function GetAnchoredLeft : Boolean;
      Procedure SetAnchoredLeft(Const Value : Boolean);

      Function GetAnchoredRight : Boolean;
      Procedure SetAnchoredRight(Const Value : Boolean);

      Function GetAnchoredTop : Boolean;
      Procedure SetAnchoredTop(Const Value : Boolean);

      Function GetTransparent : Boolean;
      Procedure SetTransparent(Const Value : Boolean);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure CreateParams(Var aParams : TCreateParams); Override;
      Procedure CMMouseEnter(Var aMessage : TMessage); Message CM_MouseEnter;
      Procedure CMMouseLeave(Var aMessage : TMessage); Message CM_MouseLeave;
      Procedure WMEraseBkgnd(Var aMessage : TWmEraseBkgnd); Message WM_ERASEBKGND;

      Procedure Paint; Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure Adjust;
      Procedure Reset;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Function IsAlignedBottom : Boolean;
      Function IsAlignedClient : Boolean;
      Function IsAlignedLeft : Boolean;
      Function IsAlignedRight : Boolean;
      Function IsAlignedTop : Boolean;

      Procedure HorizontalAlignCenter;
      Procedure HorizontalAlignLeft;
      Procedure HorizontalAlignRight;

      Procedure BevelInnerNone;
      Procedure BevelInnerLowered;
      Procedure BevelInnerRaised;

      Function IsBevelInnerNone : Boolean;
      Function IsBevelInnerLowered : Boolean;
      Function IsBevelInnerRaised : Boolean;

      Procedure BevelOuterNone;
      Procedure BevelOuterLowered;
      Procedure BevelOuterRaised;

      Function IsBevelOuterNone : Boolean;
      Function IsBevelOuterLowered : Boolean;
      Function IsBevelOuterRaised : Boolean;

      Procedure BorderNone;
      Procedure BorderSingle;

      Function IsBorderNone : Boolean;
      Function IsBorderSingle : Boolean;

      Procedure GradientTypeNone;
      Procedure GradientTypeVertical;
      Procedure GradientTypeHorizontal;

      Procedure ShuffleBottom;
      Procedure ShuffleTop;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Function RequiredHeight : Integer;
      Function NonClientHeight : Integer;
      Function VisibleControlHeight : Integer;

      Property GradientType : TUixPanelGradientType Read FGradientType Write FGradientType;
      Property GradientColour : TColour Read FGradientColour Write FGradientColour;
      Property GradientDrawWhiteEdges : Boolean Read FGradientDrawWhiteEdges Write FGradientDrawWhiteEdges;
      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property Transparent : Boolean Read GetTransparent Write SetTransparent;
      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;
      Property RoundedEdges : Boolean Read FRoundedEdges Write FRoundedEdges;

    Published
      Property Canvas;
      Property DoubleBuffered;
      Property OnKeyDown;
      Property OnKeyPress;
      Property OnKeyUp;
      Property OnEnter;
      Property OnExit;
      Property OnPaint : TNotifyEvent Read FOnPaint Write FOnPaint;
      Property OnHoverEnter : TUixPanelHoverEnterDelegate Read FOnHoverEnter Write FOnHoverEnter;
      Property OnHoverExit : TUixPanelHoverExitDelegate Read FOnHoverExit Write FOnHoverExit;
  End;

  TUixPanelClass = Class Of TUixPanel;

  TUixPanelList = Class(TUixComponentList)
    Private
      Function GetPanelByIndex(Const iIndex : Integer) : TUixPanel;

    Protected
      Function ItemClass : TComponentClass; Overload; Override;

    Public
      Property PanelByIndex[Const iIndex : Integer] : TUixPanel Read GetPanelByIndex; Default;
  End;

  TUixButton = Class(TButton)
    Private
      FTitle : String;

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetBolded : Boolean;
      Procedure SetBolded(Const Value : Boolean);

    Protected
      Procedure Initialise;
      Procedure Finalise;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure ModalNone;
      Procedure ModalOK;
      Procedure ModalCancel;
      Procedure ModalAbort;
      Procedure ModalRetry;
      Procedure ModalIgnore;
      Procedure ModalYes;
      Procedure ModalNo;

      Function IsModalNone : Boolean;
      Function IsModalOK : Boolean;
      Function IsModalCancel : Boolean;
      Function IsModalAbort : Boolean;
      Function IsModalRetry : Boolean;
      Function IsModalIgnore : Boolean;
      Function IsModalYes : Boolean;
      Function IsModalNo : Boolean;

      Procedure AlignLeft;
      Procedure AlignClient;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignRight;

      Procedure ShuffleLeft;
      Procedure ShuffleTop;
      Procedure ShuffleBottom;
      Procedure ShuffleRight;

      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;

      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property Title : String Read FTitle Write FTitle;
  End;


  TUixTimer = Class(TTimer)
    Private
      FThreadAffinity : TThreadID;

    Protected
      Procedure Timer; Override;

    Public
      constructor Create(AOwner: TComponent); Override;

      Procedure Reset;
      Procedure Start;
      Procedure Stop;
  End;


  TUixButtons = Class(TComponentList)
    Private
      Function GetButton(iIndex: Integer): TUixButton;

    Public
      Property Buttons[iIndex : Integer] : TUixButton Read GetButton; Default;
  End;

  TNotifyEvent = Classes.TNotifyEvent;

  TUixBalloonType = (UixBalloonTypeHelp, UixBalloonTypeInformation, UixBalloonTypeError, UixBalloonTypeWarning);
  TUixBalloonRenderStyle = (UixBalloonRenderStyleMessage, UixBalloonRenderStyleCustom);
  TUixBalloonAnchorPosition = (UixBalloonAnchorPositionTopLeft, UixBalloonAnchorPositionTopRight, UixBalloonAnchorPositionBottomLeft, UixBalloonAnchorPositionBottomRight);
  TUixBalloonControlPositionStyle = (UixBalloonControlPositionStyleRelative, UixBalloonControlPositionStyleAbsolute);
  TUixBalloonControlRelativeHorizontalPosition = (UixBalloonControlRelativeHorizontalPositionLeft, UixBalloonControlRelativeHorizontalPositionCenter, UixBalloonControlRelativeHorizontalPositionRight);
  TUixBalloonControlRelativeVerticalPosition = (UixBalloonControlRelativeVerticalPositionTop, UixBalloonControlRelativeVerticalPositionCenter, UixBalloonControlRelativeVerticalPositionBottom);

  TUixBalloonClass = Class Of TUixBalloon;

  TUixBalloon = Class(TCustomForm)
    Private
      FHeaderImage : TUixImage;
      FHeaderLabel : TLabel;

      FClientPanel : TPanel;
      FMessageLabel : TLabel;

      FShowTimer : TUixTimer;

      FBalloonType : TUixBalloonType;
      FAnchorPosition : TUixBalloonAnchorPosition;
      FAnchorCoordinate : TPoint;
      FUseAudibleNotification : Boolean;

      FRenderStyle : TUixBalloonRenderStyle;

      FHasAbsoluteX : Boolean;
      FAbsoluteX : Integer;
      FHasAbsoluteY : Boolean;
      FAbsoluteY : Integer;

      FControl : TWinControl;
      FControlPositionStyle : TUixBalloonControlPositionStyle;
      FControlRelativeHorizontalPosition : TUixBalloonControlRelativeHorizontalPosition;
      FControlRelativeVerticalPosition : TUixBalloonControlRelativeVerticalPosition;
      FHasControlAbsoluteX : Boolean;
      FControlAbsoluteX : Integer;
      FHasControlAbsoluteY : Boolean;
      FControlAbsoluteY : Integer;

      Function GetTitle : String;
      Procedure SetTitle(Const Value : String);

      Function GetTimeoutDuration : Integer;
      Procedure SetTimeoutDuration(Const Value : Integer);

      Function GetMessage : String;
      Procedure SetMessage(Const Value : String);

      Procedure PaintDelegate(oSender : TObject);
      Procedure ReleaseDelegate(oSender : TObject);
      Procedure ClickDelegate(oSender : TObject);

    Protected
      Procedure CreateParams(Var Params : TCreateParams); Override;
      Procedure DoClose(Var Action : TCloseAction); Override;
      Procedure WndProc(Var aMessage : TMessage); Override;

    Public
      constructor CreateNew(oOwner : TComponent; iDummy : Integer = 0); Override;

      Procedure ShowBalloon;
      Procedure ReleaseBalloon;

      Procedure BalloonTypeHelp;
      Procedure BalloonTypeInformation;
      Procedure BalloonTypeError;
      Procedure BalloonTypeWarning;

      Procedure RenderStyleMessage;
      Procedure RenderStyleCustom;

      Procedure AnchorPositionTopLeft;
      Procedure AnchorPositionTopRight;
      Procedure AnchorPositionBottomLeft;
      Procedure AnchorPositionBottomRight;

      Procedure ControlPositionStyleRelative;
      Procedure ControlPositionStyleAbsolute;

      Procedure ControlRelativeHorizontalPositionLeft;
      Procedure ControlRelativeHorizontalPositionCenter;
      Procedure ControlRelativeHorizontalPositionRight;

      Procedure ControlRelativeVerticalPositionTop;
      Procedure ControlRelativeVerticalPositionCenter;
      Procedure ControlRelativeVerticalPositionBottom;

      Property Title : String Read GetTitle Write SetTitle;
      Property BalloonType : TUixBalloonType Read FBalloonType Write FBalloonType;
      Property AnchorPosition : TUixBalloonAnchorPosition Read FAnchorPosition Write FAnchorPosition;
      Property TimeoutDuration : Integer Read GetTimeoutDuration Write SetTimeoutDuration;
      Property UseAudibleNotification : Boolean Read FUseAudibleNotification Write FUseAudibleNotification;

      Property RenderStyle : TUixBalloonRenderStyle Read FRenderStyle Write FRenderStyle;
      Property Message : String Read GetMessage Write SetMessage;
      Property ClientPanel : TPanel Read FClientPanel;

      Property HasAbsoluteX : Boolean Read FHasAbsoluteX Write FHasAbsoluteX;
      Property AbsoluteX : Integer Read FAbsoluteX Write FAbsoluteX;
      Property HasAbsoluteY : Boolean Read FHasAbsoluteY Write FHasAbsoluteY;
      Property AbsoluteY : Integer Read FAbsoluteY Write FAbsoluteY;

      Property Control : TWinControl Read FControl Write FControl;
      Property ControlPositionStyle : TUixBalloonControlPositionStyle Read FControlPositionStyle Write FControlPositionStyle;
      Property ControlRelativeHorizontalPosition : TUixBalloonControlRelativeHorizontalPosition Read FControlRelativeHorizontalPosition Write FControlRelativeHorizontalPosition;
      Property ControlRelativeVerticalPosition : TUixBalloonControlRelativeVerticalPosition Read FControlRelativeVerticalPosition Write FControlRelativeVerticalPosition;
      Property HasControlAbsoluteX : Boolean Read FHasControlAbsoluteX Write FHasControlAbsoluteX;
      Property ControlAbsoluteX : Integer Read FControlAbsoluteX Write FControlAbsoluteX;
      Property HasControlAbsoluteY : Boolean Read FHasControlAbsoluteY Write FHasControlAbsoluteY;
      Property ControlAbsoluteY : Integer Read FControlAbsoluteY Write FControlAbsoluteY;
  End;

Type
  TUixCodeToken = (UixCodeTokenText, UixCodeTokenFixed, UixCodeTokenRequired, UixCodeTokenRemainder, UixCodeTokenInvalid, UixCodeTokenCustom,
    UixCodeTokenBeyond, UixCodeTokenUnknown);

  TUixCodeMask = Class(TFslCodeMask)
    Public
      Function Link : TUixCodeMask; Overload;
  End;

  TUixCodeHighlighterAttributes = TSynHighlighterAttributes;

  TUixCodeHighlighterCustomEvent = Procedure (oSender : TObject; Const iTextIndex : Integer; Var sValue : String; oAttributes : TUixCodeHighlighterAttributes) Of Object;

{$IFDEF VER130}
  TUixCodeHighlighter = Class(TSynCustomHighlighter)
    Private
      FText : String;
      FIndex : Integer;
      FMask : TUixCodeMask;
      FValue : String;
      FToken : TUixCodeToken;

      FMaskAttributes : TSynHighlighterAttributes;
      FTextAttributes : TSynHighlighterAttributes;
      FBeyondAttributes : TSynHighlighterAttributes;
      FInvalidAttributes : TSynHighlighterAttributes;
      FRemainderAttributes : TSynHighlighterAttributes;
      FRequiredAttributes : TSynHighlighterAttributes;
      FCustomAttributes : TSynHighlighterAttributes;

      FOnCustom : TUixCodeHighlighterCustomEvent;

      Procedure SetMask(Const Value: TUixCodeMask);

      Function GetIndex: Integer;
      Procedure SetIndex(const Value: Integer);

    Protected
      Procedure Scan;

      Function CurrentToken : TUixCodeToken;

      Property Index : Integer Read GetIndex Write SetIndex; // Provides access to the underlying Run property

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Class Function GetLanguageName : String; Override;

      Function GetRange : Pointer; Override;
      Procedure SetRange(Value : Pointer); Override;

      Procedure ResetRange; Override;

      Function GetDefaultAttribute(Index : Integer) : TSynHighlighterAttributes; Override;
      Function GetEol : Boolean; Override;
      Function GetToken : String; Override;
      Function GetTokenAttribute : TSynHighlighterAttributes; Override;
      Function GetTokenKind : Integer; Override;
      Function GetTokenPos : Integer; Override;

      Procedure SetLine(NewValue: String; LineNumber: Integer); Override;
      Procedure Next; Override;

      Property Mask : TUixCodeMask Read FMask Write SetMask;

    Published
      Property MaskAttributes : TSynHighlighterAttributes Read FMaskAttributes;
      Property TextAttributes : TSynHighlighterAttributes Read FTextAttributes;
      Property InvalidAttributes : TSynHighlighterAttributes Read FInvalidAttributes;
      Property RemainderAttributes : TSynHighlighterAttributes Read FRemainderAttributes;
      Property RequiredAttributes : TSynHighlighterAttributes Read FRequiredAttributes;
      Property BeyondAttributes : TSynHighlighterAttributes Read FBeyondAttributes;
      Property CustomAttributes : TSynHighlighterAttributes Read FCustomAttributes;
      Property OnCustom : TUixCodeHighlighterCustomEvent Read FOnCustom Write FOnCustom;
  End;
{$ELSE}
  TUixUnicodeCodeHighlighter = Class(TSynCustomHighlighter)
    Private
      FUnicodeMask : TUixCodeMask;
      FValue : String;
      FToken : TUixCodeToken;

      FMaskAttributes : TSynHighlighterAttributes;
      FTextAttributes : TSynHighlighterAttributes;
      FBeyondAttributes : TSynHighlighterAttributes;
      FInvalidAttributes : TSynHighlighterAttributes;
      FRemainderAttributes : TSynHighlighterAttributes;
      FRequiredAttributes : TSynHighlighterAttributes;
      FCustomAttributes : TSynHighlighterAttributes;

      FOnCustom : TUixCodeHighlighterCustomEvent;

      Function GetText: String;
      procedure SetText(const Value: String);

      Function GetIndex: Integer;
      Procedure SetIndex(Const Value: Integer);

      Function GetMask: TUixCodeMask;
      Procedure SetMask(const Value: TUixCodeMask);

    Protected
      Function RealLineLength : Integer;
      Function CurrentToken : TUixCodeToken;

      Property Index : Integer Read GetIndex Write SetIndex; // Provides access to the underlying Run private member
      Property Text : String Read GetText Write SetText; // Provides access to the underlying fLine private member

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Next; Override;
      Function GetEol: Boolean; Override;
      Function GetToken : String; Override;
      Function GetTokenAttribute : TSynHighlighterAttributes; Override;
      Function GetTokenKind : Integer; Override;
      function GetRange: Pointer; Override;
      procedure SetRange(Value: Pointer); Override;
      procedure ResetRange; Override;
      Function GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes; Override;

      Property Mask : TUixCodeMask Read GetMask Write SetMask;

    Published
      Property MaskAttributes : TSynHighlighterAttributes Read FMaskAttributes;
      Property TextAttributes : TSynHighlighterAttributes Read FTextAttributes;
      Property InvalidAttributes : TSynHighlighterAttributes Read FInvalidAttributes;
      Property RemainderAttributes : TSynHighlighterAttributes Read FRemainderAttributes;
      Property RequiredAttributes : TSynHighlighterAttributes Read FRequiredAttributes;
      Property BeyondAttributes : TSynHighlighterAttributes Read FBeyondAttributes;
      Property CustomAttributes : TSynHighlighterAttributes Read FCustomAttributes;
      Property OnCustom : TUixCodeHighlighterCustomEvent Read FOnCustom Write FOnCustom;
  End;
{$ENDIF}

  TUixCodeEditValidationMode = (UixCodeEditValidationModeString, UixCodeEditValidationModeDateTime, UixCodeEditValidationModeDuration,
    UixCodeEditValidationModeCurrency, UixCodeEditValidationModeInteger, UixCodeEditValidationModeExtended);

  TUixCodeEditChangeDelegate = TNotifyEvent;
  TUixCodeEditDoubleClickDelegate = TNotifyEvent;
  TUixCodeEditKeyDownDelegate = TKeyEvent;
  TUixCodeEditKeyUpDelegate = TKeyEvent;
  TUixCodeEditLoseFocusDelegate = TNotifyEvent;
  TUixCodeEditReceiveFocusDelegate = TNotifyEvent;
  TUixCodeEditRepositionDelegate = TNotifyEvent;
  TUixCodeEditStandardKeyPressDelegate = Procedure (oSender : TObject; Const cChar : Char) Of Object;
  TUixCodeEditValidateContentDelegate = Function : Boolean Of Object;

  TUixCodeEdit = Class(TSynEdit)
    Private
      FBalloon : TUixBalloon;
      FBalloonAudible : Boolean;

      // Internals.
      FMask : TUixCodeMask;
      FTitle : String;
      FLastError : String;
      FAutoSelect : Boolean;
      FIgnoreOverwrite : Boolean;

      // Colours.
      FReadBackgroundColour : TColour;
      FWriteBackgroundColour : TColour;
      FInvalidBackgroundColour : TColour;
      FHasInvalidBackgroundColour : Boolean;
      FDefaultBackgroundColour : TColour;
      FHasDefaultBackgroundColour : Boolean;

      // Events.
      FChangeDelegate : TUixCodeEditChangeDelegate;
      FLoseFocusDelegate : TUixCodeEditLoseFocusDelegate;
      FRepositionDelegate : TUixCodeEditRepositionDelegate;
      FStandardKeyPressDelegate : TUixCodeEditStandardKeyPressDelegate;
      FValidateContentDelegate : TUixCodeEditValidateContentDelegate;

      // Validation/Options.
      FMandatory : Boolean;
      FValidationMode : TUixCodeEditValidationMode;

      FAllowedCharacterList : TFslCharacterList;
      FDisallowedCharacterList : TFslCharacterList;

      FAllowLeadingWhitespace : Boolean;
      FEnforceNumeric : Boolean;
      FEnforceAlphabetic : Boolean;
      FDefaultStringValue : String;

      FMinIntegerValue : Int64;
      FMaxIntegerValue : Int64;
      FDefaultIntegerValue : Int64;
      FHasDefaultIntegerValue : Boolean;

      FMinExtendedValue : Extended;
      FMaxExtendedValue : Extended;
      FExtendedDecimalPlaces : Integer;

      FMinCurrencyValue : TCurrency;
      FMaxCurrencyValue : TCurrency;
      FDenominationCurrencyValue : TCurrency;

      FMinDurationValue : TDurationMS;
      FMaxDurationValue : TDurationMS;
      FIncludeHours : Boolean;
      FIncludeMinutes : Boolean;
      FIncludeSeconds : Boolean;
      FIncludeMilliSeconds : Boolean;
      FExtendedHours : Boolean;

      FMinDateTimeValue : TDateTime;
      FMaxDateTimeValue : TDateTime;
      FIncludeDate : Boolean;
      FIncludeTime : Boolean;
      FIncludeDays : Boolean;
      FIncludeMonths : Boolean;
      FIncludeYears : Boolean;
      FAllowsFuture : Boolean;
      FAllowsPast : Boolean;
      FAssumedNow : TDateTime;
      FTimeOptional : Boolean;

{$IFDEF VER130}
      Function GetHighlighter : TUixCodeHighlighter;
{$ELSE}
      Function GetHighlighter : TUixUnicodeCodeHighlighter;
{$ENDIF}

      Function GetFixedLength: Integer;

      Function GetMandatory : Boolean;
      Procedure SetMandatory(Const Value : Boolean);

      Function GetText : String;
      Procedure SetText(Const Value : String);

      Function GetIsDefaultStringValue : Boolean;
      Procedure SetIsDefaultStringValue(Const Value : Boolean);

      Function GetDefaultStringValue : String;
      Procedure SetDefaultStringValue(Const Value : String);

      Function GetMask : String;
      Procedure SetMask(Const Value : String);

      Function GetMaxLength : Integer;
      Procedure SetMaxLength(Const Value : Integer);

      Function GetMinLength : Integer;
      Procedure SetMinLength(Const Value : Integer);

      Function GetEnforceCapitals : Boolean;
      Procedure SetEnforceCapitals(Const Value : Boolean);

      Function GetColor : TColor;
      Procedure SetColor(Const Value : TColor);

      Function GetParentColor : Boolean;
      Procedure SetParentColor(Const Value : Boolean);

      Function GetAnchoredBottom : Boolean;
      Procedure SetAnchoredBottom(Const Value : Boolean);

      Function GetAnchoredLeft : Boolean;
      Procedure SetAnchoredLeft(Const Value : Boolean);

      Function GetAnchoredRight : Boolean;
      Procedure SetAnchoredRight(Const Value : Boolean);

      Function GetAnchoredTop : Boolean;
      Procedure SetAnchoredTop(Const Value : Boolean);

      Function GetBolded : Boolean;
      Procedure SetBolded(Const Value : Boolean);

      Function GetIncludeTime : Boolean;
      Procedure SetIncludeTime(Const Value : Boolean);

      Function GetIncludeDate : Boolean;
      Procedure SetIncludeDate(Const Value : Boolean);

      Function GetIncludeDays : Boolean;
      Procedure SetIncludeDays(Const Value : Boolean);

      Function GetIncludeMonths : Boolean;
      Procedure SetIncludeMonths(Const Value : Boolean);

      Function GetIncludeYears : Boolean;
      Procedure SetIncludeYears(Const Value : Boolean);

      Function GetHasTime : Boolean;
      Procedure SetHasTime(Const bValue : Boolean);

      Function GetIncludeMilliseconds : Boolean;
      Procedure SetIncludeMilliSeconds(Const Value: Boolean);

      Function GetIncludeSeconds : Boolean;
      Procedure SetIncludeSeconds(Const Value: Boolean);

      Function GetIncludeMinutes : Boolean;
      Procedure SetIncludeMinutes(Const Value : Boolean);

      Function GetIncludeHours : Boolean;
      Procedure SetIncludeHours(Const Value : Boolean);

      Function GetExtendedHours : Boolean;
      Procedure SetExtendedHours(Const Value : Boolean);

      Function GetAllowsFuture : Boolean;
      Procedure SetAllowsFuture(Const Value : Boolean);

      Function GetAllowsPast : Boolean;
      Procedure SetAllowsPast(Const Value : Boolean);

      Function GetTimeOptional : Boolean;
      Procedure SetTimeOptional(Const Value : Boolean);

      Function GetMinDateTimeValue : TDateTime;
      Procedure SetMinDateTimeValue(Const Value : TDateTime);

      Function GetMaxDateTimeValue : TDateTime;
      Procedure SetMaxDateTimeValue(Const Value : TDateTime);

      Function GetMinDurationValue : TDurationMS;
      Procedure SetMinDurationValue(Const Value : TDurationMS);

      Function GetMaxDurationValue : TDurationMS;
      Procedure SetMaxDurationValue(Const Value : TDurationMS);

      Function GetMinCurrencyValue : TCurrency;
      Procedure SetMinCurrencyValue(Const Value : TCurrency);

      Function GetMaxCurrencyValue : TCurrency;
      Procedure SetMaxCurrencyValue(Const Value : TCurrency);

      Function GetDenominationCurrencyValue : TCurrency;
      Procedure SetDenominationCurrencyValue(Const Value : TCurrency);

      Function GetMaxIntegerValue : Int64;
      Procedure SetMaxIntegerValue(Const Value : Int64);

      Function GetMinIntegerValue : Int64;
      Procedure SetMinIntegerValue(Const Value : Int64);

      Function GetDefaultIntegerValue : Int64;
      Procedure SetDefaultIntegerValue(Const Value : Int64);

      Function GetHasDefaultIntegerValue : Boolean;
      Procedure SetHasDefaultIntegerValue(Const Value : Boolean);

      Function GetExtendedDecimalPlaces : Integer;
      Procedure SetExtendedDecimalPlaces(Const Value : Integer);

      Function GetMaxExtendedValue : Extended;
      Procedure SetMaxExtendedValue(Const Value : Extended);

      Function GetMinExtendedValue : Extended;
      Procedure SetMinExtendedValue(Const Value : Extended);

      Function GetValueAsDateTimeOffset : TDateTimeOffset;
      Procedure SetValueAsDateTimeOffset(Const Value : TDateTimeOffset);

      Function GetValueAsDateTime : TDateTime;
      Procedure SetValueAsDateTime(Const Value : TDateTime);

      Function GetValueAsDuration : TDurationMS;
      Procedure SetValueAsDuration(Const Value : TDurationMS);

      Function GetValueAsString : String;
      Procedure SetValueAsString(Const Value : String);

      Function GetValueAsInteger64 : Int64;
      Procedure SetValueAsInteger64(Const Value : Int64);

      Function GetValueAsInteger32 : LongInt;
      Procedure SetValueAsInteger32(Const Value : LongInt);

      Function GetValueAsCardinal : Cardinal;
      Procedure SetValueAsCardinal(Const Value : Cardinal);

      Function GetValueAsCurrency : TCurrency;
      Procedure SetValueAsCurrency(Const Value : TCurrency);

      Function GetValueAsExtended : Extended;
      Procedure SetValueAsExtended(Const Value : Extended);

      Procedure SetReadBackgroundColour(Const Value: TColour);
      Procedure SetWriteBackgroundColour(Const Value: TColour);

      Procedure InternalChangeDelegate(oSender : TObject);
      Procedure InternalExitDelegate(oSender : TObject);

      Procedure RefreshColor;

    Protected
      Procedure Complete;
      Procedure Change;

      Procedure Prepare;
      Procedure PrepareMask;
      Procedure PrepareDateTimeMask;
      Procedure PrepareDurationMask;
      Procedure PrepareIntegerMask;
      Procedure PrepareExtendedMask;

      Procedure Error(Const sMethod, sMessage : String);

      Procedure EnforceValidationModeString;
      Procedure EnforceValidationModeDateTime;
      Procedure EnforceValidationModeDuration;
      Procedure EnforceValidationModeCurrency;
      Procedure EnforceValidationModeInteger;
      Procedure EnforceValidationModeExtended;

      Function DisplayDateTime(Const aValue : TDateTime) : String; Overload;
      Function DisplayDateTime(Const aValue : TDateTimeOffset) : String; Overload;
      Function DisplayDuration(Const aValue : TDurationMS) : String;
      Function DisplayExtended(Const aValue : Extended) : String;

      Function IsValidContent : Boolean;
      Function IsValidLength : Boolean;

      Function ValidateContent : Boolean;
      Function ValidateLength : Boolean;

      Procedure SetCaretXYEx(bCallEnsureCursorPos: Boolean; aValue: TBufferCoord); Override;
      Procedure SetReadOnly(Value : Boolean); Override;

      Function GetEditPart : Cardinal;
      Function GetEditState : Cardinal;
      Procedure AdjustContentRect(Var aRect : TRect);
      Procedure CreateParams(Var Params: TCreateParams); Override;
      Procedure WMNCCalcSize(Var Message: TWMNCCalcSize); Message WM_NCCALCSIZE;
      Procedure WMNCPaint(Var Message: TMessage); Message WM_NCPAINT;
      Procedure Resize; Override;

      Procedure CMEnter(Var Message: TCMGotFocus); Message CM_ENTER;
      Procedure DoExit; Override;

      Function CanInsertCharacter : Boolean;
      Function CanPaste : Boolean;

      Procedure AdjustSelectionStart;
      Procedure AdjustSelectionEnd;

      Function AllowInsertCharacter(Const cChar : Char) : Boolean;

      Function ValidateCharacter(Const cChar : Char) : Boolean;
      Function ValidateInsertCharacter(Const cChar : Char) : Boolean;

      Function NormaliseDuration : String;

      Property OnChange;
      Property OnExit;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignClient;

      Procedure ShowBalloon;
      Procedure HideBalloon;

      Procedure ExecuteCommand(Command: TSynEditorCommand; cChar: Char; Data: Pointer); Override;

      Function Empty : Boolean;
      Function Full : Boolean;
      Function Valid : Boolean;

      Procedure ValidationModeString;
      Procedure ValidationModeDateTime;
      Procedure ValidationModeDuration;
      Procedure ValidationModeCurrency;
      Procedure ValidationModeInteger;
      Procedure ValidationModeExtended;

      Function IsValidationModeString : Boolean;
      Function IsValidationModeDateTime : Boolean;
      Function IsValidationModeDuration : Boolean;
      Function IsValidationModeCurrency : Boolean;
      Function IsValidationModeInteger : Boolean;
      Function IsValidationModeExtended : Boolean;

      Function HasDate : Boolean;
      Function HasPartialDate : Boolean;
      Function HasCompleteDate : Boolean;
      Function HasDateTime : Boolean;

      Function DateFormat : String;
      Function TimeFormat : String;
      Function DateTimeFormat : String;

      Function DurationFormat : String;

      Procedure CursorStart;
      Procedure CursorEnd;
      Procedure Select(Const iFrom, iTo : Integer);

      Procedure ApplyMask(Const sText : String);

      Function LeadingFixedText : String;

      Function Conforms(Const sText : String): Boolean;

      Property BalloonAudible : Boolean Read FBalloonAudible Write FBalloonAudible;
      Property LastError : String Read FLastError Write FLastError;
      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;
      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property IgnoreOverwrite : Boolean Read FIgnoreOverwrite Write FIgnoreOverwrite;
      Property Mandatory : Boolean Read GetMandatory Write SetMandatory;
      Property AllowedCharacterList : TFslCharacterList Read FAllowedCharacterList;
      Property DisallowedCharacterList : TFslCharacterList Read FDisallowedCharacterList;

      Property Mask : String Read GetMask Write SetMask;
      Property Text : String Read GetText Write SetText;
      Property Title : String Read FTitle Write FTitle;

      Property AutoSelect : Boolean Read FAutoSelect Write FAutoSelect;

{$IFDEF VER130}
      Property Highlighter : TUixCodeHighlighter Read GetHighlighter;
{$ELSE}
      Property Highlighter : TUixUnicodeCodeHighlighter Read GetHighlighter;
{$ENDIF}

      Property MinLength : Integer Read GetMinLength Write SetMinLength;
      Property MaxLength : Integer Read GetMaxLength Write SetMaxLength;
      Property FixedLength : Integer Read GetFixedLength;

      Property AllowLeadingWhitespace : Boolean Read FAllowLeadingWhitespace Write FAllowLeadingWhitespace;
      Property EnforceNumeric : Boolean Read FEnforceNumeric Write FEnforceNumeric;
      Property EnforceAlphabetic : Boolean Read FEnforceAlphabetic Write FEnforceAlphabetic;
      Property EnforceCapitals : Boolean Read GetEnforceCapitals Write SetEnforceCapitals;
      Property DefaultStringValue : String Read GetDefaultStringValue Write SetDefaultStringValue;
      Property IsDefaultStringValue : Boolean Read GetIsDefaultStringValue Write SetIsDefaultStringValue;

      Property MinExtendedValue : Extended Read GetMinExtendedValue Write SetMinExtendedValue;
      Property MaxExtendedValue : Extended Read GetMaxExtendedValue Write SetMaxExtendedValue;
      Property ExtendedDecimalPlaces : Integer Read GetExtendedDecimalPlaces Write SetExtendedDecimalPlaces;

      Property MinDateTimeValue : TDateTime Read GetMinDateTimeValue Write SetMinDateTimeValue;
      Property MaxDateTimeValue : TDateTime Read GetMaxDateTimeValue Write SetMaxDateTimeValue;
      Property IncludeDate : Boolean Read GetIncludeDate Write SetIncludeDate;
      Property IncludeTime : Boolean Read GetIncludeTime Write SetIncludeTime;
      Property IncludeDays : Boolean Read GetIncludeDays Write SetIncludeDays;
      Property IncludeMonths : Boolean Read GetIncludeMonths Write SetIncludeMonths;
      Property IncludeYears : Boolean Read GetIncludeYears Write SetIncludeYears;
      Property AllowsPast : Boolean Read GetAllowsPast Write SetAllowsPast;
      Property AllowsFuture : Boolean Read GetAllowsFuture Write SetAllowsFuture;
      Property HasTime : Boolean Read GetHasTime Write SetHasTime;
      Property AssumedNow : TDateTime Read FAssumedNow Write FAssumedNow;
      Property TimeOptional : Boolean Read GetTimeOptional Write SetTimeOptional;

      Property MinDurationValue : TDurationMS Read GetMinDurationValue Write SetMinDurationValue;
      Property MaxDurationValue : TDurationMS Read GetMaxDurationValue Write SetMaxDurationValue;
      Property IncludeHours : Boolean Read GetIncludeHours Write SetIncludeHours;
      Property IncludeMinutes : Boolean Read GetIncludeMinutes Write SetIncludeMinutes;
      Property IncludeSeconds : Boolean Read GetIncludeSeconds Write SetIncludeSeconds;
      Property IncludeMilliSeconds : Boolean Read GetIncludeMilliSeconds Write SetIncludeMilliSeconds;
      Property ExtendedHours : Boolean Read GetExtendedHours Write SetExtendedHours;

      Property MaxCurrencyValue : TCurrency Read GetMaxCurrencyValue Write SetMaxCurrencyValue;
      Property MinCurrencyValue : TCurrency Read GetMinCurrencyValue Write SetMinCurrencyValue;
      Property DenominationCurrencyValue : TCurrency Read GetDenominationCurrencyValue Write SetDenominationCurrencyValue;

      Property MinIntegerValue : Int64 Read GetMinIntegerValue Write SetMinIntegerValue;
      Property MaxIntegerValue : Int64 Read GetMaxIntegerValue Write SetMaxIntegerValue;
      Property DefaultIntegerValue : Int64 Read GetDefaultIntegerValue Write SetDefaultIntegerValue;
      Property HasDefaultIntegerValue : Boolean Read GetHasDefaultIntegerValue Write SetHasDefaultIntegerValue;

      Property ValueAsString : String Read GetValueAsString Write SetValueAsString;
      Property ValueAsDateTimeOffset : TDateTimeOffset Read GetValueAsDateTimeOffset Write SetValueAsDateTimeOffset;
      Property ValueAsDateTime : TDateTime Read GetValueAsDateTime Write SetValueAsDateTime;
      Property ValueAsDuration : TDurationMS Read GetValueAsDuration Write SetValueAsDuration;
      Property ValueAsCurrency : TCurrency Read GetValueAsCurrency Write SetValueAsCurrency;
      Property ValueAsInteger64 : Int64 Read GetValueAsInteger64 Write SetValueAsInteger64;
      Property ValueAsInteger32 : LongInt Read GetValueAsInteger32 Write SetValueAsInteger32;
      Property ValueAsCardinal : Cardinal Read GetValueAsCardinal Write SetValueAsCardinal;
      Property ValueAsExtended : Extended Read GetValueAsExtended Write SetValueAsExtended;

      Property Color : TColor Read GetColor Write SetColor Default clWindow;
      Property ParentColor : Boolean Read GetParentColor Write SetParentColor Default False;
      Property ReadBackgroundColour : TColour Read FReadBackgroundColour Write SetReadBackgroundColour;
      Property WriteBackgroundColour : TColour Read FWriteBackgroundColour Write SetWriteBackgroundColour;
      Property InvalidBackgroundColour : TColour Read FInvalidBackgroundColour Write FInvalidBackgroundColour;
      Property HasInvalidBackgroundColour : Boolean Read FHasInvalidBackgroundColour Write FHasInvalidBackgroundColour;
      Property DefaultBackgroundColour : TColour Read FDefaultBackgroundColour Write FDefaultBackgroundColour;
      Property HasDefaultBackgroundColour : Boolean Read FHasDefaultBackgroundColour Write FHasDefaultBackgroundColour;

      // SynEdit doesn't expose any functionality to capture the change event within a subclass, while still
      // maintaining an OnChange property that can be set externally. So we hide the event and redeclare as ChangeDelegate.

      Property ChangeDelegate : TUixCodeEditChangeDelegate Read FChangeDelegate Write FChangeDelegate;
      Property LoseFocusDelegate : TUixCodeEditLoseFocusDelegate Read FLoseFocusDelegate Write FLoseFocusDelegate;
      Property RepositionDelegate : TUixCodeEditRepositionDelegate Read FRepositionDelegate Write FRepositionDelegate;
      Property StandardKeyPressDelegate : TUixCodeEditStandardKeyPressDelegate Read FStandardKeyPressDelegate Write FStandardKeyPressDelegate;
      Property ValidateContentDelegate : TUixCodeEditValidateContentDelegate Read FValidateContentDelegate Write FValidateContentDelegate;
  End;

  TUixCodeEditClass = Class Of TUixCodeEdit;

  TUixCodeEditList = Class(TComponentList)
    Private
      Function GetEditByIndex(Const iIndex : Integer):  TUixCodeEdit;
      Procedure SetEditByIndex(Const iIndex : Integer; Const Value : TUixCodeEdit);

    Public
      Property EditByIndex[Const iIndex : Integer] : TUixCodeEdit Read GetEditByIndex Write SetEditByIndex; Default;
  End;

  TUixComboBox = Class(TComboBox)
    Private
      FBalloon : TUixBalloon;
      FBalloonAudible : Boolean;

      FTitle : String;
      FLastError : String;
      FMinLength : Integer;

      FInvalidBackgroundColour : TColour;

      FCustomBackgoundHandling : Boolean;

      Function GetStrict : Boolean;
      Procedure SetStrict(Const Value: Boolean);

      Function GetMandatory : Boolean;
      Procedure SetMandatory(Const Value : Boolean);

      Function GetReadOnly : Boolean;
      Procedure SetReadOnly(Const Value : Boolean);

      Procedure SetMinLength(Const Value: Integer);

      Function GetNotReadOnly: Boolean;
      Procedure SetNotReadOnly(Const Value: Boolean);

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetValue : Integer;
      Procedure SetValue(Const Value : Integer);

      {$IFDEF VER130}
      Function GetItemCount : Integer;
      {$ENDIF}

      Function GetInvalidBackgroundColour : TColour;
      Procedure SetInvalidBackgroundColour(Const Value : TColour);

    Protected
      Procedure KeyPress(Var cKey : Char); Override;
      Procedure DoExit; Override;

      Procedure Change; Override;

      Procedure Error(Const sMethod, sMessage : String);

      Property CustomBackgoundHandling : Boolean Read FCustomBackgoundHandling Write FCustomBackgoundHandling;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure HideBalloon;
      Procedure ShowBalloon;

      Procedure Prepare;

      Function Valid : Boolean;
      Function Empty : Boolean;
      Function Full : Boolean;

      Procedure AddValues(Const aValues : Array Of String); Overload;
      Procedure AddValues(oValues : TFslStringList); Overload;
      Function AddValue(Const sValue : String) : Integer;
      Procedure RemoveValue(Const sValue : String);
      Procedure ClearValues;
      Function IndexByValue(Const sValue : String) : Integer;
      Procedure DeleteByIndex(Const iIndex : Integer);

      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignClient;

      Procedure InvalidValue;

      Procedure DropdownAll;

      Property LastError : String Read FLastError Write FLastError;
      Property Value : Integer Read GetValue Write SetValue;
      {$IFDEF VER130}
      Property ItemCount : Integer Read GetItemCount;
      {$ELSE}
      Property ItemCount;
      {$ENDIF}
      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;
      Property BalloonAudible : Boolean Read FBalloonAudible Write FBalloonAudible;

      Property Title : String Read FTitle Write FTitle;
      Property MinLength : Integer Read FMinLength Write SetMinLength;
      Property Enabled : Boolean Read GetNotReadOnly Write SetNotReadOnly;
      Property Mandatory : Boolean Read GetMandatory Write SetMandatory;
      Property ReadOnly : Boolean Read GetReadOnly Write SetReadOnly;
      Property Strict : Boolean Read GetStrict Write SetStrict;
      Property InvalidBackgroundColour : TColour Read GetInvalidBackgroundColour Write SetInvalidBackgroundColour;
  End;

  TUixMenuItemEvent = TNotifyEvent;

  TUixMenuItemClass = Class Of TUixMenuItem;

  TUixMenuItem = Class(TMenuItem)
    Private
      FTag : Int64;
      FSelectedSubMenuImageList : TCustomImageList;
      FUseShortcut : Boolean;
      FMinimumWidth : Integer;

    Protected
      Procedure MeasureMenuItem(Sender: TObject; ACanvas: TCanvas; Var Width, Height: Integer);
      Procedure DrawMenuItem(Sender: TObject; ACanvas: TCanvas; ARect: TRect; Selected: Boolean);

      Function GetSelectedImageList : TCustomImageList;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Function ClassType : TUixMenuItemClass; Reintroduce; Overload; Virtual;

      Procedure BreakBar; Overload; Virtual;
      Procedure BreakNone; Overload; Virtual;

      Procedure SetSeparator; Overload; Virtual;
      Function IsSeparator : Boolean; Overload; Virtual;

      Function AddItem(Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer = -1): TUixMenuItem; Overload; Virtual;
      Function AddSeparator: TUixMenuItem; Overload; Virtual;
      Function AddSubmenu(Const sCaption: String; iImage: Integer): TUixMenuItem; Overload; Virtual;

      Function InsertItem(iIndex: Integer; Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem; Overload; Virtual;
      Function InsertSeparator(iIndex: Integer): TUixMenuItem; Overload; Virtual;
      Function InsertSubmenu(iIndex: Integer; Const sCaption: String; iImage: Integer): TUixMenuItem; Overload; Virtual;

      Function MeasuredHeight: Integer;

      Property SelectedSubMenuImageList : TCustomImageList Read FSelectedSubMenuImageList Write FSelectedSubMenuImageList;
      Property Tag : Int64 Read FTag Write FTag;
      Property UseShortcut : Boolean Read FUseShortcut Write FUseShortcut;
      Property MinimumWidth : Integer Read FMinimumWidth Write FMinimumWidth;
  End;

  TUixMenuItems = Class(TUixComponentList)
    Private
      Function GetMenuItemByIndex(iIndex: Integer): TUixMenuItem;

    Public
      Property MenuItemByIndex[iIndex : Integer] : TUixMenuItem Read GetMenuItemByIndex; Default;
  End;

  TUixPopupMenu = Class(TPopupMenu)
    Private
      FUseShortcuts : Boolean;

      FOnClosePopup : TNotifyEvent;
      FSelectedImageList : TCustomImageList;

      Function GetItems : TUixMenuItem;

      Procedure SetUseShortcuts(Const Value: Boolean);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Function MenuItemClass : TUixMenuItemClass; Overload; Virtual;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure FireClose;
      Procedure CloseMenu;

      Function IsShortCut(Var Message: TWMKey): Boolean; Override;

      Procedure AutomaticAutoLineReduction; Overload; Virtual;
      Procedure ManualAutoLineReduction; Overload; Virtual;

      Function AddItem(Const sCaption : String; aEvent : TUixMenuItemEvent; iImage : Integer = -1) : TUixMenuItem; Overload; Virtual;
      Function AddSeparator : TUixMenuItem; Overload; Virtual;
      Function AddSubMenu(Const sCaption : String; iImage : Integer = -1) : TUixMenuItem; Overload; Virtual;
      Function AddRadioItem(Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem; Overload; Virtual;

      Function InsertItem(iIndex : Integer; Const sCaption : String; aEvent : TUixMenuItemEvent; iImage : Integer = -1) : TUixMenuItem; Overload; Virtual;
      Function InsertSeparator(iIndex : Integer) : TUixMenuItem; Overload; Virtual;
      Function InsertSubmenu(iIndex : Integer; Const sCaption : String; iImage : Integer = -1) : TUixMenuItem; Overload; Virtual;

      Function VisibleCount : Integer; Overload; Virtual;

      Property Items : TUixMenuItem Read GetItems;
      Property UseShortcuts : Boolean Read FUseShortcuts Write SetUseShortcuts;
      Property SelectedImageList : TCustomImageList Read FSelectedImageList Write FSelectedImageList;

      Property OnClosePopup : TNotifyEvent Read FOnClosePopup Write FOnClosePopup;
  End;


Const
  ExtendedSeparatorBackgroundColour = $00EEE7DD;
  ExtendedSeparatorLineColour = $00C5C5C5;
  ExtendedGutterColour = $00EEEEE9;
  ExtendedItemBackgroundColour = $00FAFAFA;
  ExtendedItemSelectedColour = $00E6D5CB;
  ExtendedFontColour = $006E1500;
  ExtendedFontDisabledColour = $00DEC5D8;

  ExtendedGradientStart1 = $00EFE8E4;
  ExtendedGradientEnd1 = $00DEC5B8;
  ExtendedGradientStart2 = $00D8BAAB;
  ExtendedGradientEnd2 = $00EFE8E4;

  csDropDown = StdCtrls.csDropDown;
  csSimple = StdCtrls.csSimple;
  csDropDownList = StdCtrls.csDropDownList;

  akLeft = Controls.akLeft;
  akRight = Controls.akRight;
  akTop = Controls.akTop;
  akBottom = Controls.akBottom;


Type
  TUixToolButton = Class(TToolButton)
    Private
      FID : Cardinal;
      FDropDownOnClick : Boolean;

    Protected
      Procedure MouseDown(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer); Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Click; Override;

      Procedure StyleButton;
      Procedure StyleCheck;
      Procedure StyleDropDown;
      Procedure StyleSeparator;
      Procedure StyleDivider;

      Function IsStyleSeparator : Boolean;
      Function IsStyleButton : Boolean;
      Function IsStyleCheck : Boolean;
      Function IsStyleDropDown : Boolean;
      Function IsStyleDivider : Boolean;

      Property ID : Cardinal Read FID Write FID;
      Property DropDownOnClick : Boolean Read FDropDownOnClick Write FDropDownOnClick;
  End;

  TUixToolButtonClass = Class Of TUixToolButton;

  TUixToolBarGradientType = (UixToolBarGradientTypeNone, UixToolBarGradientTypeVertical, UixToolBarGradientTypeHorizontal);

  TUixToolBar = Class(TToolbar)
    Private
      FGradientType : TUixToolBarGradientType;
      FGradientColourTop : TColour;
      FGradientColourBottom : TColour;
      FGradientColourLeft : TColour;
      FGradientColourRight : TColour;

      Function GetButton(Const iIndex : Integer) : TUixToolButton;

      Function GetEdgeBorderBottom: Boolean;
      Procedure SetEdgeBorderBottom(Const Value: Boolean);

      Function GetEdgeBorderTop: Boolean;
      Procedure SetEdgeBorderTop(Const Value: Boolean);

      Function GetEdgeBorder: Boolean;
      Procedure SetEdgeBorder(Const Value: Boolean);

      Procedure CustomDrawDelegate(Sender: TToolBar; Const ARect: TRect; Var DefaultDraw: Boolean);

    Protected
      Function ButtonClass : TUixToolButtonClass;

      Function ButtonNew : TUixToolButton;
      Function SeparatorNew : TUixToolButton;
      Function DropDownNew : TUixToolButton;
      Function CheckNew: TUixToolButton;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Refresh;

      Function AddButton(Const sCaption : String; aEvent : TNotifyEvent; Const iImage : Integer = -1) : TUixToolButton;
      Function AddDropDown(Const sCaption : String; oPopup : TUixPopupMenu; Const iImage : Integer = -1) : TUixToolButton;
      Function AddCheck(Const sCaption : String; aOnClick : TNotifyEvent; iImage : Integer = -1) : TUixToolButton;
      Function AddSeparator : TUixToolButton;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure EdgeInnerRaised;
      Procedure EdgeOuterLowered;
      Procedure EdgeInnerNone;
      Procedure EdgeOuterNone;

      Procedure ShuffleBottom;
      Procedure ShuffleTop;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Procedure GradientNone;
      Procedure GradientVertical;
      Procedure GradientHorizontal;

      Function RequiredHeight : Integer;
      Function RequiredWidth : Integer;

      Property Buttons[Const iIndex : Integer] : TUixToolButton Read GetButton; Default;
      Property EdgeBorder : Boolean Read GetEdgeBorder Write SetEdgeBorder;
      Property EdgeBorderTop : Boolean Read GetEdgeBorderTop Write SetEdgeBorderTop;
      Property EdgeBorderBottom : Boolean Read GetEdgeBorderBottom Write SetEdgeBorderBottom;

      Property GradientColourTop : TColour Read FGradientColourTop Write FGradientColourTop;
      Property GradientColourBottom : TColour Read FGradientColourBottom Write FGradientColourBottom;
      Property GradientColourLeft : TColour Read FGradientColourLeft Write FGradientColourLeft;
      Property GradientColourRight : TColour Read FGradientColourRight Write FGradientColourRight;

    Published
      Property OnKeyPress;
      Property OnKeyUp;
      Property OnKeyDown;
  End;

  TUixToolbarClass = Class Of TUixToolbar;

  TUixToolbarList = Class(TComponentList)
    Private
      Function GetToolbarByIndex(iIndex: Integer): TUixToolbar;

    Public
      Property ToolbarByIndex[iIndex : Integer] : TUixToolbar Read GetToolbarByIndex; Default;
  End;


Type
  TUixHTMLColourComboBox = Class(TCustomComboBox)
    Private
      Function GetValue : TColour;
      Procedure SetValue(iValue: TColour);

      Procedure CNDrawItem(Var Message: TWMDrawItem); Message CN_DRAWITEM;
      Function ReadColor(sText : String; aDefault : TColour):TColour;

    Protected
      Procedure CreateHandle; Override;
      Procedure DrawItem(Index: Integer; aRect: Windows.TRect; State: TOwnerDrawState); Override;

      Property Items Stored False;
      Property Style Default csOwnerDrawFixed;

      Procedure Change; Override;

    Public
      constructor Create(AOwner: TComponent); Override;

      Property Value : TColour Read GetValue Write SetValue;

    Published
      Property Color;
      Property Ctl3D;
      Property DragCursor;
      Property DragMode;
      Property DropDownCount;
      Property Enabled;
      Property Font;
      Property ImeMode;
      Property ImeName;
      Property ItemHeight;
      Property ParentColor;
      Property ParentCtl3D;
      Property ParentFont;
      Property ParentShowHint;
      Property PopupMenu;
      Property ShowHint;
      Property Sorted;
      Property TabOrder;
      Property TabStop;
      Property Text;
      Property Visible;
      Property OnChange;
      Property OnClick;
      Property OnContextPopup;
      Property OnDblClick;
      Property OnDragDrop;
      Property OnDragOver;
      Property OnDropDown;
      Property OnEndDrag;
      Property OnEnter;
      Property OnExit;
      Property OnKeyDown;
      Property OnKeyPress;
      Property OnKeyUp;
      Property OnStartDrag;
  End;


Type
  TUixGroupBox = Class(TGroupBox)
    Private
      FShowBorder : Boolean;
      FLeftMargin : Integer;

      Procedure SetLeftMargin(Const iValue : Integer);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure AdjustClientRect(Var aRect : TRect); Override;
      Procedure Paint; Override;

      Procedure SetEnabled(bValue : Boolean); Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Function HeaderHeight : Integer; Overload; Virtual;
      Function HeaderRect : TRect; Overload; Virtual;

      Function VisibleControlHeight : Integer; Overload; Virtual;
      Function VisibleControlWidth : Integer; Overload; Virtual;

      Function RequiredNonClientHeight : Integer; Overload; Virtual;
      Function RequiredClientHeight : Integer; Overload; Virtual;
      Function RequiredHeight : Integer; Overload; Virtual;

      Function RequiredNonClientWidth : Integer; Overload; Virtual;
      Function RequiredClientWidth : Integer; Overload; Virtual;
      Function RequiredWidth : Integer; Overload; Virtual;

      Procedure Reset; Overload; Virtual;

      Procedure AlignClient; Overload; Virtual;
      Procedure AlignLeft; Overload; Virtual;
      Procedure AlignRight; Overload; Virtual;
      Procedure AlignTop; Overload; Virtual;
      Procedure AlignBottom; Overload; Virtual;

      Function IsAlignedBottom : Boolean; Overload; Virtual;
      Function IsAlignedClient : Boolean; Overload; Virtual;
      Function IsAlignedLeft : Boolean; Overload; Virtual;
      Function IsAlignedRight : Boolean; Overload; Virtual;
      Function IsAlignedTop : Boolean; Overload; Virtual;

      Procedure ShuffleBottom; Overload; Virtual;

      Property BorderWidth;

    Published
      Property ShowBorder : Boolean Read FShowBorder Write FShowBorder;
      Property LeftMargin : Integer Read FLeftMargin Write SetLeftMargin;
  End;

  TUixGroupBoxes = Class(TComponentList)
    Private
      Function GetGroupBox(Const iIndex : Integer): TUixGroupBox;

    Public
      Property GroupBoxes[Const iIndex : Integer] : TUixGroupBox Read GetGroupBox; Default;
  End;

  TUixGroupBoxClass = Class Of TUixGroupBox;

  TUixEdit = Class(TMaskEdit)
    Private
      FTitle : String;
      FBalloon : TUixBalloon;
      FLastError : String;
      FMinLength : Integer;
      FMandatory : Boolean;
      FAllowedCharacterList : TFslCharacterList;
      FDisallowedCharacterList : TFslCharacterList;
      FEnforceNumeric : Boolean;
      FEnforceAlphabetic : Boolean;
      FEnforceCapitals : Boolean;
      FGhostText : String;
      FValueIsGhostText : Boolean;
      FShowingGhostText : Boolean;

      // Colours.
      FReadBackgroundColour : TColour;
      FWriteBackgroundColour : TColour;
      FDefaultTextColour : TColour;
      FGhostTextColour : TColour;
      FInvalidBackgroundColour : TColour;
      FHasInvalidBackgroundColour : Boolean;

      FMouseEnterHandler : TNotifyEvent;
      FMouseLeaveHandler : TNotifyEvent;

      Function GetNotReadOnly: Boolean;
      Procedure SetNotReadOnly(Const Value : Boolean);

      Function GetReadOnly: Boolean;
      Procedure SetReadOnly(Const Value : Boolean);

      Function GetMandatory : Boolean;
      Procedure SetMandatory(Const bValue : Boolean);

      Function GetMask: String;
      Procedure SetMask(Const Value : String);

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetBolded: Boolean;
      Procedure SetBolded(Const Value: Boolean);

      Function GetValue : String;
      Procedure SetValue(Const sValue : String);

      Function GetGhostText: String;
      Procedure SetGhostText(Const sValue: String);

      Function GetHasGhostText: Boolean;
      Function GetCanShowGhostText : Boolean;
      Function GetCanHideGhostText: Boolean;

      Procedure CMMouseEnter(Var aMessage : TMessage); Message CM_MOUSEENTER;
      Procedure CMMouseLeave(Var aMessage : TMessage); Message CM_MOUSELEAVE;

    Protected
      Procedure Initialise;
      Procedure Finalise;

      Procedure Change; Override;
      Procedure Prepare;

      Procedure ShowGhostText;
      Procedure HideGhostText;
      Function IsGhostTextColour : Boolean;

      Procedure KeyPress(Var Key: Char); Override;

      Function TranslateCharacter(Const cKey : Char) : Char;

      Function IsValidLength : Boolean;

      Function ValidateLength : Boolean;
      Function ValidateContent : Boolean;

      Function ValidateCharacter(Const cChar : Char) : Boolean;

      Procedure DoEnter; Override;
      Procedure DoExit; Override;

      Function BalloonNew : TUixBalloon;
      Function BalloonClass : TUixBalloonClass;
      Function TextLength: Integer;

      Property HasGhostText : Boolean Read GetHasGhostText;
      Property CanShowGhostText : Boolean Read GetCanShowGhostText;
      Property CanHideGhostText : Boolean Read GetCanHideGhostText;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure ShowBalloon;
      Procedure HideBalloon;

      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignClient;

      Function Valid : Boolean;
      Function Empty : Boolean;
      Function Full : Boolean;

      Procedure RefreshHeight;

      Function IsDisplayingGhostText : Boolean;

      Procedure ValidateEdit; Override;

      Property LastError : String Read FLastError Write FLastError;
      Property Mask : String Read GetMask Write SetMask;
      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;
      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property Title : String Read FTitle Write FTitle;
      Property ReadOnly : Boolean Read GetReadOnly Write SetReadOnly;
      Property Enabled : Boolean Read GetNotReadOnly Write SetNotReadOnly;
      Property MinLength : Integer Read FMinLength Write FMinLength;
      Property Value : String Read GetValue Write SetValue;
      Property Mandatory : Boolean Read GetMandatory Write SetMandatory;
      Property AllowedCharacterList : TFslCharacterList Read FAllowedCharacterList;
      Property DisallowedCharacterList : TFslCharacterList Read FDisallowedCharacterList;
      Property EnforceNumeric : Boolean Read FEnforceNumeric Write FEnforceNumeric;
      Property EnforceAlphabetic : Boolean Read FEnforceAlphabetic Write FEnforceAlphabetic;
      Property EnforceCapitals : Boolean Read FEnforceCapitals Write FEnforceCapitals;
      Property ReadBackgroundColour : TColour Read FReadBackgroundColour Write FReadBackgroundColour;
      Property WriteBackgroundColour : TColour Read FWriteBackgroundColour Write FWriteBackgroundColour;
      Property DefaultTextColour : TColour Read FDefaultTextColour Write FDefaultTextColour;
      Property GhostTextColour : TColour Read FGhostTextColour Write FGhostTextColour;
      Property InvalidBackgroundColour : TColour Read FInvalidBackgroundColour Write FInvalidBackgroundColour;
      Property HasInvalidBackgroundColour : Boolean Read FHasInvalidBackgroundColour Write FHasInvalidBackgroundColour;
      Property GhostText : String Read GetGhostText Write SetGhostText;

      Property MouseEnterHandler : TNotifyEvent Read FMouseEnterHandler Write FMouseEnterHandler;
      Property MouseLeaveHandler : TNotifyEvent Read FMouseLeaveHandler Write FMouseLeaveHandler;
  End;

  TUixEdits = Class(TComponentList)
    Private
      Function GetEdit(Const iIndex : Integer):  TUixEdit;
      Procedure SetEdit(Const iIndex : Integer; Const Value : TUixEdit);

    Public
      Property Edits[Const iIndex : Integer] : TUixEdit Read GetEdit Write SetEdit; Default;
  End;


Type
  TUixCheckBox = Class(TCheckBox)
    Private
      FTitle : String;

      Function GetReadOnly: Boolean;
      Procedure SetReadOnly(Const Value: Boolean);

      Function GetValue : Boolean;
      Procedure SetValue(Const Value : Boolean);

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetOnChange : TNotifyEvent;
      Procedure SetOnChange(Const Value : TNotifyEvent);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure WMPaint(Var Message: TWMPaint); Message WM_PAINT;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure AlignClient; Overload; Virtual;
      Procedure AlignLeft; Overload; Virtual;
      Procedure AlignRight; Overload; Virtual;
      Procedure AlignTop; Overload; Virtual;
      Procedure AlignBottom; Overload; Virtual;

      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;
      Property Value : Boolean Read GetValue Write SetValue;
      Property ReadOnly : Boolean Read GetReadOnly Write SetReadOnly;
      Property OnChange : TNotifyEvent Read GetOnChange Write SetOnChange;
      Property ClicksDisabled;
      Property Title : String Read FTitle Write FTitle;
  End;

  TUixCheckboxList = Class(TComponentList)
    Private
      Function GetCheckboxByIndex(iIndex : Integer) : TUixCheckbox;

    Public
      Property CheckboxByIndex[iIndex : Integer] : TUixCheckbox Read GetCheckboxByIndex; Default;
  End;


Type
  TUixPageControl = Class(TPageControl)
    Private
      FFullPages : Boolean;

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure AdjustClientRect(Var Rect: TRect); Override;

      Function RequiredHeight: Integer; Overload; Virtual;

      Function VisibleControlHeight(oTabSheet: TTabSheet): Integer; Overload; Virtual;
      Function VisibleControlHeight : Integer; Overload; Virtual;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Function GetDockTab(Control : TControl) : TTabSheet;

      Function GetTabHeight : Integer; Overload; Virtual;

      Procedure AlignClient; Overload; Virtual;
      Procedure AlignLeft; Overload; Virtual;
      Procedure AlignRight; Overload; Virtual;
      Procedure AlignTop; Overload; Virtual;
      Procedure AlignBottom; Overload; Virtual;
      Function IsAlignedTop : Boolean; Overload; Virtual;
      Function IsAlignedBottom : Boolean; Overload; Virtual;
      Function IsAlignedLeft : Boolean; Overload; Virtual;
      Function IsAlignedRight : Boolean; Overload; Virtual;
      Function IsAlignedClient : Boolean; Overload; Virtual;

      Procedure TabStyleButtons; Overload; Virtual;
      Procedure TabStyleFlatButtons; Overload; Virtual;
      Procedure TabStyleTabs; Overload; Virtual;

      Procedure Reset; Overload; Virtual;

    Published
      Property ParentColor;
      Property Color;
      Property FullPages : Boolean Read FFullPages Write FFullPages;
  End;

  TUixPageControls = Class(TComponentList)
  End;


Type
  TUixTabsheet = Class(TTabsheet)
    Private
      FBusy : Boolean;

      Procedure WMEraseBkgnd(Var Message: TWmEraseBkgnd); Message WM_ERASEBKGND;

      Function GetBackgroundColour: TColour;
      Procedure SetBackgroundColour(Const Value: TColour);

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

      Procedure PrepareInitialise; Overload; Virtual;
      Procedure PrepareFinalise; Overload; Virtual;

      Procedure Error(aException : EFslExceptionClass; Const sMethod, sMessage : String); Overload; Virtual;
      Procedure Error(Const sMethod, sMessage : String); Overload; Virtual;

      Function Condition(bCondition : Boolean; Const sLocation, sMessage : String) : Boolean; Overload;
      Function Invariant(Const sMethod, sMessage: String): Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TFslObject; aClass: TFslObjectClass; Const sObject : String): Boolean; Overload;
      Function Invariants(Const sLocation : String; aReference, aClass : TClass; Const sReference : String) : Boolean; Overload;

      Function GetBusy : Boolean; Overload; Virtual;
      Procedure SetBusy(Const Value: Boolean); Overload; Virtual;

      Function TabHeading : String; Overload; Virtual;

      Procedure RefreshClient; Overload; Virtual;
      Procedure RefreshHeading; Overload; Virtual;

      Property Busy : Boolean Read GetBusy Write SetBusy;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure Restore; Overload; Virtual;
      Procedure Refresh; Overload; Virtual;
      Procedure Commit; Overload; Virtual;

      Property BackgroundColour : TColour Read GetBackgroundColour Write SetBackgroundColour;
  End;

  TUixTabsheetList = Class(TFslPointerList)
    Private
      Function GetTabsheet(Const iIndex : Integer) : TUixTabsheet;

    Public
      Property Tabsheets[Const iIndex : Integer] : TUixTabsheet Read GetTabsheet; Default;
  End;


Type
  TUixRadioButton = Class(TRadioButton)
    Private
      FCanvas : TCanvas;
      FHasFocus : Boolean;

      Function GetValue : Boolean;
      Procedure SetValue(Const Value : Boolean);

      Procedure WMPaint(Var aMessage : TWMPaint); Message WM_PAINT;

    Protected
      Procedure SetChecked(bValue : Boolean); Override;

      Procedure DoEnter; Override;
      Procedure DoExit; Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AlignClient; Overload; Virtual;
      Procedure AlignLeft; Overload; Virtual;
      Procedure AlignRight; Overload; Virtual;
      Procedure AlignTop; Overload; Virtual;
      Procedure AlignBottom; Overload; Virtual;

      Procedure ShuffleLeft; Overload; Virtual;
      Procedure ShuffleRight; Overload; Virtual;
      Procedure ShuffleTop; Overload; Virtual;
      Procedure ShuffleBottom; Overload; Virtual;

      Property Value : Boolean Read GetValue Write SetValue;
  End;

  TUixRadioButtonList = Class(TComponentList)
    Private
      Function GetRadioButtonByIndex(iIndex: Integer): TUixRadioButton;

    Public
      Property RadioButtonByIndex[iIndex : Integer] : TUixRadioButton Read GetRadioButtonByIndex; Default;
  End;

  TUixRadioButtonClass = Class Of TUixRadioButton;

  TUixScrollBox = Class(TScrollBox)
    Private
      Procedure MouseWheelUpHandler(oSender : TObject; aShiftState : TShiftState; aMousePosition : TPoint; Var bHandled : Boolean);
      Procedure MouseWheelDownHandler(oSender : TObject; aShiftState : TShiftState; aMousePosition : TPoint; Var bHandled : Boolean);

    Protected
      Procedure Initialise; Virtual;
      Procedure Finalise; Virtual;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure BorderStyleNone;
      Procedure BorderStyleSingle;

      Procedure Reset;

      Function RequiredHeight : Integer;
      Function VisibleControlHeight : Integer;

    Published
      Property OnKeyDown;
  End;

  TUixScrollBoxClass = Class Of TUixScrollBox;

  TUixRadioGroup = Class(TCustomGroupBox)
    Private
      FItemIndex : Integer;
      FItemStringList : TStrings;
      FRadioButtonList : TList;
      FColumnCount : Integer;
      FReading : Boolean;
      FUpdating : Boolean;
      FItemHeight : Integer;
      FAutoSize : Boolean;

      Function GetValue : Integer;
      Procedure SetValue(Const Value : Integer);

      Function GetItemHeight : Integer;
      Procedure SetItemHeight(Const Value : Integer);

      Function GetAutoSizeProperty : Boolean;
      Procedure SetAutoSizeProperty(Const Value : Boolean);

      Procedure ArrangeButtons;
      Procedure UpdateButtons;

      Procedure SetButtonCount(Value : Integer);
      Procedure SetColumnCount(Value : Integer);
      Procedure SetItemIndex(Value : Integer);

      Procedure ButtonClick(oSender : TObject);
      Procedure ItemsChange(oSender : TObject);

      Procedure CMEnabledChanged(Var aMessage : TMessage); Message CM_ENABLEDCHANGED;
      Procedure CMFontChanged(Var aMessage : TMessage); Message CM_FONTCHANGED;
      Procedure WMSize(Var aMessage : TWMSize); Message WM_SIZE;

    Protected
      Procedure Paint; Override;
      Procedure Loaded; Override;
      Procedure ReadState(oReader : TReader); Override;
      Procedure AdjustClientRect(Var aRect : TRect); Override;

      Function CanModify : Boolean;

      Procedure RecalculateHeight;
      Procedure RefreshTabStop;

      Procedure DoExit; Override;

{$IFDEF VER130}
      Procedure GetChildren(aProc : TGetChildProc; oRoot : TComponent); Override;
{$ENDIF}

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

{$IFNDEF VER130}
      Procedure GetChildren(aProc : TGetChildProc; oRoot : TComponent); Override;
{$ENDIF}

      Procedure FlipChildren(bAllLevels : Boolean); Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure AddValue(Const sValue : String);
      Procedure AddValues(Const aValueArray : Array Of String);

      Function RequiredHeight : Integer;

      Property AutoSize : Boolean Read GetAutoSizeProperty Write SetAutoSizeProperty;
      Property ColumnCount : Integer Read FColumnCount Write SetColumnCount Default 1;
      Property ItemIndex : Integer Read FItemIndex Write SetItemIndex Default -1;
      Property ItemStringList : TStrings Read FItemStringList;
      Property ItemHeight : Integer Read GetItemHeight Write SetItemHeight;
      Property Value : Integer Read GetValue Write SetValue;

      Property Caption;
      Property Color;
      Property OnClick;
  End;

  TUixRadioGroupClass = Class Of TUixRadioGroup;

Type
  TUixBevel = Class(TBevel)
    Private
      FTitle : String;

    Protected
      Procedure Initialise; Overload; Virtual;
      Procedure Finalise; Overload; Virtual;

    Public
      constructor Create(oOwner : TComponent); Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure AlignClient; Overload; Virtual;
      Procedure AlignLeft; Overload; Virtual;
      Procedure AlignTop; Overload; Virtual;
      Procedure AlignRight; Overload; Virtual;
      Procedure AlignBottom; Overload; Virtual;

      Procedure StyleRaised; Overload; Virtual;
      Procedure StyleLowered; Overload; Virtual;

      Procedure ShapeTopLine; Overload; Virtual;
      Procedure ShapeBottomLine; Overload; Virtual;
      Procedure ShapeLeftLine; Overload; Virtual;
      Procedure ShapeRightLine; Overload; Virtual;
      Procedure ShapeBox; Overload; Virtual;
      Procedure ShapeSpacer; Overload; Virtual;

      Property Title : String Read FTitle Write FTitle;
  End;


Type
  TUixSplitter = Class(TSplitter)
    Private
      FGradientColour : TColour;

    Protected
      Procedure Paint; Override;

      Procedure MouseUp(aMouseButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer); Override;

    Public
      constructor Create(oOwner : TComponent); Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;

      Procedure ShuffleTop;
      Procedure ShuffleBottom;
      Procedure ShuffleLeft;
      Procedure ShuffleRight;

      Procedure ResizeStyleNone;
      Procedure ResizeStyleLine;
      Procedure ResizeStyleUpdate;
      Procedure ResizeStylePattern;

      Property GradientColour : TColour Read FGradientColour Write FGradientColour;
  End;

  TUixSplitterList = Class(TFslPointerList)
    Private
      Function GetSplitter(Const iIndex : Integer) : TUixSplitter;

    Public
      Property SplitterByIndex[Const iIndex : Integer] : TUixSplitter Read GetSplitter; Default;
  End;

  TUixSplitterClass = Class Of TUixSplitter;


Type
  TUixMemo = Class(TMemo)
    Private
      FHeightInLines : Integer;

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetReadOnly : Boolean;
      Procedure SetReadOnly(Const Value: Boolean);

    Protected
      Procedure CreateParams(Var aParamsRecord : TCreateParams); Override;

    Public
      constructor Create(oOwner : TComponent); Override;

      Procedure AlignLeft;
      Procedure AlignRight;
      Procedure AlignTop;
      Procedure AlignBottom;
      Procedure AlignClient;

      Procedure BorderNone;
      Procedure BorderSingle;

      Procedure ScrollbarVertical;
      Procedure ScrollbarBoth;

      Procedure SetHeightByLines;

      Function CalculateHeightByLineCount(Const iLineCount : Integer) : Integer;

      Property HeightInLines : Integer Read FHeightInLines Write FHeightInLines;
      Property ReadOnly : Boolean Read GetReadOnly Write SetReadOnly;
      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;

    Published
      Property Text;
  End;



Type
  TUixTreeViewHeaderPopup = Class(TUixPopupMenu)
    Private
      FInlineVisibility : Boolean;

      Procedure DoColumnVisbilityClick(Sender: TObject);

    Protected
      Procedure Initialise; Override;

    Public
      Procedure Refresh; Overload; Virtual;

      Property InlineVisibility : Boolean Read FInlineVisibility Write FInlineVisibility;
  End;

  TUixTreeViewHeaderPopupClass = Class Of TUixTreeViewHeaderPopup;



Type
  TUixTreeView = Class;

  TUixTreeViewIdentifier = String;

  TUixTreeViewIdentifiers = Class(TFslStringList)
    Public
      constructor Create; Override;

      Function Link : TUixTreeViewIdentifiers;
      Function Clone : TUixTreeViewIdentifiers;

      Procedure Remove(Const sValue : String);
      Procedure Toggle(Const sValue : String);
  End;

  TUixTreeViewCheckType = (htctNone, htctTriStateCheckBox, htctCheckBox, htctRadioButton, htctButton);
  TUixTreeViewCheckState = (htcsUncheckedNormal, htcsUncheckedPressed, htcsCheckedNormal, htcsCheckedPressed, htcsMixedNormal, htcsMixedPressed);

  TUixTreeViewNodes = Class; // Forward.

  TUixTreeViewNode = Class(TFslObject)
    Private
      FID : TUixTreeViewIdentifier;
      FNode : PVirtualNode;
      FParent : TUixTreeViewNode;
      FIndex : Integer;

      FChildren    : TUixTreeViewNodes;
      FHasChildren : Boolean;

      FCaptions : TFslStringList;
      FCaptionVisibilities : TFslBooleanList;
      FBackground : TColor;
      FForeground : TColor;
      FCheckState : TUixTreeViewCheckState;
      FCheckType : TUixTreeViewCheckType;
      FEnabledText : Boolean;
      FEnabledCheck : Boolean;
      FExpanded : Boolean;
      FImage : Integer;
      FSelected :  Boolean;
      FState : Integer;
      FStyles : TFontStyles;
      FVisible : Boolean;

      // To be replaced with an object identifier.
      FData : Pointer;

      Function GetChecked : Boolean;
      Procedure SetChecked(Const Value : Boolean);

      Function GetShowCheckbox : Boolean;
      Procedure SetShowCheckbox(Const Value : Boolean);

      Function GetShowRadioButton : Boolean;
      Procedure SetShowRadioButton(Const Value : Boolean);

      Function GetBolded : Boolean;
      Procedure SetBolded(Const Value : Boolean);

      Function GetItalicised : Boolean;
      Procedure SetItalicised(Const Value : Boolean);

      Function GetEnabled : Boolean;
      Procedure SetEnabled(Const Value : Boolean);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixTreeViewNode; Overload;

      Procedure Clear; Overload;

      Function HasCheckedChildren : Boolean; Overload;
      Function HasMixedChildren : Boolean; Overload;
      Function HasParent : Boolean; Overload;

      Procedure CheckTypeNone; Overload;
      Procedure CheckTypeTriState; Overload;
      Procedure CheckTypeCheckBox; Overload;
      Procedure CheckTypeRadioButton; Overload;
      Procedure CheckTypeButton; Overload;

      Function IsRoot : Boolean; Overload;
      Function IsLeaf : Boolean; Overload;

      Property Node : PVirtualNode Read FNode Write FNode;

      Property ID : TUixTreeViewIdentifier Read FID Write FID;
      Property Parent : TUixTreeViewNode Read FParent Write FParent;
      Property Index : Integer Read FIndex Write FIndex;
      Property Children : TUixTreeViewNodes Read FChildren;
      Property HasChildren : Boolean Read FHasChildren Write FHasChildren;
      Property Background : TColor Read FBackground Write FBackground;
      Property Foreground : TColor Read FForeground Write FForeground;
      Property Captions : TFslStringList Read FCaptions;
      Property CaptionVisibilities : TFslBooleanList Read FCaptionVisibilities;
      Property CheckType : TUixTreeViewCheckType Read FCheckType Write FCheckType;
      Property CheckState : TUixTreeViewCheckState Read FCheckState Write FCheckState;
      Property Expanded : Boolean Read FExpanded Write FExpanded;
      Property Image : Integer Read FImage Write FImage;
      Property Selected : Boolean Read FSelected Write FSelected;
      Property State : Integer Read FState Write FState;
      Property Styles : TFontStyles Read FStyles Write FStyles;
      Property Data : Pointer Read FData Write FData;
      Property Visible : Boolean Read FVisible Write FVisible;

      Property Enabled : Boolean Read GetEnabled Write SetEnabled;
      Property EnabledCheck : Boolean Read FEnabledCheck Write FEnabledCheck;
      Property EnabledText : Boolean Read FEnabledText Write FEnabledText;

      Property ShowCheckbox : Boolean Read GetShowCheckbox Write SetShowCheckbox;
      Property ShowRadioButton : Boolean Read GetShowRadioButton Write SetShowRadioButton;
      Property Checked : Boolean Read GetChecked Write SetChecked;
      Property Bolded : Boolean Read GetBolded Write SetBolded;
      Property Italicised : Boolean Read GetItalicised Write SetItalicised;
  End;

  TUixTreeViewNodes = Class(TFslObjectList)
    Private
      Function GetNode(Const iIndex: Integer): TUixTreeViewNode;
      Procedure SetNode(Const iIndex: Integer; Const Value: TUixTreeViewNode);

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TUixTreeViewNodes; Overload;

      Property Nodes[Const iIndex : Integer] : TUixTreeViewNode Read GetNode Write SetNode; Default;
  End;

  TUixTreeViewColumnAlignment = TAlignment;

  TUixTreeViewColumnIndex = TColumnIndex;

  TUixTreeViewColumnName = String;

  TUixTreeViewColumn = Class(TFslName)
    Private
      FAlignment : TUixTreeViewColumnAlignment;
      FAlwaysShow : Boolean;
      FBackground : TColor;
      FEnabled : Boolean;
      FFooter : String;
      FAlternateFooter : String;
      FPosition : Integer;
      FSortable : Boolean;
      FVisible : Boolean;
      FWidth : Integer;
      FColumn : TVirtualTreeColumn;

    Public
      constructor Create; Override;

      Procedure Assign(oObject : TFslObject); Override;

      Function Link : TUixTreeViewColumn; Overload;
      Function Clone : TUixTreeViewColumn; Overload;

      Procedure AlignLeft; Overload;
      Procedure AlignCenter; Overload;
      Procedure AlignRight; Overload;

      Procedure ApplyVisible(Const Value: Boolean); Overload;

      Property Alignment : TUixTreeViewColumnAlignment Read FAlignment Write FAlignment;
      Property AlwaysShow : Boolean Read FAlwaysShow Write FAlwaysShow;
      Property Background : TColor Read FBackground Write FBackground;
      Property Enabled : Boolean Read FEnabled Write FEnabled;
      Property Footer : String Read FFooter Write FFooter;
      Property AlternateFooter : String Read FAlternateFooter Write FAlternateFooter;
      Property Position : Integer Read FPosition Write FPosition;
      Property Sortable : Boolean Read FSortable Write FSortable;
      Property Visible : Boolean Read FVisible Write FVisible;
      Property Width : Integer Read FWidth Write FWidth;
      Property Column : TVirtualTreeColumn Read FColumn Write FColumn;
  End;

  TUixTreeViewColumnsClass = Class Of TUixTreeViewColumns;

  TUixTreeViewColumns = Class(TFslNameList)
    Private
      FAlwaysSortable : Boolean;
      FSortColumn     : String;

      Function GetColumn(Const iIndex: Integer): TUixTreeViewColumn;
      Procedure SetColumn(Const iIndex: Integer; Const Value: TUixTreeViewColumn);

      Function GetSortColumnIndex : TUixTreeViewColumnIndex;
      Procedure SetSortColumnIndex(Const Value : TUixTreeViewColumnIndex);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByPosition(pA, pB : Pointer) : Integer; Overload;

    Public
      constructor Create; Override;

      Function Link : TUixTreeViewColumns; Overload;

      Function Add(oValue : TFslObject) : Integer; Overload; Override;
      Function Add(Const sName : String) : TUixTreeViewColumn; Overload;

      Function Get(Const sName : String) : TUixTreeViewColumn; Reintroduce; Overload;
      Function Ensure(Const sName : String) : TUixTreeViewColumn; Overload;
      Function Force(Const sName : String) : TUixTreeViewColumn; Overload;

      Function IsSorted(Const sCaption : String) : Boolean; Overload;

      Procedure Unsortable;

      Procedure Hide; Overload;
      Procedure Show; Overload;

      Procedure ClearFooters;

      Function HasSortColumn : Boolean; Overload;

      Procedure SortedByPosition; Overload;

      Procedure Reposition(Const sCaption : String; iPosition : Integer); Overload;
      Procedure Move(Const sCaption : String; iPosition : Integer); Overload;

      Property AlwaysSortable : Boolean Read FAlwaysSortable Write FAlwaysSortable;
      Property SortColumn : String Read FSortColumn Write FSortColumn;
      Property SortColumnIndex : TUixTreeViewColumnIndex Read GetSortColumnIndex Write SetSortColumnIndex;
      Property Columns[Const iIndex : Integer] : TUixTreeViewColumn Read GetColumn Write SetColumn; Default;
  End;

  TUixTreeViewCheckedEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewCheckingEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; Var aNewState : TUixTreeViewCheckState; Var bAllowed : Boolean) Of Object;
  TUixTreeViewCollapsedEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewCollapsingEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; Var bCollapse : Boolean) Of Object;
  TUixTreeViewSingleClickEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewDoubleClickEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewExpandedEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewExpandingEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; Var bExpand : Boolean) Of Object;
  TUixTreeViewFocusChangeEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; iColumn : TUixTreeViewColumnIndex) Of Object;
  TUixTreeViewFocusChangingEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; iColumn : TUixTreeViewColumnIndex; Var bAllow : Boolean) Of Object;
  TUixTreeViewGetNodeEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewRenderNodeEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode) Of Object;
  TUixTreeViewPaintCellEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; iColumn : TUixTreeViewColumnIndex; oCanvas : TCanvas) Of Object;
  TUixTreeViewBeforeCellPaintEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; aCanvas : TCanvas; iColumn : TColumnIndex; Const aRect : TRect) Of Object;
  TUixTreeViewAfterCellPaintEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; aCanvas : TCanvas; iColumn : TColumnIndex; Const aRect : TRect) Of Object;
  TUixTreeViewBeforeNodePaintEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; oCanvas : TCanvas; Const aItemRect : TRect; Var bHandled : Boolean) Of Object;
  TUixTreeViewAfterNodePaintEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; oCanvas : TCanvas; Const aItemRect : TRect) Of Object;

  TUixTreeViewMultiSelectSelectionChange = Procedure (oSender : TObject) Of Object;
  TUixTreeViewSelectionChangeEvent = Procedure (oSender : TObject) Of Object;
  TUixTreeViewSortChangeEvent = Procedure (oSender : TObject; oColumn : TUixTreeViewColumn) Of Object;
  TUixTreeViewDragStartEvent = Procedure (oSender : TObject) Of Object;
  TUixTreeViewDragDropBeforeNodeEvent = Procedure (oSender : TObject; oSource : TObject; oTarget : TUixTreeViewNode) Of Object;
  TUixTreeViewDragDropNodeEvent = Procedure (oSender : TObject; oSource : TObject; oTarget : TUixTreeViewNode) Of Object;
  TUixTreeViewDragDropAfterNodeEvent = Procedure (oSender : TObject; oSource : TObject; oTarget : TUixTreeViewNode) Of Object;
  TUixTreeViewDragDropFileEvent = Procedure (oSender : TObject; Const sFilename : String; oTarget : TUixTreeViewNode) Of Object;
  TUixTreeViewDragAllowedEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; Var bAllowed : Boolean) Of Object;
  TUixTreeViewDragOverEvent = Procedure (oSender : TObject; oSource : TObject; oNode : TUixTreeViewNode; Var bAccept : Boolean) Of Object;
  TUixTreeViewDragFinishEvent = Procedure (oSender : TObject) Of Object;
  TUixTreeViewHorizontalScrollEvent = Procedure (oSender : TObject; Const iOffset : Integer) Of Object;
  TUixTreeViewVerticalScrollEvent = Procedure (oSender : TObject; Const iOffset : Integer) Of Object;
  TUixTreeViewColumnResizeEvent = Procedure (Const iColumn : Integer) Of Object;
  TUixTreeViewHeaderDraggedEvent = Procedure (Const iColumn, iOldPosition: Integer) Of Object;
  TUixTreeViewEditingEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; iColumn : TUixTreeViewColumnIndex; Var bAllowed : Boolean) Of Object;
  TUixTreeViewEditedEvent = Procedure (oSender : TObject; oNode : TUixTreeViewNode; iColumn : TUixTreeViewColumnIndex) Of Object;

  TUixTreeView = Class(TCustomVirtualStringTree)
    Private
      FRoot : TUixTreeViewNode;
      FColumns : TUixTreeViewColumns;
      FMatch : TFslStringObjectMatch;

      FFocusedID : TUixTreeViewIdentifier;
      FTopID : TUixTreeViewIdentifier;
      FExpandIdentifierList : TUixTreeViewIdentifiers;
      FSelectIdentifierList : TUixTreeViewIdentifiers;
      FReselectIdentifierList : TUixTreeViewIdentifiers;
      FMixedCheckIdentifierList : TUixTreeViewIdentifiers;
      FCheckIdentifierList : TUixTreeViewIdentifiers;
      FRenderIdentifierList : TUixTreeViewIdentifiers;

      FOnBeforeCellPaint : TUixTreeViewBeforeCellPaintEvent;
      FOnAfterCellPaint : TUixTreeViewAfterCellPaintEvent;
      FOnBeforeNodePaint : TUixTreeViewBeforeNodePaintEvent;
      FOnAfterNodePaint : TUixTreeViewAfterNodePaintEvent;
      FOnSingleClick : TUixTreeViewSingleClickEvent;
      FOnDoubleClick : TUixTreeViewDoubleClickEvent;
      FOnCollapsed : TUixTreeViewCollapsedEvent;
      FOnCollapsing : TUixTreeViewCollapsingEvent;
      FOnExpanded : TUixTreeViewExpandedEvent;
      FOnExpanding : TUixTreeViewExpandingEvent;
      FOnFocusChange : TUixTreeViewFocusChangeEvent;
      FOnFocusChanging : TUixTreeViewFocusChangingEvent;
      FOnGetNode : TUixTreeViewGetNodeEvent;
      FOnRenderNode : TUixTreeViewRenderNodeEvent;
      FOnSelectionChange : TUixTreeViewSelectionChangeEvent;
      FOnSortChange : TUixTreeViewSortChangeEvent;
      FOnChecked : TUixTreeViewCheckedEvent;
      FOnChecking : TUixTreeViewCheckingEvent;
      FOnDragAllowed : TUixTreeViewDragAllowedEvent;
      FOnDragDropBeforeNode : TUixTreeViewDragDropBeforeNodeEvent;
      FOnDragDropNode : TUixTreeViewDragDropNodeEvent;
      FOnDragDropAfterNode : TUixTreeViewDragDropAfterNodeEvent;
      FOnDragDropFile : TUixTreeViewDragDropFileEvent;
      FOnDragFinish : TUixTreeViewDragFinishEvent;
      FOnDragOver : TUixTreeViewDragOverEvent;
      FOnDragStart : TUixTreeViewDragStartEvent;
      FOnPaintCell : TUixTreeViewPaintCellEvent;
      FOnHorizontalScroll : TUixTreeViewHorizontalScrollEvent;
      FOnVerticalScroll : TUixTreeViewVerticalScrollEvent;
      FOnColumnResize : TUixTreeViewColumnResizeEvent;
      FOnHeaderDragged : TUixTreeViewHeaderDraggedEvent;
      FOnEdited : TUixTreeViewEditedEvent;
      FOnEditing : TUixTreeViewEditingEvent;

      FIsExpandAll : Boolean;
      FIsSelectAll : Boolean;
      FShowCheckBoxes : Boolean;
      FShowTriStateCheckBoxes : Boolean;

      FUseStandardSelectionColours : Boolean;

      Function GetOptions : TStringTreeOptions;
      Procedure SetOptions(Const Value : TStringTreeOptions);

      Function GetCount : Integer;
      Procedure SetCount(Const Value : Integer);

      Function GetHeaderVisible : Boolean;
      Procedure SetHeaderVisible(Const Value : Boolean);

      Function GetShowRoot : Boolean;
      Procedure SetShowRoot(Const Value : Boolean);

      Function GetSelectFullRow : Boolean;
      Procedure SetSelectFullRow(Const Value : Boolean);

      Function GetSelected : TUixTreeViewIdentifier;
      Procedure SetSelected(Const Value: TUixTreeViewIdentifier);

      Function GetExtendedFocus: Boolean;
      Procedure SetExtendedFocus(Const Value: Boolean);

      Function GetRightClickSelect: Boolean;
      Procedure SetRightClickSelect(Const Value: Boolean);

      Function GetAutoTriStateTracking: Boolean;
      Procedure SetAutoTriStateTracking(Const Value: Boolean);

      Function GetShowHeaderSortGlyphs: Boolean;
      Procedure SetShowHeaderSortGlyphs(Const Value: Boolean);

      Function GetSortColumn: TUixTreeViewColumnIndex;
      Procedure SetSortColumn(Const Value: TUixTreeViewColumnIndex);

      Function GetSortDirection: Integer;
      Procedure SetSortDirection(Const Value: Integer);

      Function GetMultiSelect: Boolean;
      Procedure SetMultiSelect(Const Value: Boolean);

      Function GetCheckSupport: Boolean;
      Procedure SetCheckSupport(Const Value: Boolean);

      Function GetAutoSizeColumn: TUixTreeViewColumnIndex;
      Procedure SetAutoSizeColumn(Const Value: TUixTreeViewColumnIndex);

      Function GetHeaderMenu: TUixTreeViewHeaderPopup;
      Procedure SetHeaderMenu(Const Value: TUixTreeViewHeaderPopup);

      Function GetImages: TUixImages;
      Procedure SetImages(Const Value: TUixImages);

      Function GetShowTreeLines: Boolean;
      Procedure SetShowTreeLines(Const Value: Boolean);

      Function GetTop: TUixTreeViewIdentifier;
      Procedure SetTop(Const Value: TUixTreeViewIdentifier);

      Function GetHotTrack : Boolean;
      Procedure SetHotTrack(Const Value : Boolean);

      Function GetShowButtons : Boolean;
      Procedure SetShowButtons(Const Value : Boolean);

      Function GetToggleDoubleClick: Boolean;
      Procedure SetToggleDoubleClick(Const Value: Boolean);

      Function GetRenderFocused : Boolean;
      Procedure SetRenderFocused(Const Value : Boolean);

      Function GetShowFocused : Boolean;
      Procedure SetShowFocused(Const Value : Boolean);

      Function GetShowSelected : Boolean;
      Procedure SetShowSelected(Const Value : Boolean);

      Function GetGridExtensions: Boolean;
      Procedure SetGridExtensions(Const Value: Boolean);

      Function GetShowEditable: Boolean;
      Procedure SetShowEditable(Const Value: Boolean);

      Function GetStateImages : TUixImages;
      Procedure SetStateImages(Const Value : TUixImages);

      Function GetFocused: TUixTreeViewIdentifier;
      Procedure SetFocused(Const Value: TUixTreeViewIdentifier);

      Function GetCentreScrollIntoView : Boolean;
      Procedure SetCentreScrollIntoView(Const Value : Boolean);

      function IsUpdating : boolean;
    function CanToggleNodes: Boolean;
    procedure ClearSelection;
    procedure MultiSelectRefresh;
    Protected
      Procedure Initialise; Overload; Virtual; // for TObeliskView
      Procedure Finalise; Overload;

      Function ColumnsClass : TUixTreeViewColumnsClass; Overload;

      Function IsDestroying : Boolean; Overload;

      Function GetCheckImage(Node: PVirtualNode; ImgCheckType: TCheckType = ctNone; ImgCheckState: TCheckState = csUncheckedNormal; ImgEnabled: Boolean = True): Integer; Override;
      Function GetOptionsClass : TTreeOptionsClass; Override;
      Function OptionsClass : TTreeOptionsClass; Overload;

      Procedure CMFontChanged(Var aMessage : TMessage); Message CM_FONTCHANGED;

      // Rendering and initialisation events.
      Procedure DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect); Override;
      Procedure DoAfterCellPaint(aCanvas : TCanvas; pNode : PVirtualNode; iColumn : TColumnIndex; aCellRect : TRect); Override;
      Procedure DoBeforeItemErase(aCanvas : TCanvas; pNode : PVirtualNode; aItemRect : TRect; Var aColor : TColor; Var aEraseAction : TItemEraseAction); Override;
      Procedure DoBeforePaint(aCanvas: TCanvas); Override;
      Function DoBeforeItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect): Boolean; Override;
      Procedure DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect); Override;
      Procedure DoChecked(pNode : PVirtualNode); Override;
      Function DoChecking(pNode : PVirtualNode; Var aNewCheckState : TCheckState) : Boolean; Override;
      Procedure DoCollapsed(pNode : PVirtualNode); Override;
      Function DoCollapsing(pNode : PVirtualNode) : Boolean; Override;
      Procedure DoColumnResize(iColumn : TColumnIndex); Override;
      Procedure DoExpanded(pNode : PVirtualNode); Override;
      Function DoExpanding(pNode : PVirtualNode) : Boolean; Override;
      Procedure DoFocusChange(pNode : PVirtualNode; iColumn : TColumnIndex); Override;
      Procedure DoSelectionChange; Overload;
      Function DoFocusChanging(pOldNode, pNewNode: PVirtualNode; iOldColumn, iNewColumn: TColumnIndex): Boolean; Override;
      Procedure DoFocusNode(pNode : PVirtualNode; bAsk : Boolean); Override;
      Function DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList; Override;
      procedure DoGetText(var pEventArgs: TVSTGetCellTextEventArgs); Override;
      Procedure DoHeaderClick(const HitInfo: TVTHeaderHitInfo); Override;
      function DoInitChildren(pNode: PVirtualNode; Var iChildCount: Cardinal) : boolean; Override;
      Procedure DoInitNode(pParent, pNode : PVirtualNode; Var aInitStates : TVirtualNodeInitStates); Override;
      Procedure DoPaintText(pNode : PVirtualNode; Const aCanvas : TCanvas; iColumn : TColumnIndex; aTextType : TVSTTextType); Override;
      Procedure DoHeaderDragged(iColumn : TColumnIndex; iOldPosition: TColumnPosition); Override;
      Procedure DoScroll(iDeltaX, iDeltaY : Integer); Override;
      Procedure DoHorizontalScroll(Const iOffset : Integer); Overload;
      Procedure DoVerticalScroll(Const iOffset : Integer); Overload;
      Procedure DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; Var Allowed: Boolean); Override;
      Procedure DoEdited(oSender : TBaseVirtualTree; Node: PVirtualNode; Column: TColumnIndex); Overload;

      Function DoBeforeDrag(pNode : PVirtualNode; iColumn : TColumnIndex) : Boolean; Override;
      Procedure DoStartDrag(Var oDragObject : TDragObject); Override;
      Procedure DoEndDrag(oTarget : TObject; iX, iY : Integer); Override;
      Function DoDragOver(oSource : TObject; aShift : TShiftState; aState : TDragState; aPoint : TPoint; aMode : TDropMode; Var iEffect : Integer): Boolean; Override;
      Procedure DoDragDrop(oSource : TObject; const oDataObject : IDataObject; const aFormats : TFormatArray; aShift : TShiftState; aPoint : TPoint; Var iEffect : Integer; aMode : TDropMode); Override;
      Procedure DoCheckClick(pNode : PVirtualNode; aNewCheckState : TCheckState); Override;

      Procedure DblClick; Override;

      // Mouse events.
      Procedure MouseDown(aButton: TMouseButton; aShift: TShiftState; iX, iY: Integer); Override;

      // Selection events.
      Procedure AddToSelection(pNode : PVirtualNode; NotifySynced: Boolean); Overload; Override;
      Procedure AddToSelection(Const aNewItems : TNodeArray; iNewLength: Integer; bForceInsert: Boolean = False); Overload; Override;
      Procedure InternalClearSelection; Override;
      Procedure InternalRemoveFromSelection(pNode : PVirtualNode); Override;
      Procedure RemoveFromSelection(pNode : PVirtualNode); Override;

      Procedure RemoveFromSelects(Const sID : String); Overload;

      Procedure RefreshNodes; Overload;

      Function IsExpanded(oNode : TUixTreeViewNode) : Boolean; Overload;
      Function IsSelected(oNode : TUixTreeViewNode) : Boolean; Overload;
      Function CheckState(oNode : TUixTreeViewNode) : TUixTreeViewCheckState; Overload;
      Function CheckType : TUixTreeViewCheckType; Overload;

      Procedure UpdateMatch(oNode : TUixTreeViewNode); Overload;

      Procedure Get(oNode: TUixTreeViewNode); Overload;
      Procedure Render(oNode: TUixTreeViewNode); Overload;
      Procedure RefreshNode(oNode: TUixTreeViewNode); Overload;
      Procedure RefreshState(oNode : TUixTreeViewNode); Overload;
      Procedure RefreshChildren(oNode : TUixTreeViewNode); Overload;
      Procedure RenderNode(oNode: TUixTreeViewNode); Overload;
      Procedure RefreshCheckState(oNode: TUixTreeViewNode); Overload;

      Function Render(pNode: PVirtualNode) : TUixTreeViewNode; Overload;
      Function Get(pNode : PVirtualNode) : TUixTreeViewNode; Overload;
      Function Prepare(pNode : PVirtualNode) : TUixTreeViewNode; Overload;
      Procedure Resolve(oNode : TUixTreeViewNode); Overload;

      Procedure ExpandNode(oNode : TUixTreeViewNode);

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Procedure RefreshColumns; Overload;
      Procedure Refresh; Overload;
      Procedure Render; Overload;
      Procedure Click; Override;
      Procedure Clear; Override;

      Function CellBackgroundByNode(Const oNode : TUixTreeViewNode) : TColor;

      Function Index : Integer; Overload;
      Function IsSelected : Boolean; Overload;

      Procedure SelectIdentifier(Const sID : TUixTreeViewIdentifier); Overload;
      Procedure SelectNone; Overload;

      Function GetByID(Const sID : TUixTreeViewIdentifier) : TUixTreeViewNode; Overload;
      Function SelectedData : Pointer; Overload;
      Function SelectedNode : TUixTreeViewNode; Overload;

      Procedure DragModeAutomatic; Overload;
      Procedure DragModeManual; Overload;

      Procedure AlignLeft; Overload;
      Procedure AlignRight; Overload;
      Procedure AlignTop; Overload;
      Procedure AlignBottom; Overload;
      Procedure AlignClient; Overload;

      Procedure SortAscending; Overload;
      Procedure SortDescending; Overload;

      Function IsSortAscending : Boolean; Overload;
      Function IsSortDescending : Boolean; Overload;

      Procedure SortChange(Const iColumnIndex : TUixTreeViewColumnIndex); Overload;

      Procedure ExpandAll;
      Procedure CollapseAll;

      Function ColumnClickIndex : TUixTreeViewColumnIndex; Overload;

      Property Columns : TUixTreeViewColumns Read FColumns;
      Property TreeOptions : TStringTreeOptions Read GetOptions Write SetOptions;

      Property MixedChecks : TUixTreeViewIdentifiers Read FMixedCheckIdentifierList;
      Property Checks : TUixTreeViewIdentifiers Read FCheckIdentifierList;
      Property Expands : TUixTreeViewIdentifiers Read FExpandIdentifierList;
      Property Selects : TUixTreeViewIdentifiers Read FSelectIdentifierList;

      Property Root : TUixTreeViewNode Read FRoot;
      Property SelectedID : TUixTreeViewIdentifier Read GetSelected Write SetSelected;
      Property FocusedID : TUixTreeViewIdentifier Read GetFocused Write SetFocused;
      Property TopID : TUixTreeViewIdentifier Read GetTop Write SetTop;
      Property Count : Integer Read GetCount Write SetCount;

    Published
      Property AutoSizeColumn : TUixTreeViewColumnIndex Read GetAutoSizeColumn Write SetAutoSizeColumn;
      Property AutoTriStateTracking : Boolean Read GetAutoTriStateTracking Write SetAutoTriStateTracking;
      Property CentreScrollIntoView : Boolean Read GetCentreScrollIntoView Write SetCentreScrollIntoView;
      Property CheckSupport : Boolean Read GetCheckSupport Write SetCheckSupport;
      Property HeaderMenu : TUixTreeViewHeaderPopup Read GetHeaderMenu Write SetHeaderMenu;
      Property HeaderVisible : Boolean Read GetHeaderVisible Write SetHeaderVisible;
      Property IsExpandAll : Boolean Read FIsExpandAll Write FIsExpandAll;
      Property IsSelectAll : Boolean Read FIsSelectAll Write FIsSelectAll;
      Property ExtendedFocus : Boolean Read GetExtendedFocus Write SetExtendedFocus;
      Property HotTrack : Boolean Read GetHotTrack Write SetHotTrack;
      Property Images : TUixImages Read GetImages Write SetImages;
      Property StateImages : TUixImages Read GetStateImages Write SetStateImages;
      Property MultiSelect : Boolean Read GetMultiSelect Write SetMultiSelect;
      Property RenderFocused : Boolean Read GetRenderFocused Write SetRenderFocused;
      Property RightClickSelect : Boolean Read GetRightClickSelect Write SetRightClickSelect;
      Property SelectFullRow : Boolean Read GetSelectFullRow Write SetSelectFullRow;
      Property ShowButtons : Boolean Read GetShowButtons Write SetShowButtons;
      Property ShowCheckboxes : Boolean Read FShowCheckBoxes Write FShowCheckboxes;
      Property ShowFocused : Boolean Read GetShowFocused Write SetShowFocused;
      Property ShowHeaderSortGlyphs : Boolean Read GetShowHeaderSortGlyphs Write SetShowHeaderSortGlyphs;
      Property ShowRoot : Boolean Read GetShowRoot Write SetShowRoot;
      Property ShowSelected : Boolean Read GetShowSelected Write SetShowSelected;
      Property ShowTreeLines : Boolean Read GetShowTreeLines Write SetShowTreeLines;
      Property ShowTriStateCheckBoxes : Boolean Read FShowTriStateCheckBoxes Write FShowTriStateCheckBoxes;
      Property GridExtensions : Boolean Read GetGridExtensions Write SetGridExtensions;
      Property ShowEditable : Boolean Read GetShowEditable Write SetShowEditable;
      Property SortColumn : TUixTreeViewColumnIndex Read GetSortColumn Write SetSortColumn;
      Property SortDirection : Integer Read GetSortDirection Write SetSortDirection;
      Property ToggleDoubleClick : Boolean Read GetToggleDoubleClick Write SetToggleDoubleClick;

      Property Action;
      Property Align;
      Property Alignment;
      Property Anchors;
      Property BiDiMode;
      Property BevelEdges;
      Property BevelInner;
      Property BevelOuter;
      Property BevelKind;
      Property BevelWidth;
      Property BorderStyle;
      Property BorderWidth;
      Property CheckImageKind;
      Property Color;
      Property Colors;
      Property Constraints;
      Property Ctl3D;
      Property CustomCheckImages;
      Property DefaultNodeHeight;
      Property DragCursor;
      Property DragHeight;
      Property DragKind;
      Property DragImageKind;
      Property DragMode;
      Property DragOperations;
      Property DragType;
      Property DragWidth;
      Property DrawSelectionMode;
      Property Enabled;
      Property Font;
      Property HotCursor;
      Property Indent;
      Property ParentBiDiMode;
      Property ParentColor Default False;
      Property ParentCtl3D;
      Property ParentFont;
      Property ParentShowHint;
      Property PopupMenu;
      Property ShowHint;
      Property TabOrder;
      Property TabStop Default True;
      Property Visible;
      Property WantTabs;

      Property OnSingleClick : TUixTreeViewSingleClickEvent Read FOnSingleClick Write FOnSingleClick;
      Property OnDoubleClick : TUixTreeViewDoubleClickEvent Read FOnDoubleClick Write FOnDoubleClick;
      Property OnChecked : TUixTreeViewCheckedEvent Read FOnChecked Write FOnChecked;
      Property OnChecking : TUixTreeViewCheckingEvent Read FOnChecking Write FOnChecking;
      Property OnCollapsed : TUixTreeViewCollapsedEvent Read FOnCollapsed Write FOnCollapsed;
      Property OnCollapsing : TUixTreeViewCollapsingEvent Read FOnCollapsing Write FOnCollapsing;
      Property OnExpanded : TUixTreeViewExpandedEvent Read FOnExpanded Write FOnExpanded;
      Property OnExpanding : TUixTreeViewExpandingEvent Read FOnExpanding Write FOnExpanding;

      Property OnDragStart : TUixTreeViewDragStartEvent Read FOnDragStart Write FOnDragStart;
      Property OnDragFinish : TUixTreeViewDragFinishEvent Read FOnDragFinish Write FOnDragFinish;
      Property OnDragAllowed : TUixTreeViewDragAllowedEvent Read FOnDragAllowed Write FOnDragAllowed;
      Property OnDragOver : TUixTreeViewDragOverEvent Read FOnDragOver Write FOnDragOver;
      Property OnDragDropBeforeNode : TUixTreeViewDragDropBeforeNodeEvent Read FOnDragDropBeforeNode Write FOnDragDropBeforeNode;
      Property OnDragDropNode : TUixTreeViewDragDropNodeEvent Read FOnDragDropNode Write FOnDragDropNode;
      Property OnDragDropAfterNode : TUixTreeViewDragDropAfterNodeEvent Read FOnDragDropAfterNode Write FOnDragDropAfterNode;
      Property OnDragDropFile : TUixTreeViewDragDropFileEvent Read FOnDragDropFile Write FOnDragDropFile;

      Property OnFocusChange : TUixTreeViewFocusChangeEvent Read FOnFocusChange Write FOnFocusChange;
      Property OnFocusChanging : TUixTreeViewFocusChangingEvent Read FOnFocusChanging Write FOnFocusChanging;
      Property OnSelectionChange : TUixTreeViewSelectionChangeEvent Read FOnSelectionChange Write FOnSelectionChange;

      Property OnBeforeNodePaint : TUixTreeViewBeforeNodePaintEvent Read FOnBeforeNodePaint Write FOnBeforeNodePaint;
      Property OnAfterNodePaint : TUixTreeViewAfterNodePaintEvent Read FOnAfterNodePaint Write FOnAfterNodePaint;
      Property OnBeforeCellPaint : TUixTreeViewBeforeCellPaintEvent Read FOnBeforeCellPaint Write FOnBeforeCellPaint;
      Property OnAfterCellPaint : TUixTreeViewAfterCellPaintEvent Read FOnAfterCellPaint Write FOnAfterCellPaint;
      Property OnPaintCell : TUixTreeViewPaintCellEvent Read FOnPaintCell Write FOnPaintCell;

      Property OnGetNode : TUixTreeViewGetNodeEvent Read FOnGetNode Write FOnGetNode;
      Property OnRenderNode : TUixTreeViewRenderNodeEvent Read FOnRenderNode Write FOnRenderNode;
      Property OnSortChange : TUixTreeViewSortChangeEvent Read FOnSortChange Write FOnSortChange;

      Property OnEditing : TUixTreeViewEditingEvent Read FOnEditing Write FOnEditing;
      Property OnEdited : TUixTreeViewEditedEvent Read FOnEdited Write FOnEdited;

      Property OnHorizontalScroll : TUixTreeViewHorizontalScrollEvent Read FOnHorizontalScroll Write FOnHorizontalScroll;
      Property OnVerticalScroll : TUixTreeViewVerticalScrollEvent Read FOnVerticalScroll Write FOnVerticalScroll;
      Property OnColumnResize : TUixTreeViewColumnResizeEvent Read FOnColumnResize Write FOnColumnResize;
      Property OnHeaderDragged : TUixTreeViewHeaderDraggedEvent Read FOnHeaderDragged Write FOnHeaderDragged;

      Property UseStandardSelectionColours : Boolean Read FUseStandardSelectionColours Write FUseStandardSelectionColours;

      Property OnEnter;
      Property OnExit;
      Property OnKeyPress;
      Property IncrementalSearch;
      Property OnIncrementalSearch;
  End;



Procedure Register;

Implementation

{$R Resources\UixBalloons.res}
{$R Resources\UixToolbars.dcr}

uses
  fsl_shell;

Procedure Register;
Begin
  RegisterComponents('Uix', [TUixLabel]);
  RegisterComponents('Uix', [TUixPanel]);
  RegisterComponents('Uix', [TUixButton]);
  RegisterComponents('Uix', [TUixTimer]);
  RegisterComponents('Uix', [TUixCodeEdit]);
  RegisterComponents('Uix', [TUixComboBox]);
  RegisterComponents('Uix', [TUixToolBar, TUixToolButton]);
  RegisterComponents('Uix', [TUixPopupMenu, TUixMenuItem]);
  RegisterComponents('Uix', [TUixGroupBox]);
  RegisterComponents('Uix', [TUixEdit]);
  RegisterComponents('Uix', [TUixCheckBox]);
  RegisterComponents('Uix', [TUixPageControl]);
  RegisterComponents('Uix', [TUixTabsheet]);
  RegisterComponents('Uix', [TUixRadioButton]);
  RegisterComponents('Uix', [TUixScrollbox]);
  RegisterComponents('Uix', [TUixSplitter]);
  RegisterComponents('Uix', [TUixMemo]);
  RegisterComponents('Uix', [TUixTreeView]);
  RegisterComponents('Uix', [TUixTreeViewHeaderPopup]);
End;


Constructor TUixLabel.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  ParentColor := True;
  Transparent := True;

End;


Destructor TUixLabel.Destroy;
Begin

  Inherited;
End;


Procedure TUixLabel.Paint;

// From StdCtrls.TCustomLabel.Paint.

Const
  aAlignmentArray : Array [TAlignment] Of Word = (DT_LEFT, DT_RIGHT, DT_CENTER);
  aWordWrapArray : Array [Boolean] Of Word = (0, DT_WORDBREAK);
Var
  aRect : TRect;
  aCalcRect : TRect;
  iDrawStyle : LongInt;
Begin
  If Not Transparent Then
  Begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Brush.Style := bsSolid;
    Canvas.FillRect(ClientRect);
  End;

  Canvas.Brush.Style := bsClear;

  aRect.Left := ClientRect.Left + FLeftBorderWidth;
  aRect.Right := ClientRect.Right - FRightBorderWidth;
  aRect.Top := ClientRect.Top + FTopBorderWidth;
  aRect.Bottom := ClientRect.Bottom - FBottomBorderWidth;

  iDrawStyle := DT_EXPANDTABS Or aWordWrapArray[WordWrap] Or aAlignmentArray[Alignment];

  If Layout <> tlTop Then
  Begin
    aCalcRect := aRect;

    DoDrawText(aCalcRect, iDrawStyle Or DT_CALCRECT);

    If Layout = tlBottom Then
      OffsetRect(aRect, 0, Height - aCalcRect.Bottom)
    Else
      OffsetRect(aRect, 0, (Height - aCalcRect.Bottom) Div 2);
  End;

  DoDrawText(aRect, iDrawStyle);
End;


Procedure TUixLabel.AdjustBounds;

// From StdCtrls.TCustomLabel.AdjustBounds.

Const
  aWordWrapArray : Array [Boolean] Of Word = (0, DT_WORDBREAK);
Var
  aDC : HDC;
  aRect : TRect;
  aAlignment : TAlignment;

  iX : Integer;
Begin
  If Not (csReading In ComponentState) And AutoSize Then
  Begin
    aRect.Left := ClientRect.Left + FLeftBorderWidth;
    aRect.Right := ClientRect.Right - FRightBorderWidth;
    aRect.Top := ClientRect.Top + FTopBorderWidth;
    aRect.Bottom := ClientRect.Bottom - FBottomBorderWidth;

    aDC := GetDC(0);
    Canvas.Handle := aDC;
    DoDrawText(aRect, (DT_EXPANDTABS Or DT_CALCRECT) Or aWordWrapArray[WordWrap]);
    Canvas.Handle := 0;
    ReleaseDC(0, aDC);

    iX := Left;

    aAlignment := Alignment;

    If UseRightToLeftAlignment Then
      ChangeBiDiModeAlignment(aAlignment);

    If aAlignment = taRightJustify Then
      Inc(iX, Width - aRect.Right);

    aRect.Left := aRect.Left - FLeftBorderWidth;
    aRect.Right := aRect.Right + FRightBorderWidth;
    aRect.Top := aRect.Top - FTopBorderWidth;
    aRect.Bottom := aRect.Bottom + FBottomBorderWidth;

    SetBounds(iX, Top, aRect.Right, aRect.Bottom);
  End;
End;


Procedure TUixLabel.Adjust;
Begin
  AdjustBounds;
End;


Function TUixLabels.GetLabel(iIndex: Integer): TUixLabel;
Begin
  Result := TUixLabel(Items[iIndex]);
End;


Function TUixLabel.GetBolded : Boolean;
Begin
  Result := fsBold In Font.Style;
End;


Procedure TUixLabel.SetBolded(Const Value : Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsBold]
  Else
    Font.Style := Font.Style - [fsBold];
End;


Function TUixLabel.GetItalicised : Boolean;
Begin
  Result := fsItalic In Font.Style;
End;


Procedure TUixLabel.SetItalicised(Const Value : Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsItalic]
  Else
    Font.Style := Font.Style - [fsItalic];
End;


Function TUixLabel.GetUnderlined : Boolean;
Begin
  Result := fsUnderline In Font.Style;
End;


Procedure TUixLabel.SetUnderlined(Const Value : Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsUnderline]
  Else
    Font.Style := Font.Style - [fsUnderline];
End;


Procedure TUixLabel.HorizontalAlignCenter;
Begin
  Alignment := taCenter;
End;


Procedure TUixLabel.HorizontalAlignLeft;
Begin
  Alignment := taLeftJustify;
End;


Procedure TUixLabel.HorizontalAlignRight;
Begin
  Alignment := taRightJustify;
End;


Procedure TUixLabel.VerticalAlignCenter;
Begin
  Layout := tlCenter;
End;


Procedure TUixLabel.VerticalAlignTop;
Begin
  Layout := tlTop;
End;


Procedure TUixLabel.VerticalAlignBottom;
Begin
  Layout := tlBottom;
End;


Procedure TUixLabel.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixLabel.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixLabel.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixLabel.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixLabel.AlignTop;
Begin
  Align := alTop;
End;


Function TUixLabel.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Procedure TUixLabel.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Function TUixLabel.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Procedure TUixLabel.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Function TUixLabel.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Procedure TUixLabel.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Function TUixLabel.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixLabel.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixLabel.ShuffleTop;
Begin
  Top := 0;
End;


Procedure TUixLabel.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixLabel.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixLabel.ShuffleRight;
Begin
  Left := MaxInt;
End;


Function TUixLabel.GetBorderWidth : Integer;
Begin
  Assert((FLeftBorderWidth = FRightBorderWidth) And (FRightBorderWidth = FTopBorderWidth) And (FTopBorderWidth = FBottomBorderWidth), 'Border widths are different.');

  Result := FLeftBorderWidth;
End;


Procedure TUixLabel.SetBorderWidth(Const Value : Integer);
Begin
  FLeftBorderWidth := Value;
  FRightBorderWidth := Value;
  FTopBorderWidth := Value;
  FBottomBorderWidth := Value;

  AdjustBounds;
End;


Function TUixLabel.GetLeftBorderWidth : Integer;
Begin
  Result := FLeftBorderWidth;
End;


Procedure TUixLabel.SetLeftBorderWidth(Const Value : Integer);
Begin
  If FLeftBorderWidth <> Value Then
  Begin
    FLeftBorderWidth := Value;

    AdjustBounds;
  End;
End;


Function TUixLabel.GetRightBorderWidth : Integer;
Begin
  Result := FRightBorderWidth;
End;


Procedure TUixLabel.SetRightBorderWidth(Const Value : Integer);
Begin
  If FRightBorderWidth <> Value Then
  Begin
    FRightBorderWidth := Value;

    AdjustBounds;
  End;
End;


Function TUixLabel.GetTopBorderWidth : Integer;
Begin
  Result := FTopBorderWidth;
End;


Procedure TUixLabel.SetTopBorderWidth(Const Value : Integer);
Begin
  If FTopBorderWidth <> Value Then
  Begin
    FTopBorderWidth := Value;

    AdjustBounds;
  End;
End;


Function TUixLabel.GetBottomBorderWidth : Integer;
Begin
  Result := FBottomBorderWidth;
End;


Procedure TUixLabel.SetBottomBorderWidth(Const Value : Integer);
Begin
  If Value <> FBottomBorderWidth Then
  Begin
    FBottomBorderWidth := Value;

    AdjustBounds;
  End;
End;


Procedure TUixLabel.Resize;
Begin
  Inherited;

  If AdjustBoundsOnResize Then
    AdjustBounds;
End;


Function TUixLabel.RequiredHeight : Integer;
Const
  aWordWrapArray : Array [Boolean] Of Word = (0, DT_WORDBREAK);
Var
  aDC : HDC;
  aRect : TRect;
Begin
  aRect.Left := ClientRect.Left + FLeftBorderWidth;
  aRect.Right := ClientRect.Right - FRightBorderWidth;
  aRect.Top := ClientRect.Top + FTopBorderWidth;
  aRect.Bottom := ClientRect.Bottom - FBottomBorderWidth;

  aDC := GetDC(0);
  Canvas.Handle := aDC;
  DoDrawText(aRect, (DT_EXPANDTABS Or DT_CALCRECT) Or aWordWrapArray[WordWrap]);
  Canvas.Handle := 0;
  ReleaseDC(0, aDC);

  aRect.Top := aRect.Top - FTopBorderWidth;
  aRect.Bottom := aRect.Bottom + FBottomBorderWidth;

  Result := aRect.Bottom - aRect.Top;
End;



Function TUixComponentList.Invariant(Const sMethod, sMessage : String) : Boolean;
Begin
  Raise EFslInvariant.Create(Self, sMethod, sMessage); // Can't use Error method here as it is virtual.

  Result := True;
End;


Function TUixComponentList.Invariants(Const sLocation : String; oObject : TObject; aClass : TClass; Const sObject : String) : Boolean;
Begin
  If Not Assigned(aClass) Then
    Invariant('Invariants', 'aClass was not assigned.');

  // Ensure object is assigned.
  If Not Assigned(oObject) Then
    Invariant(sLocation, sObject + ' was not assigned and was expected to have been of class ' + aClass.ClassName);

  // Ensure object is of the expected class.
  If Not oObject.InheritsFrom(aClass) Then
    Invariant(sLocation, sObject + ' was of class ' + oObject.ClassName + ' and should been of class ' + aClass.ClassName);

  Result := True;
End;


Function TUixComponentList.ItemClass: TComponentClass;
Begin
  Result := TComponent;
End;


Procedure TUixComponentList.Notify(Ptr: Pointer; Action: TListNotification);
Begin
  // This is not a trusted method call (seems to fire with invalid ptrs)
//Assert(Not Assigned(Ptr) Or Invariants('Notify', TObject(Ptr), ItemClass, 'Ptr'));

  Inherited;
End;


Constructor TUixPanel.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

End;


Destructor TUixPanel.Destroy;
Begin

  Inherited;
End;


Procedure TUixPanel.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixPanel.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixPanel.Initialise;
Begin
  Alignment := taLeftJustify;
  BevelOuter := bvNone;
  BevelInner := bvNone;
  ParentColor := True;
  ShowHint := True;
End;


Procedure TUixPanel.Finalise;
Begin
End;


Procedure TUixPanel.Paint;
Var
  oGraphics : TGdiPlusExtendedGraphics;
  oSolidBrush : TGPSolidBrush;
  oGradientBrush : TGPLinearGradientBrush;
  aRoundedRect : TGPRect;
  aClientRect : TGPRect;
  oPen : TGPPen;
Begin
  If Assigned(FOnPaint) Then
  Begin
    FOnPaint(Self);
  End
  Else If RoundedEdges Then
  Begin
    aClientRect := VCLRectToGDIPlusRect(ClientRect);
    aRoundedRect := ContractRectangle(aClientRect, BorderWidth Div 2);
    aRoundedRect.Width := aRoundedRect.Width - 1;
    aRoundedRect.Height := aRoundedRect.Height - 1;

    oGraphics := TGdiPlusExtendedGraphics.Create(Canvas.Handle);
    oSolidBrush := TGPSolidBrush.Create(argbBlack);
    oPen := TGPPen.Create(argbBlack);
    Try
      oSolidBrush.SetColor($FFA3A3A3);
      oGraphics.FillRectangle(oSolidBrush, aClientRect);

      oGraphics.SetSmoothingMode(SmoothingModeAntiAlias);
      oSolidBrush.SetColor(VCLColourToGDIPlusColour(Color));
      oGraphics.FillRoundedRectangle(oSolidBrush, aRoundedRect, 3);

      If (GradientType <> UixPanelGradientTypeNone) Then
      Begin
        oGradientBrush := TGPLinearGradientBrush.Create(aClientRect, VCLColourToGDIPlusColour(Color), VCLColourToGDIPlusColour(FGradientColour), LinearGradientModeVertical);
        Try
          oGraphics.FillRoundedRectangle(oGradientBrush, aRoundedRect, 3);
        Finally
          oGradientBrush.Free;
        End;
      End;

      oPen.SetColor(argbHalfTransparentWhite);
      oGraphics.DrawRoundedRectangle(oPen, aRoundedRect, 3);
    Finally
      oPen.Free;
      oSolidBrush.Free;
      oGraphics.Free;
    End;
  End
  Else If GradientType <> UixPanelGradientTypeNone Then
  Begin
    Case GradientType Of
      UixPanelGradientTypeVertical : ColourGradientVertical(Canvas.Handle, ClientRect, Color, FGradientColour);
      UixPanelGradientTypeHorizontal : ColourGradientHorizontal(Canvas.Handle, ClientRect, Color, FGradientColour);
    End;

    If FGradientDrawWhiteEdges Then
    Begin
      Canvas.Pen.Color := clWhite;
      Canvas.PenPos := Point(0, 0);
      Canvas.LineTo(ClientWidth, 0);

      Canvas.PenPos := Point(0, 0);
      Canvas.LineTo(0, ClientHeight);
    End;
  End
  Else
  Begin
    Inherited;
  End;
End;


Function TUixPanelList.GetPanelByIndex(Const iIndex : Integer) : TUixPanel;
Begin
  Result := TUixPanel(Items[iIndex]);
End;


Function TUixPanel.RequiredHeight : Integer;
Begin
  Result := VisibleControlHeight + NonClientHeight;
End;


Function TUixPanel.VisibleControlHeight : Integer;
Var
  iLoop    : Integer;
  oControl : TControl;
Begin
  Result := 0;

  For iLoop := 0 To ControlCount - 1 Do
  Begin
    oControl := Controls[iLoop];

    If oControl.Visible And (oControl.Align In [alTop, alBottom, alClient]) Then
      Inc(Result, oControl.Height);
  End;
End;


Procedure TUixPanel.Reset;
Begin
  Height := RequiredHeight;
End;


Procedure TUixPanel.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixPanel.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixPanel.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixPanel.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixPanel.AlignTop;
Begin
  Align := alTop;
End;


Function TUixPanel.IsAlignedBottom : Boolean;
Begin
  Result := Align = alBottom;
End;


Function TUixPanel.IsAlignedClient : Boolean;
Begin
  Result := Align = alClient;
End;


Function TUixPanel.IsAlignedLeft : Boolean;
Begin
  Result := Align = alLeft;
End;


Function TUixPanel.IsAlignedRight : Boolean;
Begin
  Result := Align = alRight;
End;


Function TUixPanel.IsAlignedTop : Boolean;
Begin
  Result := Align = alTop;
End;


Function TUixPanel.GetBolded: Boolean;
Begin
  Result := fsBold In Font.Style;
End;


Procedure TUixPanel.SetBolded(Const Value: Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsBold]
  Else
    Font.Style := Font.Style - [fsBold];
End;


Procedure TUixPanel.HorizontalAlignCenter;
Begin
  Alignment := taCenter;
End;


Procedure TUixPanel.HorizontalAlignLeft;
Begin
  Alignment := taLeftJustify;
End;


Procedure TUixPanel.HorizontalAlignRight;
Begin
  Alignment := taRightJustify;
End;


Procedure TUixPanel.BevelInnerNone;
Begin
  BevelInner := bvNone;
End;


Procedure TUixPanel.BevelInnerLowered;
Begin
  BevelInner := bvLowered;
End;


Procedure TUixPanel.BevelInnerRaised;
Begin
  BevelInner := bvRaised;
End;


Function TUixPanel.IsBevelInnerNone : Boolean;
Begin
  Result := BevelInner = bvNone;
End;


Function TUixPanel.IsBevelInnerLowered : Boolean;
Begin
  Result := BevelInner = bvLowered;
End;


Function TUixPanel.IsBevelInnerRaised : Boolean;
Begin
  Result := BevelInner = bvRaised;
End;


Procedure TUixPanel.BevelOuterNone;
Begin
  BevelOuter := bvNone;
End;


Procedure TUixPanel.BevelOuterRaised;
Begin
  BevelOuter := bvRaised;
End;


Procedure TUixPanel.BevelOuterLowered;
Begin
  BevelOuter := bvLowered;
End;


Function TUixPanel.IsBevelOuterNone : Boolean;
Begin
  Result := BevelInner = bvNone;
End;


Function TUixPanel.IsBevelOuterLowered : Boolean;
Begin
  Result := BevelInner = bvLowered;
End;


Function TUixPanel.IsBevelOuterRaised : Boolean;
Begin
  Result := BevelInner = bvRaised;
End;


Procedure TUixPanel.GradientTypeHorizontal;
Begin
  GradientType := UixPanelGradientTypeHorizontal;
End;


Procedure TUixPanel.GradientTypeNone;
Begin
  GradientType := UixPanelGradientTypeNone;
End;


Procedure TUixPanel.GradientTypeVertical;
Begin
  GradientType := UixPanelGradientTypeVertical;
End;


Function TUixPanel.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Procedure TUixPanel.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Function TUixPanel.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Procedure TUixPanel.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Function TUixPanel.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Procedure TUixPanel.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Function TUixPanel.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixPanel.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixPanel.BorderNone;
Begin
  BorderStyle := bsNone;
End;


Procedure TUixPanel.BorderSingle;
Begin
  BorderStyle := bsSingle;
End;


Function TUixPanel.IsBorderNone : Boolean;
Begin
  Result := BorderStyle = bsNone;
End;


Function TUixPanel.IsBorderSingle : Boolean;
Begin
  Result := BorderStyle = bsSingle;
End;


Procedure TUixPanel.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixPanel.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixPanel.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixPanel.ShuffleTop;
Begin
  Top := 0;
End;


Function TUixPanelList.ItemClass : TComponentClass;
Begin
  Result := TUixPanel;
End;


Function TUixPanel.NonClientHeight : Integer;
Begin
  Result := (2 * BorderWidth);

  If Not IsBevelInnerNone Then
    Inc(Result, 2 * BevelWidth);

  If Not IsBevelOuterNone Then
    Inc(Result, 2 * BevelWidth);

  If IsBorderSingle Then
    Inc(Result, 2);
End;


Procedure TUixPanel.CreateParams(Var aParams : TCreateParams);
Begin
  Inherited;

  If FTransparent Then
    aParams.ExStyle := aParams.ExStyle Or WS_EX_TRANSPARENT;
End;


Procedure TUixPanel.WMEraseBkgnd(Var aMessage : TWmEraseBkgnd);
Begin
  If FTransparent Then
    aMessage.Result := 1
  Else If Not Assigned(FOnPaint) And (GradientType = UixPanelGradientTypeNone) Then
    Inherited;
End;


Function TUixPanel.GetTransparent : Boolean;
Begin
  Result := FTransparent;
End;


Procedure TUixPanel.SetTransparent(Const Value : Boolean);
Begin
  If Value <> FTransparent Then
  Begin
    FTransparent := Value;

    RecreateWnd;
  End;
End;


Procedure TUixPanel.Adjust;
Begin
  AdjustSize;
End;


Procedure TUixPanel.CMMouseEnter(Var aMessage : TMessage);
Var
  aClientOrigin : TPoint;
Begin
  If Assigned(FOnHoverEnter) Then
  Begin
    aClientOrigin := ClientOrigin;

    FOnHoverEnter(Self, aClientOrigin.X, aClientOrigin.Y);
  End;
End;


Procedure TUixPanel.CMMouseLeave(Var aMessage: TMessage);
Begin
  If Assigned(FOnHoverExit) Then
    FOnHoverExit(Self);
End;


Constructor TUixButton.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Destructor TUixButton.Destroy;
Begin
  Inherited;
End;


Procedure TUixButton.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixButton.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixButton.Initialise;
Begin
  Height := 23;
  Width := 72;
End;


Procedure TUixButton.Finalise;
Begin
End;


Function TUixButton.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixButton.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixButton.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixButton.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixButton.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixButton.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixButton.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixButton.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixButton.ModalCancel;
Begin
  ModalResult := mrCancel;
End;


Procedure TUixButton.ModalNone;
Begin
  ModalResult := mrNone;
End;


Procedure TUixButton.ModalOK;
Begin
  ModalResult := mrOK;
End;


Procedure TUixButton.ModalAbort;
Begin
  ModalResult := mrAbort;
End;


Procedure TUixButton.ModalIgnore;
Begin
  ModalResult := mrIgnore;
End;


Procedure TUixButton.ModalNo;
Begin
  ModalResult := mrNo;
End;


Procedure TUixButton.ModalRetry;
Begin
  ModalResult := mrRetry;
End;


Procedure TUixButton.ModalYes;
Begin
  ModalResult := mrYes;
End;


Function TUixButtons.GetButton(iIndex: Integer): TUixButton;
Begin
  Result := TUixButton(Items[iIndex]);
End;


Procedure TUixButton.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixButton.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixButton.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixButton.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixButton.AlignTop;
Begin
  Align := alTop;
End;


Function TUixButton.GetBolded : Boolean;
Begin
  Result := fsBold In Font.Style;
End;


Procedure TUixButton.SetBolded(Const Value : Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsBold]
  Else
    Font.Style := Font.Style - [fsBold];
End;


Function TUixButton.IsModalAbort : Boolean;
Begin
  Result := ModalResult = mrAbort;
End;


Function TUixButton.IsModalCancel : Boolean;
Begin
  Result := ModalResult = mrCancel;
End;


Function TUixButton.IsModalIgnore : Boolean;
Begin
  Result := ModalResult = mrIgnore;
End;


Function TUixButton.IsModalNo : Boolean;
Begin
  Result := ModalResult = mrNo;
End;


Function TUixButton.IsModalNone : Boolean;
Begin
  Result := ModalResult = mrNone;
End;


Function TUixButton.IsModalOK : Boolean;
Begin
  Result := ModalResult = mrOK;
End;


Function TUixButton.IsModalRetry : Boolean;
Begin
  Result := ModalResult = mrRetry;
End;


Function TUixButton.IsModalYes : Boolean;
Begin
  Result := ModalResult = mrYes;
End;


Procedure TUixButton.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixButton.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixButton.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixButton.ShuffleTop;
Begin
  Top := 0;
End;

Const
  UixBalloonTypeResourceNameArray : Array [TUixBalloonType] Of String = ('HELP', 'INFORMATION', 'ERROR', 'WARNING');

  DefaultBufferInteger = 8;


Constructor TUixBalloon.CreateNew(oOwner : TComponent; iDummy : Integer);
Begin
  Inherited;

  BorderStyle := bsNone;
  FormStyle := fsStayOnTop;
  Color := clInfoBk;

  Font.Name := 'Tahoma';
  Font.Size := 9;

  AnchorPosition := UixBalloonAnchorPositionTopLeft;
  ControlRelativeHorizontalPosition := UixBalloonControlRelativeHorizontalPositionRight;
  ControlRelativeVerticalPosition := UixBalloonControlRelativeVerticalPositionBottom;

  OnClick := ClickDelegate;
  OnDeactivate := ReleaseDelegate;
  OnPaint := PaintDelegate;

  FClientPanel := TPanel.Create(Self);
  FClientPanel.Parent := Self;
  FClientPanel.ParentColor := True;
  FClientPanel.ParentFont := True;
  FClientPanel.BorderStyle := bsNone;
  FClientPanel.BevelOuter := bvNone;
  FClientPanel.BevelInner := bvNone;
  FClientPanel.Left := DefaultBufferInteger;

  FHeaderImage := TUixImage.Create(Self);
  FHeaderImage.Transparent := True;
  FHeaderImage.OnClick := ClickDelegate;
  FHeaderImage.Left := 12;

  FHeaderLabel := TLabel.Create(Self);
  FHeaderLabel.OnClick := ClickDelegate;
  FHeaderLabel.Parent := Self;
  FHeaderLabel.ParentColor := True;
  FHeaderLabel.ParentFont := True;
  FHeaderLabel.AutoSize := True;
  FHeaderLabel.Font.Style := [fsBold];
  FHeaderLabel.Left := 34;

  FMessageLabel := TLabel.Create(Self);
  FMessageLabel.OnClick := ClickDelegate;
  FMessageLabel.Parent := Self;
  FMessageLabel.ParentColor := True;
  FMessageLabel.ParentFont := True;
  FMessageLabel.AutoSize := True;
  FMessageLabel.Left := DefaultBufferInteger;

  FShowTimer := TUixTimer.Create(Self);
  FShowTimer.Enabled := False;
  FShowTimer.Interval := 0;
  FShowTimer.OnTimer := ReleaseDelegate;
End;


Procedure TUixBalloon.CreateParams(Var Params : TCreateParams);
Const
  CS_DROPSHADOW = $00020000;
Begin
  Inherited;

  Params.Style := (Params.Style And Not WS_CAPTION) Or WS_POPUP;
  Params.ExStyle := Params.ExStyle Or WS_EX_TOOLWINDOW;
  Params.WndParent := GetDesktopWindow;

  If SupportsShadows Then
    Params.WindowClass.Style := Params.WindowClass.Style Or CS_DROPSHADOW;
End;


Procedure TUixBalloon.AnchorPositionBottomLeft;
Begin
  FAnchorPosition := UixBalloonAnchorPositionBottomLeft;
End;


Procedure TUixBalloon.AnchorPositionBottomRight;
Begin
  FAnchorPosition := UixBalloonAnchorPositionBottomRight;
End;


Procedure TUixBalloon.AnchorPositionTopLeft;
Begin
  FAnchorPosition := UixBalloonAnchorPositionTopLeft;
End;


Procedure TUixBalloon.AnchorPositionTopRight;
Begin
  FAnchorPosition := UixBalloonAnchorPositionTopRight;
End;


Procedure TUixBalloon.BalloonTypeError;
Begin
  FBalloonType := UixBalloonTypeError;
End;


Procedure TUixBalloon.BalloonTypeHelp;
Begin
  FBalloonType := UixBalloonTypeHelp;
End;


Procedure TUixBalloon.BalloonTypeInformation;
Begin
  FBalloonType := UixBalloonTypeInformation;
End;


Procedure TUixBalloon.BalloonTypeWarning;
Begin
  FBalloonType := UixBalloonTypeWarning;
End;


Procedure TUixBalloon.ControlPositionStyleAbsolute;
Begin
  FControlPositionStyle := UixBalloonControlPositionStyleAbsolute;
End;


Procedure TUixBalloon.ControlPositionStyleRelative;
Begin
  FControlPositionStyle := UixBalloonControlPositionStyleRelative;
End;


Procedure TUixBalloon.ControlRelativeHorizontalPositionCenter;
Begin
  FControlRelativeHorizontalPosition := UixBalloonControlRelativeHorizontalPositionCenter;
End;


Procedure TUixBalloon.ControlRelativeHorizontalPositionLeft;
Begin
  FControlRelativeHorizontalPosition := UixBalloonControlRelativeHorizontalPositionLeft;
End;


Procedure TUixBalloon.ControlRelativeHorizontalPositionRight;
Begin
  FControlRelativeHorizontalPosition := UixBalloonControlRelativeHorizontalPositionRight;
End;


Procedure TUixBalloon.ControlRelativeVerticalPositionBottom;
Begin
  FControlRelativeVerticalPosition := UixBalloonControlRelativeVerticalPositionBottom;
End;


Procedure TUixBalloon.ControlRelativeVerticalPositionCenter;
Begin
  FControlRelativeVerticalPosition := UixBalloonControlRelativeVerticalPositionCenter;
End;


Procedure TUixBalloon.ControlRelativeVerticalPositionTop;
Begin
  FControlRelativeVerticalPosition := UixBalloonControlRelativeVerticalPositionTop;
End;


Procedure TUixBalloon.RenderStyleCustom;
Begin
  FRenderStyle := UixBalloonRenderStyleCustom;
End;


Procedure TUixBalloon.RenderStyleMessage;
Begin
  FRenderStyle := UixBalloonRenderStyleMessage;
End;


Procedure TUixBalloon.ShowBalloon;
Const
  AnchorHeightInteger : Integer = 20;
  AnchorWidthInteger : Integer = 20;
Var
  aRectangle : TRect;
  aFormRegion : HRGN;
  aAnchorRegion : HRGN;
  aPointArray : Array [0..2] Of TPoint;
  iClientHeightContribution : Integer;
  iClientWidthContribution : Integer;
Begin
  Assert((FRenderStyle <> UixBalloonRenderStyleMessage) Or (Message <> ''), 'Message must be set when the render style is message.');
  Assert((FRenderStyle = UixBalloonRenderStyleMessage) Or (Message = ''), 'Message cannot be set when the render style is not message.');

  FHeaderImage.LoadBitmapFromResource(UixBalloonTypeResourceNameArray[FBalloonType]);

  If Assigned(FControl) Then
  Begin
    Assert(Not FHasAbsoluteX, 'Absolute point cannot be provided when using control relative position. (X <> 0)');
    Assert(Not FHasAbsoluteY, 'Absolute point cannot be provided when using control relative position. (Y <> 0)');

    GetWindowRect(FControl.Handle, aRectangle);

    FAnchorCoordinate.X := 0;
    FAnchorCoordinate.Y := 0;

    If FControlPositionStyle = UixBalloonControlPositionStyleAbsolute Then
    Begin
      Assert(FHasControlAbsoluteX, 'Control absolute X coordinate must be set when the position style is absolute.');
      Assert(FHasControlAbsoluteY, 'Control absolute Y coordinate must be set when the position style is absolute.');

      FAnchorCoordinate.X := aRectangle.Left + FControlAbsoluteX;
      FAnchorCoordinate.Y := aRectangle.Top + FControlAbsoluteY;
    End
    Else
    Begin
      Assert(Not FHasControlAbsoluteX, 'Control absolute X coordinate can only be set when the control position style is absolute.');
      Assert(Not FHasControlAbsoluteY, 'Control absolute Y coordinate can only be set when the control position style is absolute.');

      Case FControlRelativeVerticalPosition Of
        UixBalloonControlRelativeVerticalPositionTop :
        Begin
          FAnchorCoordinate.Y := aRectangle.Top;
        End;

        UixBalloonControlRelativeVerticalPositionCenter :
        Begin
          FAnchorCoordinate.Y := aRectangle.Top + Round((aRectangle.Bottom - aRectangle.Top) / 2);
        End;

        UixBalloonControlRelativeVerticalPositionBottom :
        Begin
          FAnchorCoordinate.Y := aRectangle.Bottom;
        End;
      End;

      Case FControlRelativeHorizontalPosition Of
        UixBalloonControlRelativeHorizontalPositionLeft :
        Begin
          FAnchorCoordinate.X := aRectangle.Left;
        End;

        UixBalloonControlRelativeHorizontalPositionCenter :
        Begin
          FAnchorCoordinate.X := aRectangle.Left + Round((aRectangle.Right - aRectangle.Left) / 2);
        End;

        UixBalloonControlRelativeHorizontalPositionRight :
        Begin
          FAnchorCoordinate.X := aRectangle.Right;
        End;
      End;
    End;
  End
  Else
  Begin
    Assert(FHasAbsoluteX, 'Neither Control nor absolute point provided (X = 0)');
    Assert(FHasAbsoluteY, 'Neither Control nor absolute point provided (Y = 0)');
  End;

  FHeaderImage.Top := 12;

  If FAnchorPosition In [UixBalloonAnchorPositionTopLeft, UixBalloonAnchorPositionTopRight] Then
    FHeaderImage.Top := FHeaderImage.Top + AnchorHeightInteger;

  FHeaderLabel.Top := FHeaderImage.Top;

  FMessageLabel.Visible := FRenderStyle = UixBalloonRenderStyleMessage;
  FClientPanel.Visible := FRenderStyle = UixBalloonRenderStyleCustom;

  If FMessageLabel.Visible Then
  Begin
    FMessageLabel.Top := FHeaderLabel.Top + FHeaderLabel.Height + DefaultBufferInteger;

    iClientHeightContribution := FMessageLabel.Top + FMessageLabel.Height;
    iClientWidthContribution := FMessageLabel.Left + FMessageLabel.Width;
  End
  Else
  Begin
    FClientPanel.Top := FHeaderLabel.Top + FHeaderLabel.Height + DefaultBufferInteger;

    iClientHeightContribution := FClientPanel.Top + FClientPanel.Height;
    iClientWidthContribution := FClientPanel.Left + FClientPanel.Width;
  End;

  If FAnchorPosition In [UixBalloonAnchorPositionTopLeft, UixBalloonAnchorPositionTopRight] Then
    ClientHeight := iClientHeightContribution + DefaultBufferInteger
  Else
    ClientHeight := iClientHeightContribution + DefaultBufferInteger + AnchorHeightInteger;

  Width := IntegerMax(FHeaderLabel.Left + FHeaderLabel.Width, iClientWidthContribution) + DefaultBufferInteger;

  If Assigned(FControl) Then
  Begin
    Case FAnchorPosition Of
      UixBalloonAnchorPositionBottomLeft,
      UixBalloonAnchorPositionBottomRight :
      Begin
        Top := FAnchorCoordinate.Y - Height;
      End;

      UixBalloonAnchorPositionTopLeft,
      UixBalloonAnchorPositionTopRight :
      Begin
        Top := FAnchorCoordinate.Y - 2;
      End;
    End;

    Case FAnchorPosition Of
      UixBalloonAnchorPositionTopRight,
      UixBalloonAnchorPositionBottomRight :
      Begin
        Left := FAnchorCoordinate.X - (Width - AnchorWidthInteger);
      End;

      UixBalloonAnchorPositionTopLeft,
      UixBalloonAnchorPositionBottomLeft :
      Begin
        Left := FAnchorCoordinate.X - AnchorWidthInteger;
      End;
    End;
  End
  Else
  Begin
    Left := FAbsoluteX;
    Top := FAbsoluteY;
  End;

  If FAnchorPosition In [UixBalloonAnchorPositionBottomRight, UixBalloonAnchorPositionBottomLeft] Then
    aFormRegion := CreateRoundRectRgn(0, 0, Width, Height - (AnchorHeightInteger - 2), 7, 7)
  Else
    aFormRegion := CreateRoundRectRgn(0, AnchorHeightInteger + 2, Width, Height, 7, 7);

  Case FAnchorPosition Of
    UixBalloonAnchorPositionBottomRight :
    Begin
      aPointArray[0] := Point(Width - AnchorWidthInteger - 20, Height - AnchorHeightInteger);
      aPointArray[1] := Point(Width - 20, Height);
      aPointArray[2] := Point(Width - 20, Height - AnchorHeightInteger);
    End;

    UixBalloonAnchorPositionBottomLeft :
    Begin
      aPointArray[0] := Point(20, Height - AnchorHeightInteger);
      aPointArray[1] := Point(20, Height);
      aPointArray[2] := Point(20 + AnchorWidthInteger, Height - AnchorHeightInteger);
    End;

    UixBalloonAnchorPositionTopLeft :
    Begin
      aPointArray[0] := Point(20, 2);
      aPointArray[1] := Point(20, AnchorHeightInteger + 2);
      aPointArray[2] := Point(20 + AnchorWidthInteger, AnchorHeightInteger + 2);
    End;

    UixBalloonAnchorPositionTopRight :
    Begin
      aPointArray[0] := Point(Width - 20, 2);
      aPointArray[1] := Point(Width - 20, AnchorHeightInteger + 2);
      aPointArray[2] := Point(Width - 20 - AnchorWidthInteger, AnchorHeightInteger + 2);
    End;
  End;

  aAnchorRegion := CreatePolygonRgn(aPointArray, 3, WINDING);

  CombineRgn(aFormRegion, aFormRegion, aAnchorRegion, RGN_OR);
  DeleteObject(aAnchorRegion);
  SetWindowRgn(Handle, aFormRegion, True);

  Visible := False;

  If TimeoutDuration <> 0 Then
    ShowWindow(Handle, SW_SHOWNOACTIVATE)
  Else
    ShowWindow(Handle, SW_NORMAL);

  Visible := True;

  FShowTimer.Enabled := TimeoutDuration <> 0;

  If UseAudibleNotification Then
  Begin
    If BalloonType = UixBalloonTypeError Then
      SoundBeepAsterisk
    Else If BalloonType = UixBalloonTypeWarning Then
      SoundBeepExclamation;
  End;
End;


Procedure TUixBalloon.ReleaseBalloon;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To 100 Do
  Begin
    MakeWindowTransparent(Handle, iLoop);

    ThreadSleep(1);

    Update;
  End;

  Close;
End;


Procedure TUixBalloon.DoClose(Var Action : TCloseAction);
Begin
  FShowTimer.Enabled := False;

  Action := caHide;
End;


Procedure TUixBalloon.WndProc(Var aMessage : TMessage);
Begin
  If (aMessage.Msg = WM_SIZE) And (aMessage.WParam = SIZE_MINIMIZED) Then
    Show;

  Inherited;

  If (aMessage.Msg = WM_ACTIVATEAPP) Then
  Begin
    If Not TWMActivateApp(aMessage).Active Then
      Close;
  End;
End;


Procedure TUixBalloon.PaintDelegate(oSender : TObject);
Var
  aRegion : HRGN;
Begin
  Canvas.Brush.Color := clBlack;
  Canvas.Brush.Style := bsSolid;

  aRegion := CreateRectRgn(0, 0, 1, 1);

  GetWindowRgn(Handle, aRegion);

  FrameRgn(Canvas.Handle, aRegion, Canvas.Brush.Handle, 1, 1);

  DeleteObject(aRegion);
End;


Procedure TUixBalloon.ReleaseDelegate(oSender: TObject);
Begin
  ReleaseBalloon;
End;


Procedure TUixBalloon.ClickDelegate(oSender: TObject);
Begin
  If Assigned(Parent) And Parent.CanFocus Then
    Parent.SetFocus;

  ReleaseBalloon;
End;


Function TUixBalloon.GetTimeoutDuration : Integer;
Begin
  Result := FShowTimer.Interval;
End;


Procedure TUixBalloon.SetTimeoutDuration(Const Value : Integer);
Begin
  FShowTimer.Interval := Value;
End;


Function TUixBalloon.GetMessage : String;
Begin
  Result := FMessageLabel.Caption;
End;


Procedure TUixBalloon.SetMessage(Const Value : String);
Begin
  FMessageLabel.Caption := Value;
End;


Function TUixBalloon.GetTitle : String;
Begin
  Result := FHeaderLabel.Caption;
End;


Procedure TUixBalloon.SetTitle(Const Value : String);
Begin
  FHeaderLabel.Caption := Value;
End;


Constructor TUixTimer.Create(AOwner: TComponent);
Begin
  Inherited;

  FThreadAffinity := ThreadID;
End;


Procedure TUixTimer.Reset;
Begin
  Enabled := False;
  Enabled := True;
End;


Procedure TUixTimer.Start;
Begin
  Enabled := True;
End;


Procedure TUixTimer.Stop;
Begin
  Enabled := False;
End;


Procedure TUixTimer.Timer;
Begin
  If ThreadID <> FThreadAffinity Then
    raise ELibraryException.create('Thread affinity was violated.');

  Inherited;
End;


Function TUixCodeMask.Link : TUixCodeMask;
Begin
  Result := TUixCodeMask(Inherited Link);
End;


{$IFDEF VER130}
Constructor TUixCodeHighlighter.Create(oOwner : TComponent);
Begin
  Inherited;

  FTextAttributes := TSynHighLighterAttributes.Create('Text');
  AddAttribute(FTextAttributes);

  FBeyondAttributes := TSynHighLighterAttributes.Create('Beyond');
  FBeyondAttributes.Foreground := clBtnText;
  AddAttribute(FBeyondAttributes);

  FInvalidAttributes := TSynHighLighterAttributes.Create('Invalid');
  FInvalidAttributes.Style := [];
  FInvalidAttributes.Background := clRed;
  FInvalidAttributes.Foreground := clWhite;
  AddAttribute(FInvalidAttributes);

  FMaskAttributes := TSynHighLighterAttributes.Create('Fixed');
  FMaskAttributes.Style := [];
  AddAttribute(FMaskAttributes);

  FRemainderAttributes := TSynHighLighterAttributes.Create('Remainder');
  FRemainderAttributes.Style := [];
  FRemainderAttributes.Foreground := clGrayText;
  AddAttribute(FRemainderAttributes);

  FRequiredAttributes := TSynHighLighterAttributes.Create('Required');
  FRequiredAttributes.Style := [fsUnderline];
  AddAttribute(FRequiredAttributes);

  FCustomAttributes := TSynHighlighterAttributes.Create('Custom');
  AddAttribute(FCustomAttributes);
End;


Destructor TUixCodeHighlighter.Destroy;
Begin
  FMask.Free;

  Inherited;
End;


Procedure TUixCodeHighlighter.Next;
Begin
  Scan;
End;


Procedure TUixCodeHighlighter.ResetRange;
Begin
  FToken := UixCodeTokenUnknown;
End;


Procedure TUixCodeHighlighter.Scan;
Var
  iStart : Integer;
Begin
  If (Index > Length(FText)) Then
  Begin
    If Index <= FMask.MinLength Then
    Begin
      FValue := Copy(FMask.FixedFormat, Index, FMask.MinLength - Index + 1);
      Index := FMask.MinLength + 1;
      FToken := UixCodeTokenRequired;
    End
    Else
    Begin
      FValue := Copy(FMask.FixedFormat, Index, MaxInt);
      Index := FMask.FixedLength + 1;
      FToken := UixCodeTokenRemainder;
    End;
  End
  Else If (Index > FMask.MaxLength) And (FMask.MaxLength > 0) Then
  Begin
    FValue := Copy(FText, Index, MaxInt);
    Index := Length(FText) + 1;
    FToken := UixCodeTokenInvalid;
  End
  Else
  Begin
    iStart := Index;

    If (Index > FMask.Count) Then
    Begin
      FValue := Copy(FText, Index, MaxInt);
      Index := Length(FText) + 1;
      FToken := UixCodeTokenText;
    End
    Else
    Begin
      FToken := CurrentToken;

      Index := Index + 1;
      While (Index <= Length(FText)) And (Index <= FMask.Count) And (FToken = CurrentToken) Do
        Index := Index + 1;

      FValue := Copy(FText, iStart, Index - iStart);
    End;

    If Assigned(FOnCustom) Then
    Begin
      FCustomAttributes.Assign(GetTokenAttribute);

      FOnCustom(Self, iStart, FValue, FCustomAttributes);

      // FValue may be reduced and transformed here to break the whole text into
      // different coloured sections.

      Index := iStart + Length(FValue);
      FToken := UixCodeTokenCustom;
    End;
  End;
End;


Function TUixCodeHighlighter.CurrentToken : TUixCodeToken;
Begin
  If FMask.Fixed(Index) And (FText[Index] = FMask.FixedFormat[Index]) Then
    Result := UixCodeTokenFixed
  Else If CharInSet(FText[Index], FMask.Allowed(Index)) Then
    Result := UixCodeTokenText
  Else
    Result := UixCodeTokenInvalid;
End;


Function TUixCodeHighlighter.GetEol: Boolean;
Begin
  Result := FValue = '';
End;


Function TUixCodeHighlighter.GetIndex: Integer;
Begin
  Result := FIndex;
End;


Procedure TUixCodeHighlighter.SetIndex(const Value: Integer);
Begin
  FIndex := Value;
End;


Class Function TUixCodeHighlighter.GetLanguageName: String;
Begin
  Result := ClassName;
End;


Function TUixCodeHighlighter.GetToken: String;
Begin
  Result := FValue;
End;


Function TUixCodeHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
Begin
  Case FToken Of
    UixCodeTokenFixed     : Result := FMaskAttributes;
    UixCodeTokenText      : Result := FTextAttributes;
    UixCodeTokenBeyond    : Result := FBeyondAttributes;
    UixCodeTokenInvalid   : Result := FInvalidAttributes;
    UixCodeTokenRemainder : Result := FRemainderAttributes;
    UixCodeTokenRequired  : Result := FRequiredAttributes;
    UixCodeTokenCustom    : Result := FCustomAttributes;
  Else
    Result := Nil;
  End;
End;


Function TUixCodeHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
Begin
  Case Index Of
    SYN_ATTR_IDENTIFIER : Result := FTextAttributes;
    SYN_ATTR_KEYWORD    : Result := FMaskAttributes;
    SYN_ATTR_WHITESPACE :
    Begin
      If (FMask.MaxLength > 0) And TUixCodeEdit(Owner).Focused Then
        Result := FBeyondAttributes
      Else
        Result := FTextAttributes;
    End;
  Else
    Result := Nil;
  End;
End;


Function TUixCodeHighlighter.GetTokenKind: Integer;
Begin
  Result := Integer(FToken);
End;


Function TUixCodeHighlighter.GetTokenPos: Integer;
Begin
  Result := Index - Length(FValue) - 1;
End;


Function TUixCodeHighlighter.GetRange : Pointer;
Begin
  Result := Pointer(FToken);
End;


Procedure TUixCodeHighlighter.SetRange(Value : Pointer);
Begin
  FToken := TUixCodeToken(Value);
End;


Procedure TUixCodeHighlighter.SetMask(Const Value: TUixCodeMask);
Begin
  FMask.Free;
  FMask := Value;
End;


Procedure TUixCodeHighlighter.SetLine(NewValue: String; LineNumber: Integer);
Begin
  FText := NewValue;
  Index := 1;
  Scan;
End;
{$ENDIF}


Constructor TUixCodeEdit.Create(oOwner: TComponent);
Begin
  Inherited;

  FReadBackgroundColour := clBtnFace;
  FWriteBackgroundColour := clWindow;
  FInvalidBackgroundColour := clWindow;

  FAllowedCharacterList := TFslCharacterList.Create;
  FAllowedCharacterList.SortByValue;
  FAllowedCharacterList.IgnoreDuplicates;

  FDisallowedCharacterList := TFslCharacterList.Create;
  FDisallowedCharacterList.SortByValue;
  FDisallowedCharacterList.IgnoreDuplicates;

  FMask := TUixCodeMask.Create;

{$IFDEF VER130}
  Inherited Highlighter := TUixCodeHighlighter.Create(Self);
{$ELSE}
  Inherited Highlighter := TUixUnicodeCodeHighlighter.Create(Self);
{$ENDIF}

  Highlighter.Mask := FMask.Link;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  OnChange := InternalChangeDelegate;
  OnExit := InternalExitDelegate;

  FAutoSelect := True;
  FBalloonAudible := False;

  Height := 21;

  Options := Options - [eoScrollPastEOL, eoTrimTrailingSpaces] + [eoRightMouseMovesCursor];
  WantReturns := False;
  ScrollBars := ssNone;
  HideSelection := True;
  RightEdge := 0;

  Font.Name := 'Courier New'; // TODO: Consolas is better but only available in Vista/Office2007.
  Font.Size := 8;

  Gutter.Visible := False;

  Lines.Add('');

  Prepare;
End;


Destructor TUixCodeEdit.Destroy;
Begin
  FDisallowedCharacterList.Free;
  FAllowedCharacterList.Free;
  FMask.Free;

  Inherited;
End;


Procedure TUixCodeEdit.Error(Const sMethod, sMessage : String);
Begin
  Raise EFslException.Create(Self, sMethod, sMessage);
End;


Procedure TUixCodeEdit.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixCodeEdit.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixCodeEdit.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixCodeEdit.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixCodeEdit.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixCodeEdit.EnforceValidationModeDateTime;
Begin
  If Not IsValidationModeDateTime Then
    Error('EnforceValidationModeDateTime', 'Edit control is not in date time mode.');
End;


Procedure TUixCodeEdit.EnforceValidationModeString;
Begin
  If Not IsValidationModeString Then
    Error('EnforceValidationModeString', 'Edit control is not in string mode.');
End;


Procedure TUixCodeEdit.EnforceValidationModeDuration;
Begin
  If Not IsValidationModeDuration Then
    Error('EnforceValidationModeDuration', 'Edit control is not in duration mode.');
End;


Procedure TUixCodeEdit.EnforceValidationModeExtended;
Begin
  If Not IsValidationModeExtended Then
    Error('EnforceValidationModeExtended', 'Edit control is not in extended mode.');
End;


Procedure TUixCodeEdit.EnforceValidationModeCurrency;
Begin
  If Not IsValidationModeCurrency Then
    Error('EnforceValidationModeCurrency', 'Edit control is not in currency mode.');
End;


Procedure TUixCodeEdit.EnforceValidationModeInteger;
Begin
  If Not IsValidationModeInteger Then
    Error('EnforceValidationModeInteger', 'Edit control is not in integer mode.');
End;


Function TUixCodeEdit.IsValidationModeString : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeString;
End;


Function TUixCodeEdit.IsValidationModeDateTime : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeDateTime;
End;


Function TUixCodeEdit.IsValidationModeDuration : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeDuration;
End;


Function TUixCodeEdit.IsValidationModeInteger : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeInteger;
End;


Function TUixCodeEdit.IsValidationModeExtended : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeExtended;
End;


Function TUixCodeEdit.IsValidationModeCurrency : Boolean;
Begin
  Result := FValidationMode = UixCodeEditValidationModeCurrency;
End;


Procedure TUixCodeEdit.ValidationModeDateTime;
Begin
  FValidationMode := UixCodeEditValidationModeDateTime;

  FIncludeDate := True;
  FIncludeDays := True;
  FIncludeMonths := True;
  FIncludeYears := True;
  FAllowsPast := True;
  FAllowsFuture := True;
  FMinDateTimeValue := DATETIME_MIN;
  FMaxDateTimeValue := DATETIME_MAX;

  PrepareDateTimeMask;
End;


Procedure TUixCodeEdit.ValidationModeDuration;
Begin
  FValidationMode := UixCodeEditValidationModeDuration;

  FIncludeHours := True;
  FIncludeMinutes := True;
  FIncludeSeconds := True;
  FIncludeMilliSeconds := True;
  FMinDurationValue := 0;
  FMaxDurationValue := High(FMaxDurationValue);

  PrepareDurationMask;
End;


Procedure TUixCodeEdit.ValidationModeCurrency;
Begin
  FValidationMode := UixCodeEditValidationModeCurrency;

  FMinCurrencyValue := 0.0;
  FMinCurrencyValue := FMinCurrencyValue - 922337203685477.58;
  FMaxCurrencyValue := 922337203685477.58;
  FDenominationCurrencyValue := 0.01;
End;


Procedure TUixCodeEdit.ValidationModeInteger;
Begin
  FValidationMode := UixCodeEditValidationModeInteger;

  MinIntegerValue := 0;
  MaxIntegerValue := High(Int64);

  FHasDefaultIntegerValue := False;
  FDefaultIntegerValue := 0;

  Prepare;
End;


Procedure TUixCodeEdit.ValidationModeExtended;
Begin
  FValidationMode := UixCodeEditValidationModeExtended;

  MaxExtendedValue := 10000000000;
End;


Procedure TUixCodeEdit.ValidationModeString;
Begin
  FValidationMode := UixCodeEditValidationModeString;
End;


Procedure TUixCodeEdit.ExecuteCommand(Command: TSynEditorCommand; cChar: Char; Data: Pointer);
Var
  iPosition : Integer;
  iTrailingFixedLength : Integer;
Begin
  If (Command <> ecLineBreak) And (Command <> ecInsertLine) Then
  Begin
    Case Command Of
      ecChar :
      Begin
        HideBalloon;

        If Not ReadOnly And CanInsertCharacter Then
        Begin
          If AllowInsertCharacter(cChar) Then
          Begin
            If Not ValidateCharacter(cChar) Or Not ValidateInsertCharacter(cChar) Then
            Begin
              ShowBalloon;
            End
            Else If (cChar >= ' ') And (cChar <= #127) Then
            Begin
              If SelAvail Then
                AdjustSelectionEnd;

              Inherited;

              If Assigned(FStandardKeyPressDelegate) Then
                FStandardKeyPressDelegate(Self, cChar);

              Complete;
            End;
          End;
        End;
      End;

      ecCut,
      ecDeleteChar :
      Begin
        HideBalloon;

        If SelAvail Then
        Begin
          AdjustSelectionEnd;

          Inherited;

          Complete;
        End
        Else
        Begin
          iPosition := CaretX - 1;
          iTrailingFixedLength := FMask.TrailingFixedLength(CaretX);

          If (iPosition >= FMask.LeadingFixedLength) Then
          Begin
            Inherited;

            If (iTrailingFixedLength > 0) And ((iPosition + iTrailingFixedLength) = Length(Text)) Then
            Begin
              While (iTrailingFixedLength > 0) Do
              Begin
                Inherited;

                Dec(iTrailingFixedLength);
              End;
            End;
          End;
        End;
      End;

      ecDeleteLastChar,
      ecDeleteLastWord,
      ecDeleteLine,
      ecDeleteEOL,
      ecDeleteBOL :
      Begin
        HideBalloon;

        If SelAvail Then
        Begin
          AdjustSelectionStart;
          AdjustSelectionEnd;

          If SelAvail Then
          Begin
            Inherited;

            Complete;
          End;
        End
        Else
        Begin
          iPosition := CaretX - 1;
          iTrailingFixedLength := FMask.TrailingFixedLength(iPosition);

          If (iTrailingFixedLength > 0) And ((iPosition + iTrailingFixedLength) = Length(Text)) Then
          Begin
            CaretX := CaretX + iTrailingFixedLength;
            iPosition := iPosition + iTrailingFixedLength;
          End;

          If (iPosition > FMask.LeadingFixedLength) Then
          Begin
            Inherited;

            While (iPosition > 0) And FMask.Fixed(iPosition) Do
            Begin
              Inherited;

              Dec(iPosition);
            End;
          End;
        End;
      End;

      ecInsertMode, ecOverwriteMode, ecToggleMode :
      Begin
        HideBalloon;

        If Not FIgnoreOverwrite Then
          Inherited;
      End;

      ecPaste :
      Begin
        HideBalloon;

        If CanPaste Then
          Inherited;
      End;
    Else
      Inherited;
    End;
  End;
End;


Procedure TUixCodeEdit.CursorEnd;
Begin
  CaretX := 1;
End;


Procedure TUixCodeEdit.CursorStart;
Begin
  CaretX := Length(Text);
End;


Procedure TUixCodeEdit.Complete;
Var
  iPosition : Integer;
  sText : String;
Begin
  iPosition := CaretX;

  sText := Text;

  While (iPosition >= Length(Text)) And FMask.Fixed(iPosition) Do
  Begin
    If sText[iPosition] <> FMask.FixedFormat[iPosition] Then
      sText := sText + FMask.FixedFormat[iPosition];

    Inc(iPosition);
  End;

  CaretX := iPosition;

  If Text <> sText Then
    Text := sText;

  Prepare;
End;


Procedure TUixCodeEdit.HideBalloon;
Begin
  If Assigned(FBalloon) Then
  Begin
    FBalloon.Release;
    FBalloon := Nil;
  End;
End;


Procedure TUixCodeEdit.ShowBalloon;
Begin
  HideBalloon;

  FBalloon := TUixBalloon.CreateNew(Self);
  FBalloon.Parent := Self;
  FBalloon.Title := Title;
  FBalloon.Message := LastError;
  FBalloon.TimeoutDuration := 4000;
  FBalloon.BalloonTypeError;
  FBalloon.AnchorPositionTopLeft;
  FBalloon.UseAudibleNotification := FBalloonAudible;
  FBalloon.Control := Self;
  FBalloon.ControlRelativeHorizontalPositionCenter;
  FBalloon.ControlRelativeVerticalPositionBottom;
  FBalloon.ShowBalloon;
End;


Procedure TUixCodeEdit.SetText(Const Value: String);
Begin
  Inherited Text := Value;

  If Lines.Count = 0 Then
    Lines.Add('');

  CaretX := Length(Value) + 1;

  Complete;
  Prepare;
End;


Function TUixCodeEdit.Empty: Boolean;
Begin
  If IsValidationModeDuration Then
    Result := (StringReplace(Text, [TimeSeparator, ' '], '') = '')
  Else
    Result := Text = '';
End;


Function TUixCodeEdit.Full : Boolean;
Begin
  Result := (FMask.MaxLength > 0) And (Length(Text) = FMask.MaxLength);
End;


Function TUixCodeEdit.ValidateCharacter(Const cChar : Char) : Boolean;
Var
  iAllowedIndex : Integer;
  sAllowedCharacters : String;
  sCharacterDesription : String;
Begin
  Result := Not DisallowedCharacterList.ExistsByValue(cChar);

  If Result And Not AllowedCharacterList.ExistsByValue(cChar) Then
  Begin
    If EnforceAlphabetic And EnforceNumeric Then
      Result := StringIsAlphanumeric(cChar)
    Else If EnforceAlphabetic Then
      Result := StringIsAlphabetic(cChar)
    Else If EnforceNumeric Then
      Result := StringIsNumeric(cChar);
  End;

  If Not Result Then
  Begin
    sAllowedCharacters := '';

    If EnforceAlphabetic Then
    Begin
      StringAppend(sAllowedCharacters, 'A-Z', ', ');

      If Not EnforceCapitals Then
      Begin
        StringAppend(sAllowedCharacters, 'a-z', ', ');
      End;
    End;

    If EnforceNumeric Then
    Begin
      StringAppend(sAllowedCharacters, '0-9', ', ');
    End;

    For iAllowedIndex := 0 To AllowedCharacterList.Count - 1 Do
      sAllowedCharacters := sAllowedCharacters + ', ' + AllowedCharacterList[iAllowedIndex].Value;

    sCharacterDesription := '';

    If cChar = cEnter Then
      sCharacterDesription := 'Carriage Return'
    Else If cChar = cFeed Then
      sCharacterDesription := 'Line Feed'
    Else If CharInSet(cChar, [cTab, cVerticalTab]) Then
      sCharacterDesription := 'Tab';

    If sCharacterDesription = '' Then
      LastError := StringFormat('%s does not allow character (%s) as it can only contain characters (%s)', [Title, cChar, sAllowedCharacters])
    Else
      LastError := StringFormat('%s does not allow character (%s) as it can only contain characters (%s)', [Title, sCharacterDesription, sAllowedCharacters]);
  End;

  If Result And IsValidationModeCurrency Then
  Begin
    Result := StringIsNumeric(cChar) Or CharInSet(cChar, (setSigns + ['.']));

    If Not Result Then
      LastError := Title + ' can only contain number characters (0-9), decimal point and minus sign.';
  End;

  If Result And IsValidationModeInteger Then
  Begin
    Result := StringIsNumeric(cChar) Or CharInSet(cChar, setSigns);

    If Not Result Then
    Begin
      LastError := Title + ' can only contain number characters (0-9) and minus sign.';
    End
    Else
    Begin
      Result := (MinIntegerValue < 0) Or (cChar <> '-');

      If Not Result Then
        LastError := 'You can only type a positive number here.';
    End;
  End;

  If Result And IsValidationModeExtended Then
  Begin
    Result := StringIsNumeric(cChar) Or CharInSet(cChar, (setSigns + ['.']));

    If Not Result Then
    Begin
      LastError := Title + ' can only contain number characters (0-9), decimal point and minus sign.';
    End
    Else
    Begin
      Result := (MinExtendedValue < 0) Or (cChar <> '-');

      If Not Result Then
      Begin
        LastError := 'You can only type a positive number here.';
      End;
    End;
  End;
End;


Function TUixCodeEdit.ValidateInsertCharacter(Const cChar : Char) : Boolean;
Var
  bIsCRKey : Boolean;
  iDecimalPos : Integer;
  iPosition : Integer;
Begin
  Result := SelAvail;

  If Not Result Then
  Begin
    iPosition := CaretX;

    bIsCRKey := cChar = cEnter;

    Result := (FMask.MaxLength <= 0) Or (Length(Text) < FMask.MaxLength) Or bIsCRKey;

    If Not bIsCRKey Then
    Begin
      If Not Result Then
      Begin
        LastError := StringFormat('%s cannot exceed %d %s.', [Title, FMask.MaxLength, StringPlural('character', FMask.MaxLength)]);
      End
      Else
      Begin
        Result := CharInSet(cChar, FMask.Allowed(iPosition));

        If Not Result Then
          LastError := StringFormat('%s only allows characters (%s) in this position.', [Title, FMask.AllowedAsText(iPosition)]);
      End;
    End;
  End;

  If Result And IsValidationModeCurrency Then
  Begin
    Case cChar Of
      '.' :
      Begin
        Result := Pos('.', Text) = 0;

        If Not Result Then
          LastError := Title + ' already contains a decimal point.';
      End;

      '-' :
      Begin
        Result := FMinCurrencyValue < 0.0;

        If Result Then
        Begin
          Result := Pos('-', Text) = 0;

          If Not Result Then
            LastError := Title + ' already contains a minus sign.';

          If Result Then
          Begin
            Result := CaretX = 1;

            If Not Result Then
              LastError := 'Minus sign must be the first character of ' + Title + '.';
          End;
        End
        Else
        Begin
          LastError := 'Negative amounts are not allowed.';
        End;
      End;
    Else
      iDecimalPos := Pos('.', Text);

      If (iDecimalPos > 0) And (CaretX > iDecimalPos) Then
      Begin
        Result := iDecimalPos >= (Length(Text) - 1);

        If Not Result Then
          LastError := Title + ' only allows two decimal places.';
      End;
    End;
  End;
End;


Function TUixCodeEdit.Valid : Boolean;
Begin
  LastError := '';

  Result := ReadOnly Or Not Enabled Or (ValidateLength And ValidateContent);
End;


Function TUixCodeEdit.ValidateLength : Boolean;
Begin
  Result := Not (Empty And Mandatory);

  If Not Result Then
  Begin
    LastError := Title + ' is a mandatory field and must be entered.';
  End
  Else
  Begin
    Result := (Empty And Not Mandatory);

    If Not Result Then
    Begin
      Result := (Length(Text) >= FMask.MinLength);

      If Not Result Then
      Begin
        LastError := StringFormat('%s must be at least %d %s in length.', [Title, FMask.MinLength, StringPlural('character', FMask.MinLength)])
      End
      Else
      Begin
        Result := (FMask.MaxLength <= 0) Or (Length(Text) <= FMask.MaxLength);

        If Not Result Then
        Begin
          LastError := StringFormat('%s can be no more than %d %s in length.', [Title, FMask.MaxLength, StringPlural('character', FMask.MaxLength)]);
        End;
      End;
    End;
  End;
End;


Function TUixCodeEdit.ValidateContent : Boolean;
Var
  aCurrencyValue : TCurrency;
  aDateTimeNow : TDateTime;
  aDateTimeValue : TDateTime;
  aDurationValue : TDurationMS;
  iLoop : Integer;
  iIntegerValue : Int64;
  rValue : Extended;
  sText : String;
  sValue : String;
Begin
  Result := Conforms(Text);

  If Not Result Then
  Begin
    LastError := StringFormat('%s does not match the input mask.', [Title]);
  End
  Else
  Begin
    iLoop := 1;

    sText := Text;

    While Result And (iLoop <= Length(sText)) Do
    Begin
      Result := ValidateCharacter(sText[iLoop]);

      Inc(iLoop);
    End;
  End;

  If Result And IsValidationModeDateTime Then
  Begin
    sValue := Text;

    Result := Empty;

    If Not Result Then
    Begin
      If FIncludeDate Then
        Result := IsDateTime(sValue, DateTimeFormat)
      Else If FIncludeTime Then
        Result := StringIsTime(sValue)
      Else
        Result := True;

      If Not Result Then
        LastError := Title + ' is an invalid date/time format.'
      Else
      Begin
        aDateTimeValue := ValueAsDateTime;

        Result := (aDateTimeValue >= FMinDateTimeValue);

        If Not Result Then
          LastError := StringFormat('%s has a value of ''%s'', it must be later than ''%s''.', [Title, DisplayDateTime(aDateTimeValue), DisplayDateTime(FMinDateTimeValue)])
        Else
        Begin
          Result := (aDateTimeValue <= FMaxDateTimeValue);

          If Not Result Then
            LastError := StringFormat('%s has a value of ''%s'', it must be earlier than ''%s''.', [Title, DisplayDateTime(aDateTimeValue), DisplayDateTime(FMaxDateTimeValue)]);
        End;

        If Result Then
        Begin
          If FAssumedNow = 0 Then
            aDateTimeNow := LocalDateTime
          Else
            aDateTimeNow := FAssumedNow;

          If IncludeDate And Not IncludeTime Then
          Begin
            aDateTimeNow := AsDate(aDateTimeNow);

            If Not IncludeDays Then
              aDateTimeNow := FirstOfMonth(aDateTimeNow);
          End
          Else If IncludeTime And Not IncludeDate Then
          Begin
            aDateTimeNow := AsTime(aDateTimeNow);
          End;

          Result := FAllowsPast Or (aDateTimeValue >= aDateTimeNow);

          If Not Result Then
          Begin
            LastError := StringFormat('%s cannot be in the past.', [Title]);
          End
          Else
          Begin
            Result := FAllowsFuture Or (aDateTimeValue <= aDateTimeNow);

            If Not Result Then
              LastError := StringFormat('%s cannot be in the future.', [Title]);
          End;
        End;
      End;
    End;
  End;

  If Result And IsValidationModeDuration Then
  Begin
    Result := Empty;

    If Not Result Then
    Begin
      Result := StringIsDuration(NormaliseDuration);

      If Not Result Then
        LastError := Text + ' is an invalid duration.'
      Else
      Begin
        aDurationValue := ValueAsDuration;

        Result := (aDurationValue >= FMinDurationValue);

        If Not Result Then
          LastError := StringFormat('%s has a value of ''%s'', it must be greater or equal to ''%s''.', [Title, DisplayDuration(aDurationValue), DisplayDuration(FMinDurationValue)])
        Else
        Begin
          Result := (aDurationValue <= FMaxDurationValue);

          If Not Result Then
            LastError := StringFormat('%s has a value of ''%s'', it must be less than or equal to ''%s''.', [Title, DisplayDuration(aDurationValue), DisplayDuration(FMaxDurationValue)]);
        End;
      End;
    End;
  End;

  If Result And IsValidationModeCurrency And (Text <> '') Then
  Begin
    aCurrencyValue := StringToCurrency(Text);

    Result := (aCurrencyValue >= FMinCurrencyValue) And (aCurrencyValue <= FMaxCurrencyValue);

    If Not Result Then
      LastError := StringFormat('%s has a value of ''%s'', which is out of valid range (%s..%s).', [Title, Text, CurrencyToString(FMinCurrencyValue), CurrencyToString(FMaxCurrencyValue)])
    Else
    Begin
      Result := Trunc(aCurrencyValue * 100) Mod Trunc(FDenominationCurrencyValue * 100) = 0;

      If Not Result Then
        LastError := StringFormat('%s has a value of ''%s'' which is required to be a multiple of ''%s''.', [Title, Text, CurrencyToString(FDenominationCurrencyValue)]);
    End;
  End;

  If Result And IsValidationModeInteger And (Text <> '') Then
  Begin
    Result := StringIsInteger64(Text);

    If Not Result Then
    Begin
      LastError := Title + ' has an invalid integer value of ''' + Text + '''';
    End
    Else
    Begin
      iIntegerValue := StringToInteger64(Text);

      Result := (iIntegerValue >= FMinIntegerValue) And (iIntegerValue <= FMaxIntegerValue);

      If Not Result Then
        LastError := StringFormat('%s has a value of ''%d'', which is not within the valid range (%d..%d)', [Title, iIntegerValue, FMinIntegerValue, FMaxIntegerValue]);
    End;
  End;

  If Result And IsValidationModeExtended And (Text <> '') Then
  Begin
    Result := StringIsReal(Text);

    If Not Result Then
    Begin
      LastError := Title + ' has an invalid real value of ''' + Text + '''';
    End
    Else
    Begin
      rValue := StringToReal(Text);

      Result := (rValue >= FMinExtendedValue) And (rValue <= FMaxExtendedValue);

      If Not Result Then
        LastError := StringFormat('%s has a value of ''%s'', which is out of valid range (%s..%s)', [Title, Text, DisplayExtended(FMinExtendedValue), DisplayExtended(FMaxExtendedValue)]);
    End;
  End;

  If Result Then
  Begin
    Result := FAllowLeadingWhitespace Or (Text = '');

    If Not Result Then
    Begin
      Result := StringGet(Text, 1) <> ' ';

      If Not Result Then
        LastError := Title + ' cannot have leading whitespace.'
    End;
  End;

  If Result And Assigned(FValidateContentDelegate) Then
    Result := FValidateContentDelegate;
End;


Procedure TUixCodeEdit.Change;
Begin
  Prepare;
End;


Procedure TUixCodeEdit.Prepare;
Begin
  If ReadOnly Then
    Color := FReadBackgroundColour
  Else If Not IsValidLength And HasInvalidBackgroundColour Then
    Color := InvalidBackgroundColour
  Else If IsDefaultStringValue And HasDefaultBackgroundColour Then
    Color := DefaultBackgroundColour
  Else
    Color := FWriteBackgroundColour;
End;


Procedure TUixCodeEdit.PrepareMask;
Begin
  If IsValidationModeDateTime Then
    PrepareDateTimeMask
  Else If IsValidationModeDuration Then
    PrepareDurationMask;
End;


Procedure TUixCodeEdit.PrepareDurationMask;
Var
  sFormat : String;
Begin
  EnforceValidationModeDuration;

  sFormat := DurationFormat;

  If FExtendedHours Then
    FMaxDurationValue := StringToDuration(StringMultiply('9', Length('hhh')) + TimeSeparator + '59' + TimeSeparator + '59' + DecimalSeparator + '999')
  Else
    FMaxDurationValue := StringToDuration('99' + TimeSeparator + '59' + TimeSeparator + '59' + DecimalSeparator + '999');

  MaxLength := Length(sFormat);

  If FMandatory Then
    MinLength := MaxLength
  Else
    MinLength := 0;

  Mask := StringReplace(sFormat, setUniversal - [TimeSeparator, DecimalSeparator], '9');
End;


Procedure TUixCodeEdit.PrepareDateTimeMask;
Var
  sFormat : String;
Begin
  EnforceValidationModeDateTime;

  sFormat := DateTimeFormat;

  MaxLength := Length(sFormat);
  sFormat := StringReplace(sFormat, ['d', 'm', 'y', 'h', 'n', 's'], '9');
  sFormat := StringReplace(sFormat, ['/'], fsl_utilities.DateSeparator);
  sFormat := StringReplace(sFormat, [':'], fsl_utilities.TimeSeparator);
  Mask := sFormat;

  If TimeOptional Then
    MinLength := Length(DateFormat)
  Else
    MinLength := MaxLength;
End;


Procedure TUixCodeEdit.PrepareIntegerMask;
Begin
  EnforceValidationModeInteger;

  MaxLength := IntegerMax(Length(IntegerToString(FMinIntegerValue)), Length(IntegerToString(FMaxIntegerValue)));
End;


Procedure TUixCodeEdit.PrepareExtendedMask;
Begin
  EnforceValidationModeExtended;

  MaxLength := IntegerMax(Length(DisplayExtended(FMinExtendedValue)), Length(DisplayExtended(FMaxExtendedValue))) + FExtendedDecimalPlaces + 1;
End;


Function TUixCodeEdit.DisplayDateTime(Const aValue : TDateTime) : String;
Begin
  If DateTimeEquals(aValue, 0, DATETIME_MILLISECOND_ONE) Then
    Result := ''
  Else If FIncludeDate Then
    Result := fsl_utilities.DateTimeFormat(aValue, DateTimeFormat)
  Else If FIncludeTime Then
    Result := fsl_utilities.DateTimeFormat(aValue, TimeFormat)
  Else
    Result := '';
End;


Function TUixCodeEdit.DisplayDateTime(Const aValue : TDateTimeOffset) : String;
Begin
  If DateTimeOffsetEquals(aValue, DateTimeOffsetZero, DATETIME_MILLISECOND_ONE) Then
    Result := ''
  Else If FIncludeDate Then
    Result := fsl_utilities.DateTimeFormat(aValue, DateTimeFormat)
  Else If FIncludeTime Then
    Result := fsl_utilities.DateTimeFormat(aValue, TimeFormat)
  Else
    Result := '';
End;


Function TUixCodeEdit.DisplayDuration(Const aValue : TDurationMS) : String;
Var
  iSeparator : Integer;
  iCount : Integer;
Begin
  If IncludeMilliSeconds Then
    Result := DurationToMilliSecondsString(aValue)
  Else If IncludeSeconds Then
    Result := DurationToSecondsString(aValue)
  Else If IncludeMinutes Then
    Result := DurationToMinutesString(aValue)
  Else
    Result := DurationToHoursString(aValue);

  If Not IncludeHours Then
    Result := Copy(Result, 4, MaxInt);
  If Not IncludeMinutes And Not IncludeHours Then
    Result := Copy(Result, 4, MaxInt);
  If Not IncludeSeconds And Not IncludeMinutes Then
    Result := Copy(Result, 4, MaxInt);

  If ExtendedHours And IncludeHours Then
  Begin
    iSeparator := Pos(TimeSeparator, Result);

    If iSeparator < 1 Then
      iSeparator := MaxInt;

    iCount := Length('hhh') - Length(Copy(Result, 1, iSeparator - 1));

    If iCount > 0 Then
      Result := StringMultiply('0', iCount) + Result;
  End;
End;


Function TUixCodeEdit.DisplayExtended(Const aValue : Extended) : String;
Begin
  Result := StringFormat('%.' + IntegerToString(FExtendedDecimalPlaces) + 'f', [aValue]);
End;


Function TUixCodeEdit.NormaliseDuration : String;
Var
  sPrefix : String;
Begin
  sPrefix := '';

  If Not IncludeHours Then
  Begin
    If FExtendedHours Then
      sPrefix := '000'
    Else
      sPrefix := '00';

    If IncludeMinutes Then
      sPrefix := sPrefix + TimeSeparator
    Else
    Begin
      If IncludeSeconds Then
        sPrefix := sPrefix + TimeSeparator + '00' + TimeSeparator
      Else
      Begin
        If IncludeMilliSeconds Then
          sPrefix := sPrefix + TimeSeparator + '00' + TimeSeparator + '00' + DecimalSeparator
      End;
    End;
  End;

  Result := sPrefix + Text;
End;


Procedure TUixCodeEdit.Select(Const iFrom, iTo: Integer);
Begin
  SelStart := iFrom;
  SelEnd := iTo;
End;


Function TUixCodeEdit.Conforms(Const sText : String): Boolean;
Begin
  Result := FMask.Conforms(sText);
End;


Function TUixCodeEdit.DateFormat : String;
Begin
  Result := '';

  If FIncludeDate Then
  Begin
    If FIncludeDays Then
      StringAppend(Result, 'dd', '/');

    If FIncludeMonths Then
      StringAppend(Result, 'mm', '/');

    If FIncludeYears Then
      StringAppend(Result, 'yyyy', '/');
  End;
End;


Function TUixCodeEdit.TimeFormat: String;
Begin
  If FIncludeTime Then
    Result := 'hh:nn'
  Else
    Result := '';
End;


Function TUixCodeEdit.DateTimeFormat : String;
Begin
  Result := DateFormat;

  If FIncludeTime Then
    StringAppend(Result, TimeFormat, ' ');
End;


Function TUixCodeEdit.DurationFormat : String;
Begin
  Result := '';

  If FIncludeHours Then
  Begin
    If FExtendedHours Then
      StringAppend(Result, 'hhh', '')
    Else
      StringAppend(Result, 'hh', '');
  End;

  If FIncludeMinutes Then
    StringAppend(Result, 'mm', TimeSeparator);

  If FIncludeSeconds Then
    StringAppend(Result, 'ss', TimeSeparator);

  If FIncludeMilliSeconds Then
    StringAppend(Result, 'zzz', DecimalSeparator);
End;


Function TUixCodeEdit.HasDate : Boolean;
Begin
  Result := IncludeDate And StringIsDate(Copy(Text, 1, Length(DateFormat)), DateFormat)
End;


Function TUixCodeEdit.HasCompleteDate : Boolean;
Var
  sDateFormat : String;
Begin
  sDateFormat := DateFormat;

  Result := IncludeDate And
            (Length(Text) = Length(sDateFormat)) And
            StringIsDate(Copy(Text, 1, Length(sDateFormat)), sDateFormat);
End;


Function TUixCodeEdit.HasPartialDate : Boolean;
Var
  sDateFormat : String;
Begin
  sDateFormat := DateFormat;

  Result := IncludeDate And StringIsDate(Copy(Text, 1, Length(sDateFormat)), sDateFormat)
End;


Function TUixCodeEdit.HasDateTime : Boolean;
Begin
  Result := HasDate And HasTime;
End;


Procedure TUixCodeEdit.InternalChangeDelegate(oSender: TObject);
Begin
  Change;

  If Assigned(FChangeDelegate) Then
    FChangeDelegate(Self);
End;


Procedure TUixCodeEdit.InternalExitDelegate(oSender : TObject);
Begin
  If IsValidationModeCurrency And (Text <> '') Then
    Text := CurrencyToString(StringToCurrency(StringStrip(Text, ',')));

  If Assigned(FLoseFocusDelegate) Then
    FLoseFocusDelegate(Self);
End;


Function TUixCodeEdit.GetEditPart : Cardinal;
Begin
  If SystemIsWindowsVista Then
    Result := EP_EDITBORDER_NOSCROLL
  Else
    Result := EP_EDITTEXT;
End;


Function TUixCodeEdit.GetEditState : Cardinal;
Begin
  If SystemIsWindowsVista Then
  Begin
    If Focused Then
      Result := EPSN_FOCUSED
    Else
      Result := EPSN_NORMAL
  End
  Else
  Begin
    If Focused Then
      Result := ETS_FOCUSED
    Else
      Result := ETS_NORMAL;
  End;
End;


Procedure TUixCodeEdit.AdjustContentRect(Var aRect : TRect);
Begin
  If SystemIsWindowsVista Then
    InflateRect(aRect, -2, -2)
  Else
    InflateRect(aRect, -1, -1);
End;


Procedure TUixCodeEdit.CreateParams(Var Params: TCreateParams);
Begin
  Inherited;

  If UseThemes Then
  Begin
    Params.Style := Params.Style And Not WS_BORDER;
    Params.ExStyle := Params.ExStyle And Not WS_EX_CLIENTEDGE;
  End;
End;


Procedure TUixCodeEdit.WMNCCalcSize(Var Message: TWMNCCalcSize);
Var
  hT : HTHEME;
  hD : HDC;
  aRect : TRect;
Begin
  Inherited;

  If UseThemes Then
  Begin
    hT := OpenThemeData(Handle, 'Edit');
    hD := GetWindowDC(Handle);
    Try
      aRect := Message.CalcSize_Params^.rgrc[0];
      GetThemeBackgroundContentRect(hT, hD, GetEditPart, GetEditState, aRect, @aRect);
      AdjustContentRect(aRect);
      Message.CalcSize_Params^.rgrc[0] := aRect;
    Finally
      CloseThemeData(hT);
      ReleaseDC(Handle, hD);
    End;
  End;
End;


Procedure TUixCodeEdit.WMNCPaint(Var Message: TMessage);
Var
  hT : HTHEME;
  hD : HDC;
  aRect : TRect;
Begin
  Inherited;

  If UseThemes Then
  Begin
    hT := OpenThemeData(Handle, 'Edit');
    hD := GetWindowDC(Handle);
    Try
      aRect := wp_graphics.Rect(0, 0, Width, Height);
      DrawThemeBackground(hT, hD, GetEditPart, GetEditState, aRect, Nil);
      Invalidate;
    Finally
      CloseThemeData(hT);
      ReleaseDC(Handle, hD);
    End;
  End;
End;


Procedure TUixCodeEdit.Resize;
Begin
  Inherited;
  Invalidate;
End;


Procedure TUixCodeEdit.CMEnter(Var Message: TCMGotFocus);
Begin
  If FAutoSelect And (Not (csLButtonDown In ControlState)) And (SelStart = SelEnd) Then
  Begin
    SelStart := Highlighter.Mask.LeadingFixedLength;
    SelEnd := Length(Text);
  End;

  Inherited;

  AdjustSelectionStart;

  Perform(WM_NCPAINT, 0, 0);
  Invalidate;
End;


Procedure TUixCodeEdit.DoExit;
Begin
  Inherited;

  HideBalloon;

  Perform(WM_NCPAINT, 0, 0);
  Invalidate;
End;


Function TUixCodeEdit.GetText: String;
Begin
  Result := StringTrimSet(Inherited Text, setVertical);
End;


{$IFDEF VER130}
Function TUixCodeEdit.GetHighlighter: TUixCodeHighlighter;
Begin
  Result := TUixCodeHighlighter(Inherited Highlighter);
End;
{$ELSE}
Function TUixCodeEdit.GetHighlighter: TUixUnicodeCodeHighlighter;
Begin
  Result := TUixUnicodeCodeHighlighter(Inherited Highlighter);
End;
{$ENDIF}


Procedure TUixCodeEdit.SetMask(Const Value: String);
Var
  sText : String;
Begin
  If FMask.Format <> Value Then
  Begin
    sText := FMask.Unformatted(Text);
    FMask.Format := Value;
    Text := FMask.Formatted(sText);

    Complete;
    Prepare;
  End;
End;


Function TUixCodeEdit.GetMinLength: Integer;
Begin
  Result := FMask.MinLength;
End;


Procedure TUixCodeEdit.SetMinLength(Const Value: Integer);
Begin
  FMask.MinLength := Value;
  Prepare;
End;


Procedure TUixCodeEdit.SetMaxLength(Const Value: Integer);
Begin
  FMask.MaxLength := Value;
  Prepare;
End;


Function TUixCodeEdit.GetMaxLength: Integer;
Begin
  Result := FMask.MaxLength;
End;


Function TUixCodeEdit.GetFixedLength: Integer;
Begin
  Result := FMask.FixedCount;
End;


Function TUixCodeEdit.GetEnforceCapitals : Boolean;
Begin
  Result := FMask.Capitals;
End;


Procedure TUixCodeEdit.SetEnforceCapitals(Const Value : Boolean);
Begin
  FMask.Capitals := Value;

  Prepare;
End;


Procedure TUixCodeEdit.SetCaretXYEx(bCallEnsureCursorPos: Boolean; aValue: TBufferCoord);
Begin
  If ((aValue.Char <> CaretX) Or (aValue.Line <> CaretY)) Then
  Begin
    Inherited;

    If Assigned(FRepositionDelegate) Then
      FRepositionDelegate(Self);
  End
  Else
  Begin
    Inherited;
  End;
End;


Procedure TUixCodeEdit.SetReadOnly(Value : Boolean);
Begin
  Inherited;

  Prepare;
End;


Function TUixCodeEdit.GetColor: TColor;
Begin
  Result := Inherited Color;
End;


Procedure TUixCodeEdit.SetColor(Const Value: TColor);
Begin
  Inherited Color := Value;

  RefreshColor;
End;


Function TUixCodeEdit.GetParentColor: Boolean;
Begin
  Result := Inherited ParentColor;
End;


Procedure TUixCodeEdit.SetParentColor(Const Value: Boolean);
Begin
  Inherited ParentColor := Value;

  RefreshColor;
End;


Procedure TUixCodeEdit.RefreshColor;
Begin
  Highlighter.TextAttributes.Background := Color;
  Highlighter.MaskAttributes.Background := Color;
  Highlighter.RemainderAttributes.Background := Color;
  Highlighter.RequiredAttributes.Background := Color;
  Highlighter.CustomAttributes.Background := Color;
  Highlighter.BeyondAttributes.Background := Color;
  Invalidate;
End;


Function TUixCodeEdit.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixCodeEdit.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixCodeEdit.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixCodeEdit.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixCodeEdit.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixCodeEdit.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixCodeEdit.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixCodeEdit.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Function TUixCodeEdit.GetBolded: Boolean;
Begin
  Result := fsBold In Font.Style;
End;


Procedure TUixCodeEdit.SetBolded(Const Value: Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsBold]
  Else
    Font.Style := Font.Style - [fsBold];
End;


Procedure TUixCodeEdit.ApplyMask(Const sText: String);
Var
  aCaretXY : TBufferCoord;
Begin
  Mask := FMask.FormatFromText(sText);
  aCaretXY := CaretXY;
  Text := Copy(FMask.FixedFormat, 1, FMask.LeadingFixedLength);
  CaretXY := aCaretXY;
End;


Function TUixCodeEdit.GetMask: String;
Begin
  Result := FMask.Format;
End;


Function TUixCodeEdit.CanInsertCharacter : Boolean;
Begin
  Result := SelStart >= Highlighter.Mask.LeadingFixedLength;
End;


Function TUixCodeEdit.CanPaste : Boolean;
Begin
  Result := CanInsertCharacter;
End;


Procedure TUixCodeEdit.AdjustSelectionStart;
Var
  iSelStart : Integer;
Begin
  iSelStart := IntegerMax(SelStart, Highlighter.Mask.LeadingFixedLength);

  If SelStart <> iSelStart Then
    SelStart := iSelStart;
End;


Procedure TUixCodeEdit.AdjustSelectionEnd;
Var
  iPosition : Integer;
  iTrailingFixedLength : Integer;
Begin
  iPosition := CaretX - 1;
  iTrailingFixedLength := FMask.TrailingFixedLength(iPosition);

  If (iTrailingFixedLength > 0) And ((iPosition + iTrailingFixedLength) = Length(Text)) Then
    SelEnd := SelEnd + iTrailingFixedLength;
End;


Function TUixCodeEdit.LeadingFixedText: String;
Begin
  Result := FMask.LeadingFixedText;
End;


Function TUixCodeEdit.AllowInsertCharacter(Const cChar : Char) : Boolean;
Begin
  Result := Not CharInSet(cChar, setVertical + [cNull, cBackspace]);
End;

Function TUixCodeEdit.IsValidContent : Boolean;
Var
  sLastError : String;
Begin
  sLastError := LastError;
  Try
    Result := ValidateContent;
  Finally
    LastError := sLastError;
  End;
End;


Function TUixCodeEdit.IsValidLength : Boolean;
Var
  sLastError : String;
Begin
  sLastError := LastError;
  Try
    Result := ValidateLength;
  Finally
    LastError := sLastError;
  End;
End;


Function TUixCodeEdit.GetIsDefaultStringValue : Boolean;
Begin
  Result := (FDefaultStringValue <> '') And (FDefaultStringValue = Text);
End;


Procedure TUixCodeEdit.SetIsDefaultStringValue(Const Value : Boolean);
Begin
  If Value Then
    DefaultStringValue := Text
  Else
    DefaultStringValue := '';
End;


Function TUixCodeEdit.GetDefaultStringValue : String;
Begin
  Result := FDefaultStringValue;
End;


Procedure TUixCodeEdit.SetDefaultStringValue(Const Value : String);
Begin
  FDefaultStringValue := Value;

  Prepare;
End;


Function TUixCodeEdit.GetMandatory : Boolean;
Begin
  Result := FMandatory;
End;


Procedure TUixCodeEdit.SetMandatory(Const Value: Boolean);
Begin
  FMandatory := Value;

  Prepare;

  PrepareMask;
End;


Function TUixCodeEdit.GetAllowsFuture : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FAllowsFuture;
End;


Procedure TUixCodeEdit.SetAllowsFuture(Const Value : Boolean);
Begin
  EnforceValidationModeDateTime;

  FAllowsFuture := Value;
End;


Function TUixCodeEdit.GetTimeOptional : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FTimeOptional;
End;


Procedure TUixCodeEdit.SetTimeOptional(Const Value : Boolean);
Begin
  EnforceValidationModeDateTime;

  FTimeOptional := Value;

  PrepareMask;
End;


Function TUixCodeEdit.GetAllowsPast : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FAllowsPast;
End;


Procedure TUixCodeEdit.SetAllowsPast(Const Value : Boolean);
Begin
  EnforceValidationModeDateTime;

  FAllowsPast := Value;
End;


Function TUixCodeEdit.GetIncludeTime : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FIncludeTime;
End;


Procedure TUixCodeEdit.SetIncludeTime(Const Value: Boolean);
Begin
  EnforceValidationModeDateTime;

  FIncludeTime := Value;

  PrepareDateTimeMask;
End;


Function TUixCodeEdit.GetIncludeDate : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FIncludeDate;
End;


Procedure TUixCodeEdit.SetIncludeDate(Const Value: Boolean);
Begin
  EnforceValidationModeDateTime;

  FIncludeDate := Value;

  PrepareDateTimeMask;
End;


Function TUixCodeEdit.GetIncludeDays : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FIncludeDays;
End;


Procedure TUixCodeEdit.SetIncludeDays(Const Value: Boolean);
Begin
  EnforceValidationModeDateTime;

  FIncludeDays := Value;

  PrepareDateTimeMask;
End;


Function TUixCodeEdit.GetIncludeMonths : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FIncludeMonths;
End;


Procedure TUixCodeEdit.SetIncludeMonths(Const Value: Boolean);
Begin
  EnforceValidationModeDateTime;

  FIncludeMonths := Value;

  PrepareDateTimeMask;
End;


Function TUixCodeEdit.GetIncludeYears : Boolean;
Begin
  EnforceValidationModeDateTime;

  Result := FIncludeYears;
End;


Procedure TUixCodeEdit.SetIncludeYears(Const Value: Boolean);
Begin
  EnforceValidationModeDateTime;

  FIncludeYears := Value;

  PrepareDateTimeMask;
End;


Function TUixCodeEdit.GetIncludeMilliSeconds : Boolean;
Begin
  EnforceValidationModeDuration;

  Result := FIncludeMilliSeconds;
End;


Procedure TUixCodeEdit.SetIncludeMilliSeconds(Const Value : Boolean);
Begin
  EnforceValidationModeDuration;

  FIncludeMilliSeconds := Value;

  If IncludeMilliSeconds And IncludeMinutes Then
    IncludeSeconds := True;

  PrepareDurationMask;
End;


Function TUixCodeEdit.GetIncludeSeconds : Boolean;
Begin
  EnforceValidationModeDuration;

  Result := FIncludeSeconds;
End;


Procedure TUixCodeEdit.SetIncludeSeconds(Const Value : Boolean);
Begin
  EnforceValidationModeDuration;

  FIncludeSeconds := Value;

  If IncludeSeconds Then
  Begin
    If IncludeHours Then
      IncludeMinutes := True;
  End
  Else
  Begin
    If IncludeMinutes Then
      IncludeMilliSeconds := False;
  End;

  PrepareDurationMask;
End;


Function TUixCodeEdit.GetIncludeMinutes : Boolean;
Begin
  EnforceValidationModeDuration;

  Result := FIncludeMinutes;
End;


Procedure TUixCodeEdit.SetIncludeMinutes(Const Value : Boolean);
Begin
  EnforceValidationModeDuration;

  FIncludeMinutes := Value;

  If Not IncludeMinutes Then
  Begin
    If IncludeHours Then
      IncludeSeconds := False;
  End;

  PrepareDurationMask;
End;


Function TUixCodeEdit.GetIncludeHours : Boolean;
Begin
  EnforceValidationModeDuration;

  Result := FIncludeHours;
End;


Procedure TUixCodeEdit.SetIncludeHours(Const Value : Boolean);
Begin
  EnforceValidationModeDuration;

  FIncludeHours := Value;

  If IncludeHours And Not IncludeMinutes Then
  Begin
    IncludeSeconds := False;
    IncludeMilliSeconds := False;
  End;

  PrepareDurationMask;
End;


Function TUixCodeEdit.GetHasTime : Boolean;
Var
  sTime : String;
Begin
  EnforceValidationModeDateTime;

  Result := IncludeTime;

  If Result Then
  Begin
    If IncludeDate Then
      sTime := Copy(Text, Length(DateFormat) + 2, MaxInt)
    Else
      sTime := Text;

    Result := Not (StringTrimWhitespace(StringStrip(sTime, ':')) = '') And StringIsTime(sTime);
  End;
End;


Procedure TUixCodeEdit.SetHasTime(Const bValue : Boolean);
Begin
  EnforceValidationModeDateTime;

  If Not bValue And (DisplayDateTime(ValueAsDateTime) <> '') Then
    Text := fsl_utilities.DateTimeFormat(ValueAsDateTime, DateFormat);
End;


Function TUixCodeEdit.GetExtendedHours : Boolean;
Begin
  EnforceValidationModeDuration;

  Result := FExtendedHours;
End;


Procedure TUixCodeEdit.SetExtendedHours(Const Value: Boolean);
Begin
  EnforceValidationModeDuration;

  FExtendedHours := Value;

  PrepareDurationMask;
End;


Function TUixCodeEdit.GetMinDateTimeValue : TDateTime;
Begin
  EnforceValidationModeDateTime;

  Result := FMinDateTimeValue;
End;


Procedure TUixCodeEdit.SetMinDateTimeValue(Const Value : TDateTime);
Begin
  EnforceValidationModeDateTime;

  FMinDateTimeValue := Value;
End;


Function TUixCodeEdit.GetMaxDateTimeValue : TDateTime;
Begin
  EnforceValidationModeDateTime;

  Result := FMaxDateTimeValue;
End;


Procedure TUixCodeEdit.SetMaxDateTimeValue(Const Value : TDateTime);
Begin
  EnforceValidationModeDateTime;

  FMaxDateTimeValue := Value;
End;


Function TUixCodeEdit.GetMinDurationValue : TDurationMS;
Begin
  EnforceValidationModeDuration;

  Result := FMinDurationValue;
End;


Procedure TUixCodeEdit.SetMinDurationValue(Const Value : TDurationMS);
Begin
  EnforceValidationModeDuration;

  FMinDurationValue := Value;
End;


Function TUixCodeEdit.GetMaxDurationValue : TDurationMS;
Begin
  EnforceValidationModeDuration;

  Result := FMaxDurationValue;
End;


Procedure TUixCodeEdit.SetMaxDurationValue(Const Value : TDurationMS);
Begin
  EnforceValidationModeDuration;

  FMaxDurationValue := Value;
End;


Function TUixCodeEdit.GetMinCurrencyValue : TCurrency;
Begin
  EnforceValidationModeCurrency;

  Result := FMinCurrencyValue;
End;


Procedure TUixCodeEdit.SetMinCurrencyValue(Const Value : TCurrency);
Begin
  EnforceValidationModeCurrency;

  FMinCurrencyValue := Value;
End;


Function TUixCodeEdit.GetMaxCurrencyValue : TCurrency;
Begin
  EnforceValidationModeCurrency;

  Result := FMaxCurrencyValue;
End;


Procedure TUixCodeEdit.SetMaxCurrencyValue(Const Value : TCurrency);
Begin
  EnforceValidationModeCurrency;

  FMaxCurrencyValue := Value;
End;


Function TUixCodeEdit.GetDenominationCurrencyValue : TCurrency;
Begin
  EnforceValidationModeCurrency;

  Result := FDenominationCurrencyValue;
End;


Procedure TUixCodeEdit.SetDenominationCurrencyValue(Const Value : TCurrency);
Begin
  EnforceValidationModeCurrency;

  FDenominationCurrencyValue := Value;
End;


Function TUixCodeEdit.GetMinIntegerValue : Int64;
Begin
  EnforceValidationModeInteger;

  Result := FMinIntegerValue;
End;


Procedure TUixCodeEdit.SetMinIntegerValue(Const Value : Int64);
Begin
  EnforceValidationModeInteger;

  FMinIntegerValue := Value;

  PrepareIntegerMask;
End;


Function TUixCodeEdit.GetMaxIntegerValue : Int64;
Begin
  EnforceValidationModeInteger;

  Result := FMaxIntegerValue;
End;


Procedure TUixCodeEdit.SetMaxIntegerValue(Const Value : Int64);
Begin
  EnforceValidationModeInteger;

  FMaxIntegerValue := Value;

  PrepareIntegerMask;
End;


Function TUixCodeEdit.GetDefaultIntegerValue : Int64;
Begin
  EnforceValidationModeInteger;

  Result := FDefaultIntegerValue;
End;


Procedure TUixCodeEdit.SetDefaultIntegerValue(Const Value : Int64);
Begin
  EnforceValidationModeInteger;

  FDefaultIntegerValue := Value;
End;


Function TUixCodeEdit.GetHasDefaultIntegerValue : Boolean;
Begin
  EnforceValidationModeInteger;

  Result := FHasDefaultIntegerValue;
End;


Procedure TUixCodeEdit.SetHasDefaultIntegerValue(Const Value : Boolean);
Begin
  EnforceValidationModeInteger;

  FHasDefaultIntegerValue := Value;
End;


Function TUixCodeEdit.GetExtendedDecimalPlaces : Integer;
Begin
  EnforceValidationModeExtended;

  Result := FExtendedDecimalPlaces;
End;


Procedure TUixCodeEdit.SetExtendedDecimalPlaces(Const Value : Integer);
Begin
  EnforceValidationModeExtended;

  FExtendedDecimalPlaces := Value;

  PrepareExtendedMask;
End;


Function TUixCodeEdit.GetMaxExtendedValue : Extended;
Begin
  EnforceValidationModeExtended;

  Result := FMaxExtendedValue;
End;


Procedure TUixCodeEdit.SetMaxExtendedValue(Const Value : Extended);
Begin
  EnforceValidationModeExtended;

  FMaxExtendedValue := Value;

  PrepareExtendedMask;
End;


Function TUixCodeEdit.GetMinExtendedValue : Extended;
Begin
  EnforceValidationModeExtended;

  Result := FMinExtendedValue;
End;


Procedure TUixCodeEdit.SetMinExtendedValue(Const Value : Extended);
Begin
  EnforceValidationModeExtended;

  FMinExtendedValue := Value;

  PrepareExtendedMask;
End;


Function TUixCodeEdit.GetValueAsDateTimeOffset : TDateTimeOffset;
Var
  bValid : Boolean;
Begin
  EnforceValidationModeDateTime;

  If Empty Then
    bValid := False
  Else If HasDate Then
    bValid := ToDateTimeOffset(Text, DateTimeFormat, Result)
  Else
    bValid := False;

  If Not bValid Then
    Result := DateTimeOffsetZero;
End;


Procedure TUixCodeEdit.SetValueAsDateTimeOffset(Const Value : TDateTimeOffset);
Begin
  EnforceValidationModeDateTime;

  Text := DisplayDateTime(Value);
End;


Function TUixCodeEdit.GetValueAsDateTime : TDateTime;
Var
  bValid : Boolean;
Begin
  EnforceValidationModeDateTime;

  If Empty Then
    bValid := False
  Else If HasDate Then
    bValid := ToDateTime(Text, DateTimeFormat, Result)
  Else
    bValid := False;

  If Not bValid Then
    Result := 0;
End;


Procedure TUixCodeEdit.SetValueAsDateTime(Const Value : TDateTime);
Begin
  EnforceValidationModeDateTime;

  Text := DisplayDateTime(Value);
End;


Function TUixCodeEdit.GetValueAsDuration : TDurationMS;
Var
  sText : String;
Begin
  EnforceValidationModeDuration;

  sText := NormaliseDuration;

  If StringIsDuration(sText) Then
    Result := StringToDuration(sText)
  Else
    Result := 0;
End;


Procedure TUixCodeEdit.SetValueAsDuration(Const Value : TDurationMS);
Begin
  EnforceValidationModeDuration;

  Text := DisplayDuration(Value);
End;


Function TUixCodeEdit.GetValueAsString: String;
Begin
  EnforceValidationModeString;

  Result := Text;
End;


Procedure TUixCodeEdit.SetValueAsString(Const Value: String);
Begin
  EnforceValidationModeString;

  Text := Value;
End;


Function TUixCodeEdit.GetValueAsCurrency : TCurrency;
Begin
  EnforceValidationModeCurrency;

  If Text = '' Then
    Result := 0.0
  Else
    Result := StringToCurrency(StringStrip(Text, ','));
End;


Procedure TUixCodeEdit.SetValueAsCurrency(Const Value : TCurrency);
Begin
  EnforceValidationModeCurrency;

  Text := CurrencyToString(Value);
End;


Function TUixCodeEdit.GetValueAsInteger64 : Int64;
Begin
  Result := StrToInt64Def(Text, FDefaultIntegerValue);
End;


Procedure TUixCodeEdit.SetValueAsInteger64(Const Value : Int64);
Begin
  If Not FHasDefaultIntegerValue Or (Value <> DefaultIntegerValue) Then
    Text := IntegerToString(Value);
End;


Function TUixCodeEdit.GetValueAsInteger32 : LongInt;
Var
  iInt64Value : Int64;
Begin
  iInt64Value := ValueAsInteger64;

  If Not IntegerBetween(Low(LongInt), iInt64Value, High(LongInt)) Then
    Error('GetValueAsInteger32', 'LongInt value out of bounds.');

  Result := iInt64Value;
End;


Procedure TUixCodeEdit.SetValueAsInteger32(Const Value : Integer);
Begin
  ValueAsInteger64 := Value;
End;


Function TUixCodeEdit.GetValueAsCardinal : Cardinal;
Var
  iInt64Value : Int64;
Begin
  iInt64Value := ValueAsInteger64;

  If Not IntegerBetween(Low(Cardinal), iInt64Value, High(Cardinal)) Then
    Error('GetValueAsCardinal', 'Cardinal value out of bounds.');

  Result := iInt64Value;
End;


Procedure TUixCodeEdit.SetValueAsCardinal(Const Value : Cardinal);
Begin
  ValueAsInteger64 := Value;
End;


Function TUixCodeEdit.GetValueAsExtended : Extended;
Begin
  EnforceValidationModeExtended;

  If Text = '' Then
    Result := 0.0
  Else
    Result := StrToFloatDef(Text, 0);
End;


Procedure TUixCodeEdit.SetValueAsExtended(Const Value : Extended);
Begin
  EnforceValidationModeExtended;

  Text := DisplayExtended(Value);
End;


Procedure TUixCodeEdit.SetReadBackgroundColour(Const Value: TColour);
Begin
  FReadBackgroundColour := Value;
  Prepare;
End;


Procedure TUixCodeEdit.SetWriteBackgroundColour(Const Value: TColour);
Begin
  FWriteBackgroundColour := Value;
  Prepare;
End;


Function TUixCodeEditList.GetEditByIndex(Const iIndex : Integer): TUixCodeEdit;
Begin
  Result := TUixCodeEdit(Items[iIndex]);
End;


Procedure TUixCodeEditList.SetEditByIndex(Const iIndex : Integer; Const Value : TUixCodeEdit);
Begin
  Items[iIndex] := Value;
End;


{$IFNDEF VER130}
{ TUixUnicodeCodeHighlighter }

constructor TUixUnicodeCodeHighlighter.Create(oOwner: TComponent);
begin
  inherited;

  FTextAttributes := TSynHighLighterAttributes.Create('Text');
  AddAttribute(FTextAttributes);

  FBeyondAttributes := TSynHighLighterAttributes.Create('Beyond');
  FBeyondAttributes.Foreground := clBtnText;
  AddAttribute(FBeyondAttributes);

  FInvalidAttributes := TSynHighLighterAttributes.Create('Invalid');
  FInvalidAttributes.Style := [];
  FInvalidAttributes.Background := clRed;
  FInvalidAttributes.Foreground := clWhite;
  AddAttribute(FInvalidAttributes);

  FMaskAttributes := TSynHighLighterAttributes.Create('Fixed');
  FMaskAttributes.Style := [];
  AddAttribute(FMaskAttributes);

  FRemainderAttributes := TSynHighLighterAttributes.Create('Remainder');
  FRemainderAttributes.Style := [];
  FRemainderAttributes.Foreground := clGrayText;
  AddAttribute(FRemainderAttributes);

  FRequiredAttributes := TSynHighLighterAttributes.Create('Required');
  FRequiredAttributes.Style := [fsUnderline];
  AddAttribute(FRequiredAttributes);

  FCustomAttributes := TSynHighlighterAttributes.Create('Custom');
  AddAttribute(FCustomAttributes);
end;


Destructor TUixUnicodeCodeHighlighter.Destroy;
Begin
  FUnicodeMask.Free;

  Inherited;
End;


Function TUixUnicodeCodeHighlighter.CurrentToken: TUixCodeToken;
Begin
  If Index < 1 Then
    Result := UixCodeTokenText
  Else If FUnicodeMask.Fixed(Index) And (Text[Index] = FUnicodeMask.FixedFormat[Index]) Then
    Result := UixCodeTokenFixed
  Else If CharInSet(Text[Index], FUnicodeMask.Allowed(Index)) Then
    Result := UixCodeTokenText
  Else
    Result := UixCodeTokenInvalid;
End;


Function TUixUnicodeCodeHighlighter.GetDefaultAttribute(Index: Integer): TSynHighlighterAttributes;
Begin
  Case Index Of
    SYN_ATTR_IDENTIFIER : Result := FTextAttributes;
    SYN_ATTR_KEYWORD    : Result := FMaskAttributes;
    SYN_ATTR_WHITESPACE :
    Begin
      If (FUnicodeMask.MaxLength > 0) And TUixCodeEdit(Owner).Focused Then
        Result := FBeyondAttributes
      Else
        Result := FTextAttributes;
    End;
  Else
    Result := Nil;
  End;
End;


Function TUixUnicodeCodeHighlighter.RealLineLength : Integer;
Var
  iFixedFormatLength : Integer;
Begin
  iFixedFormatLength := Length(FUnicodeMask.FixedFormat);
  If iFixedFormatLength > 0 Then
    Result := iFixedFormatLength
  Else
    Result := Length(Text);
End;


Function TUixUnicodeCodeHighlighter.GetEol: Boolean;
Begin
  Result := FValue = '';
End;


Function TUixUnicodeCodeHighlighter.GetIndex: Integer;
Begin
//  Result := Run;
End;


Procedure TUixUnicodeCodeHighlighter.SetIndex(Const Value: Integer);
Begin
//  Run := Value;
End;


Procedure TUixUnicodeCodeHighlighter.SetMask(const Value: TUixCodeMask);
Begin
  FUnicodeMask.Free;
  FUnicodeMask := Value;
End;


Function TUixUnicodeCodeHighlighter.GetMask: TUixCodeMask;
Begin
  Result := FUnicodeMask;
End;


Function TUixUnicodeCodeHighlighter.GetRange: Pointer;
Begin
  Result := Pointer(FToken);
End;


Procedure TUixUnicodeCodeHighlighter.SetRange(Value: Pointer);
Begin
  FToken := TUixCodeToken(Value);
End;


Function TUixUnicodeCodeHighlighter.GetText: String;
Begin
//  Result := fLine;
End;


Procedure TUixUnicodeCodeHighlighter.SetText(const Value: String);
Begin
//  fLineStr := Value;
End;


Function TUixUnicodeCodeHighlighter.GetToken: String;
Begin
  Result := FValue;
End;


Function TUixUnicodeCodeHighlighter.GetTokenAttribute: TSynHighlighterAttributes;
Begin
  Case FToken Of
    UixCodeTokenFixed     : Result := FMaskAttributes;
    UixCodeTokenText      : Result := FTextAttributes;
    UixCodeTokenBeyond    : Result := FBeyondAttributes;
    UixCodeTokenInvalid   : Result := FInvalidAttributes;
    UixCodeTokenRemainder : Result := FRemainderAttributes;
    UixCodeTokenRequired  : Result := FRequiredAttributes;
    UixCodeTokenCustom    : Result := FCustomAttributes;
  Else
    Result := Nil;
  End;
End;


Function TUixUnicodeCodeHighlighter.GetTokenKind: Integer;
Begin
  Result := Integer(FToken);
End;


Procedure TUixUnicodeCodeHighlighter.Next;
Var
  iStart : Integer;
  aAttributes : TSynHighlighterAttributes;
Begin
  //FTokenPos := Index;
  If Index >= Length(Text) Then
  Begin
    If Index <= FUnicodeMask.MinLength Then
    Begin
      FValue := Copy(FUnicodeMask.FixedFormat, Index, FUnicodeMask.MinLength - Index + 1);
      Index := FUnicodeMask.MinLength + 1;
      FToken := UixCodeTokenRequired;
    End
    Else
    Begin
      FValue := Copy(FUnicodeMask.FixedFormat, Index, MaxInt);
      Index := FUnicodeMask.FixedLength + 1;
      FToken := UixCodeTokenRemainder;
    End;
  End
  Else If (Index > FUnicodeMask.MaxLength) And (FUnicodeMask.MaxLength > 0) Then
  Begin
    FValue := Copy(Text, Index, MaxInt);
    Index := Length(Text) + 1;
    FToken := UixCodeTokenInvalid;
  End
  Else
  Begin
    iStart := Index;

    If (Index > FUnicodeMask.Count) Then
    Begin
      FValue := Copy(Text, Index, MaxInt);
      Index := Length(Text) + 1;
      FToken := UixCodeTokenText;
    End
    Else
    Begin
      FToken := CurrentToken;

      Index := Index + 1;
      While (Index < Length(Text)) And (Index <= FUnicodeMask.Count) And (FToken = CurrentToken) Do
        Index := Index + 1;

      FValue := Copy(Text, iStart, Index - iStart);
    End;

    If Assigned(FOnCustom) Then
    Begin
      aAttributes := GetTokenAttribute;

      If Assigned(aAttributes) Then
      Begin
        FCustomAttributes.Assign(GetTokenAttribute);

        FOnCustom(Self, iStart, FValue, FCustomAttributes);

        // FValue may be reduced and transformed here to break the whole text into
        // different coloured sections.

        Index := iStart + Length(FValue);
        FToken := UixCodeTokenCustom;
      End;
    End;
  End;

  Inherited;
End;


Procedure TUixUnicodeCodeHighlighter.ResetRange;
Begin
  FToken := UixCodeTokenText;
End;
{$ENDIF}



Constructor TUixComboBox.Create(oOwner: TComponent);
Begin
  Inherited;

  FInvalidBackgroundColour := clWindow;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  FBalloonAudible := False;
End;


Destructor TUixComboBox.Destroy;
Begin

  Inherited;
End;


Function TUixComboBox.GetStrict : Boolean;
Begin
  Result := Style = csDropDownList;
End;


Procedure TUixComboBox.SetStrict(Const Value : Boolean);
Begin
  If Value Then
    Style := csDropDownList
  Else
    Style := csDropDown;
End;


Function TUixComboBox.GetValue : Integer;
Begin
  Result := ItemIndex;
End;


Procedure TUixComboBox.SetValue(Const Value : Integer);
Begin
  ItemIndex := Value;
  Prepare;
End;


Procedure TUixComboBox.InvalidValue;
Begin
  Value := -1;
End;


Procedure TUixComboBox.ShowBalloon;
Begin
  HideBalloon;

  FBalloon := TUixBalloon.CreateNew(Self);
  FBalloon.Parent := Self;
  FBalloon.Title := FTitle;
  FBalloon.Message := FLastError;
  FBalloon.BalloonTypeError;
  FBalloon.AnchorPositionTopLeft;
  FBalloon.TimeoutDuration := 4000;
  FBalloon.UseAudibleNotification := FBalloonAudible;
  FBalloon.Control := Self;
  FBalloon.ControlRelativeHorizontalPositionCenter;
  FBalloon.ControlRelativeVerticalPositionBottom;
  FBalloon.ShowBalloon;
End;


Procedure TUixComboBox.Prepare;
Begin
  If Not CustomBackgoundHandling Then
  Begin
    If ReadOnly Then
      Color := clBtnFace // Some parents are not clBtnFace
    Else If Enabled And Mandatory And Empty Then
      Color := InvalidBackgroundColour
    Else
      Color := clWindow;
  End;
End;


Procedure TUixComboBox.DoExit;
Begin
  Inherited;

  HideBalloon;
End;


Procedure TUixComboBox.HideBalloon;
Begin
  If Assigned(FBalloon) Then
  Begin
    FBalloon.Release;
    FBalloon := Nil;
  End;
End;


Function TUixComboBox.Valid : Boolean;
Var
  iLoop : Integer;
  oStrings : TFslStringList;
Begin
  LastError := '';

  Result := Not Mandatory And (Text = '');

  If Not Result Then
  Begin
    Result := (Length(Text) > 0) Or (MinLength = 0);

    If Not Result Then
      LastError := Title + ' is a mandatory field and must be entered.'
    Else
    Begin
      Result := Length(Text) >= MinLength;

      If Not Result Then
        LastError := Title + ' must be at least ' + IntegerToString(MinLength) + ' characters. Please complete the required field.'
      Else
      Begin
        Result := (MaxLength = 0) Or (Length(Text) <= MaxLength);

        If Not Result Then
          LastError := Title + ' must be no more than ' + IntegerToString(MaxLength) + ' characters. Please reduce the characters in the field.'
      End;
    End;
  End;

  If Result Then
  Begin
    Result := Empty;

    If Not Result Then
    Begin
      Result := Not Strict;

      If Not Result Then
      Begin
        oStrings := TFslStringList.Create;
        Try
          For iLoop := 0 To Items.Count - 1 Do
            oStrings.Add(Items[iLoop]);

          Result := oStrings.ExistsByValue(Text);
        Finally
          oStrings.Free;
        End;
      End;

      If Not Result Then
        LastError := Title + ' must be contained in the available list.';
    End;
  End;
End;


Function TUixComboBox.Empty : Boolean;
Begin
  If Strict Then
    Result := Value < 0
  Else
    Result := Text = '';
End;


Function TUixComboBox.Full : Boolean;
Begin
  Result := (MaxLength > 0) And (Length(Text) = MaxLength);
End;


Function TUixComboBox.AddValue(Const sValue : String) : Integer;
Var
  iLength : Integer;
Begin
  Result := Items.Add(sValue);

  iLength := Length(sValue);

  If IntegerBetweenExclusive(0, MaxLength, iLength) Then
    MaxLength := iLength;

  If IntegerBetweenExclusive(0, iLength, MinLength) Then
    MinLength := iLength;
End;


Procedure TUixComboBox.RemoveValue(Const sValue : String);
Var
  iIndex : Integer;
Begin
  iIndex := Items.IndexOf(sValue);
  If iIndex >= 0 Then
    Items.Delete(iIndex);
End;


Procedure TUixComboBox.AddValues(Const aValues : Array Of String);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aValues) To High(aValues) Do
    AddValue(aValues[iLoop]);
End;


Procedure TUixComboBox.AddValues(oValues : TFslStringList);
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To oValues.Count - 1 Do
    AddValue(oValues[iLoop]);
End;


Procedure TUixComboBox.ClearValues;
Begin
  Items.Clear;
End;


Function TUixComboBox.GetReadOnly : Boolean;
Begin
  Result := Not Enabled;
End;


Procedure TUixComboBox.SetReadOnly(Const Value : Boolean);
Begin
  Enabled := Not Value;
  Prepare;
End;


Procedure TUixComboBox.SetMinLength(Const Value: Integer);
Begin
  FMinLength := Value;
  Prepare;
End;


Function TUixComboBox.GetNotReadOnly: Boolean;
Begin
  Result := Inherited Enabled;
End;


Procedure TUixComboBox.SetNotReadOnly(Const Value: Boolean);
Begin
  Inherited Enabled := Value;

  Prepare;
End;


Function TUixComboBox.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixComboBox.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixComboBox.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixComboBox.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixComboBox.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixComboBox.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixComboBox.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixComboBox.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixComboBox.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixComboBox.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixComboBox.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixComboBox.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixComboBox.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixComboBox.DropdownAll;
Begin
  DropdownCount := ItemCount;
End;

{$IFDEF VER130}
Function TUixComboBox.GetItemCount: Integer;
Begin
  Result := Items.Count;
End;
{$ENDIF}

Function TUixComboBox.IndexByValue(Const sValue: String): Integer;
Begin
  Result := Items.IndexOf(sValue);
End;


Procedure TUixComboBox.Error(Const sMethod, sMessage: String);
Begin
  Raise EFslException.Create(Self, sMethod, sMessage);
End;


Function StringIsKeyboard(Const cValue : Char) : Boolean;
Begin
  Result := CharInSet(cValue, setKeyboard);
End;

Procedure TUixComboBox.KeyPress(Var cKey : Char);
Begin
  Inherited;

  If Enabled Then
  Begin
    LastError := '';

    HideBalloon;

    If StringIsKeyboard(cKey) Then
    Begin
      If Full And (SelLength = 0) Then
        LastError := Title + ' cannot exceed ' + IntegerToString(MaxLength) + ' characters.';

      If LastError <> '' Then
      Begin
        ShowBalloon;

        cKey := #0;
      End;
    End;
  End;
End;


Function TUixComboBox.GetMandatory : Boolean;
Begin
  Result := MinLength > 0;
End;


Procedure TUixComboBox.SetMandatory(Const Value : Boolean);
Begin
  MinLength := Ord(Value);

  Prepare;
End;


Procedure TUixComboBox.Change;
Begin
  Inherited;

  Prepare;
End;


Procedure TUixComboBox.DeleteByIndex(Const iIndex: Integer);
Begin
  Items.Delete(iIndex);
End;


Function TUixComboBox.GetInvalidBackgroundColour: TColour;
Begin
  Result := FInvalidBackgroundColour;
End;


Procedure TUixComboBox.SetInvalidBackgroundColour(Const Value : TColour);
Begin
  FInvalidBackgroundColour := Value;

  Prepare;
End;

Const
  EDGEBORDER_UNIVERSAL = [Low(TEdgeBorder)..High(TEdgeBorder)];


Constructor TUixToolBar.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  ShowCaptions := False;
  FGradientType := UixToolBarGradientTypeNone;
  OnCustomDraw := CustomDrawDelegate;
End;


Destructor TUixToolbar.Destroy;
Begin
  Inherited;
End;


Function TUixToolBar.ButtonClass: TUixToolButtonClass;
Begin
  Result := TUixToolButton;
End;


Function TUixToolBar.ButtonNew: TUixToolButton;
Begin
  Result := ButtonClass.Create(Self);
End;


Function TUixToolBar.SeparatorNew : TUixToolButton;
Begin
  Result := ButtonNew;
  Result.StyleSeparator;
  Result.Width := 8;
End;


Function TUixToolBar.DropDownNew: TUixToolButton;
Begin
  Result := ButtonNew;
  Result.StyleDropDown;
End;


Function TUixToolBar.CheckNew: TUixToolButton;
Begin
  Result := ButtonNew;
  Result.StyleCheck;
End;


Function TUixToolBar.AddSeparator : TUixToolButton;
Begin
  Result := SeparatorNew;
End;


Function TUixToolBar.AddButton(Const sCaption : String; aEvent : TNotifyEvent; Const iImage : Integer) : TUixToolButton;
Begin
  Result := ButtonNew;
  Result.Caption := sCaption;

  If Not ShowCaptions Then
    Result.Hint := sCaption;

  Result.OnClick := aEvent;
  Result.ImageIndex := iImage;
End;


Function TUixToolBar.AddDropDown(Const sCaption: String; oPopup: TUixPopupMenu; Const iImage: Integer): TUixToolButton;
Begin
  Result := DropDownNew;
  Result.Caption := sCaption;

  If Not ShowCaptions Then
    Result.Hint := sCaption;

  Result.ImageIndex := iImage;
  Result.DropDownMenu := oPopup;
End;


Function TUixToolbar.AddCheck(Const sCaption: String; aOnClick: TNotifyEvent; iImage: Integer): TUixToolButton;
Begin
  Result := CheckNew;

  Result.Caption := sCaption;

  If Not ShowCaptions Then
    Result.Hint := sCaption;

  Result.ImageIndex := iImage;
  Result.OnClick := aOnClick;
End;


Function TUixToolBar.GetButton(Const iIndex: Integer): TUixToolButton;
Begin
  Result := TUixToolButton(Inherited Buttons[iIndex]);
End;


Procedure TUixToolBar.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixToolBar.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixToolBar.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixToolBar.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixToolBar.AlignTop;
Begin
  Align := alTop;
End;


Constructor TUixToolButton.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

End;


Destructor TUixToolButton.Destroy;
Begin

  Inherited;
End;


Procedure TUixToolButton.Click;
Begin
  If IsStyleDropDown And DropDownOnClick Then
  Begin
    CheckMenuDropDown;
  End
  Else
  Begin
    If IsStyleCheck And Not SystemIsWindowsXP Then
      Down := Not Down;

    Inherited;
  End;
End;


Function TUixToolButton.IsStyleButton : Boolean;
Begin
  Result := Style = tbsButton;
End;


Function TUixToolButton.IsStyleCheck : Boolean;
Begin
  Result := Style = tbsCheck;
End;


Function TUixToolButton.IsStyleDivider : Boolean;
Begin
  Result := Style = tbsDivider;
End;


Function TUixToolButton.IsStyleDropDown : Boolean;
Begin
  Result := Style = tbsDropDown;
End;


Function TUixToolButton.IsStyleSeparator: Boolean;
Begin
  Result := Style = tbsSeparator;
End;


Procedure TUixToolButton.MouseDown(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer);
Begin
  Inherited;

  If IsStyleDropDown And Down Then
    Down := False;
End;


Procedure TUixToolButton.StyleButton;
Begin
  Style := tbsButton;
End;


Procedure TUixToolButton.StyleCheck;
Begin
  Style := tbsCheck;
End;


Procedure TUixToolButton.StyleDivider;
Begin
  Style := tbsDivider;
End;


Procedure TUixToolButton.StyleDropDown;
Begin
  Style := tbsDropDown;
End;


Procedure TUixToolButton.StyleSeparator;
Begin
  Style := tbsSeparator;
End;


Procedure TUixToolBar.EdgeInnerRaised;
Begin
  EdgeInner := esRaised;
End;


Procedure TUixToolBar.EdgeOuterLowered;
Begin
  EdgeOuter := esLowered;
End;


Procedure TUixToolBar.EdgeInnerNone;
Begin
  EdgeInner := esNone;
End;


Procedure TUixToolBar.EdgeOuterNone;
Begin
  EdgeOuter := esNone;
End;


Function TUixToolBar.GetEdgeBorderTop: Boolean;
Begin
  Result := ebTop In EdgeBorders;
End;


Procedure TUixToolBar.SetEdgeBorderTop(Const Value: Boolean);
Begin
  If Value Then
    EdgeBorders := EdgeBorders + [ebTop]
  Else
    EdgeBorders := EdgeBorders - [ebTop];
End;


Function TUixToolBar.GetEdgeBorderBottom: Boolean;
Begin
  Result := ebBottom In EdgeBorders;
End;


Procedure TUixToolBar.SetEdgeBorderBottom(Const Value: Boolean);
Begin
  If Value Then
    EdgeBorders := EdgeBorders + [ebBottom]
  Else
    EdgeBorders := EdgeBorders - [ebBottom];
End;


Function TUixToolBar.GetEdgeBorder: Boolean;
Begin
  Result := EDGEBORDER_UNIVERSAL - EdgeBorders = [];
End;

Procedure TUixToolBar.SetEdgeBorder(Const Value: Boolean);
Begin
  If Value Then
    EdgeBorders := EDGEBORDER_UNIVERSAL
  Else
    EdgeBorders := [];
End;


Procedure TUixToolbar.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixToolbar.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixToolbar.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixToolbar.ShuffleTop;
Begin
  Top := 0;
End;


Function TUixToolbarList.GetToolbarByIndex(iIndex: Integer): TUixToolbar;
Begin
  Result := TUixToolbar(Items[iIndex]);
End;


Function TUixToolBar.RequiredHeight: Integer;
Begin
  Result := Height + BorderWidth * 2;
End;


Function TUixToolBar.RequiredWidth: Integer;
Var
  iIndex : Integer;
Begin
  Result := BorderWidth * 2;

  For iIndex := 0 To ButtonCount - 1 Do
  Begin
    If Buttons[iIndex].Visible Then
      Result := Result + Buttons[iIndex].Width;
  End;
End;


Procedure TUixToolBar.GradientHorizontal;
Begin
  FGradientType := UixToolBarGradientTypeHorizontal;
  Invalidate;
End;


Procedure TUixToolBar.GradientNone;
Begin
  FGradientType := UixToolBarGradientTypeNone;
  Invalidate;
End;


Procedure TUixToolBar.GradientVertical;
Begin
  FGradientType := UixToolBarGradientTypeVertical;
  Invalidate;
End;


Procedure TUixToolBar.CustomDrawDelegate(Sender: TToolBar; Const ARect: TRect; Var DefaultDraw: Boolean);
Begin
  Case FGradientType Of
    UixToolBarGradientTypeNone :
    Begin
    End;

    UixToolBarGradientTypeVertical :
    Begin
      ColourGradientVertical(Canvas.Handle, ARect, GradientColourTop, GradientColourBottom);
    End;

    UixToolBarGradientTypeHorizontal :
    Begin
      ColourGradientHorizontal(Canvas.Handle, ARect, GradientColourLeft, GradientColourRight);
    End;
  Else
    Raise EFslException.Create('TUixToolbar', 'CustomDrawDelegate', 'Unhandled toolbar gradient type.');
  End;
End;


Procedure TUixToolBar.Refresh;
Begin
  RecreateWnd;
End;


Const
  SEPARATOR_CAPTION = '-';

  NORMAL_ICON_WIDTH = 16;

  EXTENDED_MARG_X                     = 4;
  EXTENDED_MARG_Y                     = 2;
  EXTENDED_SEPARATOR_LEADING          = 6;
  EXTENDED_GUTTER_WIDTH               = 26;


Constructor TUixMenuItem.Create(oOwner: TComponent);
Begin
  Inherited;

  FUseShortcut := True;

  OnMeasureItem := MeasureMenuItem;
  OnDrawItem := DrawMenuItem;
End;


Destructor TUixMenuItem.Destroy;
Begin
  Inherited;
End;


Function TUixMenuItem.ClassType : TUixMenuItemClass;
Begin
  Result := TUixMenuItemClass(Inherited ClassType);
End;


Procedure TUixMenuItem.MeasureMenuItem(Sender: TObject; aCanvas: TCanvas; Var Width, Height: Integer);
Var
  oMenuItem : TMenuItem;

  sSeparatorHint : String;
  aRectangle : TRect;

  sCaption : String;
  aCaptionRectangle : TRect;
  iCaptionHeight : Integer;
  iCaptionWidth : Integer;

  sHint : String;
  aHintRectangle : TRect;
  iHintHeight : Integer;
  iHintWidth : Integer;

  sShortCut : String;
  aShortCutRectangle : TRect;
  iShortCutWidth : Integer;

  iIconHeight : Integer;
  iIconWidth : Integer;

  bHasGutter : Boolean;
  iGutterWidth : Integer;
Begin
  oMenuItem := TMenuItem(sender);

  If IsSeparator Then
  Begin
    sSeparatorHint := oMenuItem.Hint;

    //Separator with text:
    If sSeparatorHint <> '' Then
    Begin
      //Initialize
      aRectangle := wp_graphics.Rect(0, 0, 0, 0);
      aCanvas.Font.Style := [fsBold];

      //Make windows calculate needed space
      Height := drawText(aCanvas.Handle, PChar(sSeparatorHint), Length(sSeparatorHint), aRectangle, DT_CALCRECT Or DT_LEFT Or DT_EXTERNALLEADING);
      Width := aRectangle.Right - aRectangle.Left;

      //Give some extra room for padding:
      Inc(Height, EXTENDED_MARG_Y * 4);
      Inc(Width,  EXTENDED_MARG_X * 2 + EXTENDED_SEPARATOR_LEADING);
    End
    Else
    //Plain old separator:
    Begin
      //Fixed height and width:
      Height := 4;
      Width := 10;
    End;
  End
  Else
  Begin
    //Caption
    sCaption := oMenuItem.Caption;
    aCaptionRectangle := wp_graphics.Rect(0, 0, 0, 0);
    aCanvas.Font.Style := [fsBold];

//    bHasGutter := (oMenuItem.GetImageList <> Nil) Or Checked;
    bHasGutter := True;

    iGutterWidth := EXTENDED_GUTTER_WIDTH;

    iCaptionHeight := DrawText(aCanvas.Handle, PChar(sCaption), Length(sCaption), aCaptionRectangle, DT_CALCRECT Or DT_LEFT Or DT_EXTERNALLEADING);
    iCaptionWidth  := aCaptionRectangle.Right - aCaptionRectangle.Left;

    //Shortcut:
    sShortCut := ShortCutToText(oMenuItem.ShortCut);

    If UseShortCut And (sShortCut <> '') Then
    Begin
      aShortCutRectangle := wp_graphics.Rect(0, 0, 0, 0);

      DrawText(aCanvas.Handle, PChar(sShortCut), Length(sShortCut), aShortCutRectangle, DT_CALCRECT Or DT_LEFT Or DT_EXTERNALLEADING);

      iShortCutWidth := aShortCutRectangle.Right - aShortCutRectangle.Left;
      Inc(iCaptionWidth, iShortCutWidth + EXTENDED_MARG_X * 2);
    End;

    //Hint:
    aHintRectangle := wp_graphics.Rect(0, 0, 0, 0);
    sHint := oMenuItem.Hint;
    aCanvas.Font.Style := [];

    iHintHeight := DrawText(aCanvas.Handle, PChar(sHint), Length(sHint), aHintRectangle, DT_CALCRECT Or DT_LEFT Or DT_EXTERNALLEADING);
    iHintWidth  := aHintRectangle.Right - aHintRectangle.Left;

    //Icon
    iIconHeight := 0;
    iIconWidth := 0;

    If (oMenuItem.ImageIndex >= 0) And (oMenuItem.GetImageList <> Nil) Then
    Begin
      iIconHeight := oMenuItem.GetImageList.Height;
      iIconWidth := oMenuItem.GetImageList.Width;
    End;

    If iIconWidth > NORMAL_ICON_WIDTH Then
      Inc(iGutterWidth, iIconWidth - NORMAL_ICON_WIDTH);

    Width := IntegerMax(iCaptionWidth, iHintWidth) + EXTENDED_MARG_X * 2;

    If bHasGutter Then
      Inc(Width, iGutterWidth);

    Width := IntegerMax(MinimumWidth, Width);

    Height := IntegerMax(iCaptionHeight + iHintHeight, iIconHeight) + EXTENDED_MARG_Y * 4;
  End;
End;


Procedure TUixMenuItem.DrawMenuItem(Sender: TObject; aCanvas: TCanvas; ARect: TRect; Selected: Boolean);
Const
  CheckStateValue : Array[Boolean] Of Integer = (DFCS_BUTTONCHECK, DFCS_BUTTONCHECK Or DFCS_CHECKED);

Var
  sHint : String;
  sCaption : String;
  sShortCut : String;

  aRectangle : TRect;
  aSelectedRegion : HRGN;
  CheckState : Integer;
  CheckRectangle : TRect;

  iOffset : Integer;
  iGutterWidth : Integer;
  iIconWidth : Integer;

  bHasGutter : Boolean;
  bHasIcon : Boolean;

  oMenuItem : TUixMenuItem;
  oImageList : TCustomImageList;
Begin
  oMenuItem := TUixMenuItem(sender);

  If IsSeparator Then
  Begin
    bHasGutter := oMenuItem.GetImageList <> Nil;

    //Background:
    aCanvas.Brush.Style := bsSolid;
    aCanvas.Brush.Color := ExtendedSeparatorBackgroundColour;
    aCanvas.FillRect(ARect);

    //Lines:
    aCanvas.Pen.Color := ExtendedSeparatorLineColour;
    aCanvas.Polyline([point(ARect.Left, ARect.Bottom - 2), point(ARect.Right, ARect.Bottom - 2)]);
    aCanvas.Pen.Color := ExtendedItemBackgroundColour;
    aCanvas.Polyline([point(ARect.Left, ARect.Bottom - 1), point(ARect.Right, ARect.Bottom - 1)]);

    //Text
    sHint := oMenuItem.Hint;

    If sHint <> '' Then
    Begin
      //Text:
      aCanvas.Brush.Style := bsClear;
      aCanvas.Font.Style := [fsBold];
      aCanvas.Font.Color := ExtendedFontColour;

      aRectangle.Left := ARect.Left + EXTENDED_MARG_X;

      If bHasGutter Then
        Inc(aRectangle.Left, EXTENDED_SEPARATOR_LEADING);

      aRectangle.Right := ARect.Right - EXTENDED_MARG_X;
      aRectangle.Top := ARect.Top;
      aRectangle.Bottom := ARect.Bottom;

      DrawText(aCanvas.Handle, PChar(sHint), Length(sHint), aRectangle, DT_LEFT Or DT_EXTERNALLEADING Or DT_SINGLELINE Or DT_VCENTER);
    End
    Else If bHasGutter Then
    Begin
     //Gutter
      aCanvas.Brush.Style := bsSolid;
      aCanvas.Brush.Color := ExtendedGutterColour;

      aRectangle := ARect;
      aRectangle.Right := aRectangle.Left + EXTENDED_GUTTER_WIDTH;

      aCanvas.FillRect(aRectangle);

      aCanvas.Pen.Color := ExtendedSeparatorLineColour;
      aCanvas.Polyline([point(aRectangle.Right, aRectangle.top), point(aRectangle.Right, aRectangle.Bottom)]);
    End;
  End
  Else
  Begin
    sHint := oMenuItem.Hint;
    sCaption := oMenuItem.Caption;
//    bHasGutter := (oMenuItem.GetImageList <> Nil);
    bHasGutter := True;

    iGutterWidth := EXTENDED_GUTTER_WIDTH;

//    If Not bHasGutter And Assigned(oMenuItem.Parent) Then
//    Begin
//      iLoop := 0;
//
//      While Not bHasGutter And (iLoop < oMenuItem.Parent.Count) Do
//      Begin
//        bHasGutter := oMenuItem.Parent.Items[iLoop].Checked;
//
//        Inc(iLoop);
//      End;
//    End;

    //Caption-height:
    If Default Then
      aCanvas.Font.Style := [fsBold];

    aRectangle := wp_graphics.Rect(0, 0, 0, 0);

    iOffset := DrawText(aCanvas.Handle, PChar(sCaption), Length(sCaption), aRectangle, DT_CALCRECT Or DT_EXTERNALLEADING Or DT_TOP);

    //Background
    aCanvas.Brush.Style := bsSolid;
    aCanvas.Brush.Color := ExtendedItemBackgroundColour;
    aCanvas.FillRect(ARect);

    bHasIcon := (oMenuItem.ImageIndex >= 0) And (oMenuItem.GetImageList <> Nil);

    //Icon
    iIconWidth := 0;

    If bHasIcon Then
      iIconWidth := oMenuItem.GetImageList.Width;

    If iIconWidth > NORMAL_ICON_WIDTH Then
      Inc(iGutterWidth, iIconWidth - NORMAL_ICON_WIDTH);


    //Gutter
    If bHasGutter Then
    Begin
      aCanvas.Brush.Style := bsSolid;
      aCanvas.Brush.Color := ExtendedGutterColour;

      aRectangle := aRect;
      aRectangle.Right := aRectangle.Left + iGutterWidth;//EXTENDED_GUTTER_WIDTH;
      //Right must be base on the left position, or you run into drawing issues when you have a multicolumn popup

      aCanvas.FillRect(aRectangle);

      aCanvas.Pen.Color := ExtendedSeparatorLineColour;
      aCanvas.Polyline([point(aRectangle.Right, aRectangle.top), point(aRectangle.Right, aRectangle.Bottom)]);
    End;

    //Selection
    If Selected Then
    Begin
      //Set a rounded rectangle as clip-region
      aSelectedRegion := CreateRoundRectRgn(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
      SelectClipRgn(aCanvas.Handle, aSelectedRegion);

      If sHint <> '' Then
      Begin
        //First gradient - caption
        aRectangle := ARect;
        aRectangle.Bottom := aRectangle.Top + iOffset + EXTENDED_MARG_Y * 2;

        ColourGradientVertical(aCanvas.Handle, wp_graphics.TRect(aRectangle), ExtendedGradientStart1, ExtendedGradientEnd1);

        //Second gradient - description
        aRectangle.Top := aRectangle.Bottom;
        aRectangle.Bottom := ARect.Bottom;
        ColourGradientVertical(aCanvas.Handle, wp_graphics.TRect(aRectangle), ExtendedGradientStart2, ExtendedGradientEnd2);
      End
      Else
      Begin
        //Only one gradient under caption
        aRectangle := ARect;
        ColourGradientVertical(aCanvas.Handle, wp_graphics.TRect(aRectangle), ExtendedGradientStart1, ExtendedGradientEnd1);
      End;

      //Release clipregion
      SelectClipRgn(aCanvas.Handle, 0);

      //Outline selection
      aCanvas.Pen.Color := ExtendedSeparatorLineColour;
      aCanvas.Brush.Style := bsClear;
      aCanvas.RoundRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom, 3, 3);
    End;


    //Caption
    aCanvas.Brush.Style := bsClear;

    If Default Then
      aCanvas.Font.Style := [fsBold];

    If oMenuItem.Enabled Then
      aCanvas.Font.Color := ExtendedFontColour
    Else
      aCanvas.Font.Color := ExtendedFontDisabledColour;

    aRectangle.Left := ARect.Left + EXTENDED_MARG_X;

    If bHasGutter Or Checked Then
      Inc(aRectangle.Left, iGutterWidth);//EXTENDED_GUTTER_WIDTH);

    aRectangle.Right := ARect.Right - EXTENDED_MARG_X;
    aRectangle.Top := ARect.Top + EXTENDED_MARG_Y;
    aRectangle.Bottom := ARect.Bottom;

    DrawText(aCanvas.Handle, PChar(sCaption), Length(sCaption), aRectangle, DT_LEFT Or DT_EXTERNALLEADING Or DT_TOP);


    //Shortcut
    sShortCut := ShortCutToText(oMenuItem.ShortCut);

    If UseShortcut And (sShortCut <> '') Then
      DrawText(aCanvas.Handle, PChar(sShortCut), Length(sShortCut), aRectangle, DT_RIGHT Or DT_EXTERNALLEADING Or DT_TOP);


    //Hint
    aCanvas.Font.Style := [];

    If oMenuItem.Enabled Then
      aCanvas.Font.Color := ExtendedFontColour
    Else
      aCanvas.Font.Color := ExtendedFontDisabledColour;

    aRectangle.Left := ARect.Left + EXTENDED_MARG_X;

    If bHasGutter Or Checked Then
      Inc(aRectangle.Left, iGutterWidth);//EXTENDED_GUTTER_WIDTH);

    aRectangle.Right := ARect.Right - EXTENDED_MARG_X;
    aRectangle.Top := ARect.Top + iOffset + EXTENDED_MARG_Y*2;
    aRectangle.Bottom := ARect.Bottom;

    DrawText(aCanvas.Handle, PChar(sHint), Length(sHint), aRectangle, DT_LEFT Or DT_EXTERNALLEADING Or DT_TOP);

    If Checked And Not bHasIcon Then
    Begin
      CheckRectangle := aRect;
      CheckRectangle.Right := CheckRectangle.Left + iGutterWidth;//EXTENDED_GUTTER_WIDTH;

      InflateRect(CheckRectangle, -2, -2);

      CheckState := CheckStateValue[Checked];

      DrawFrameControl(aCanvas.Handle, CheckRectangle, DFC_BUTTON, CheckState);
    End;

    //Icon
    If bHasIcon Then
    Begin
      If Selected Then
        oImageList := oMenuItem.GetSelectedImageList
      Else
        oImageList := Nil;

      If Not Assigned(oImageList) Then
        oImageList := oMenuItem.GetImageList;

      oImageList.Draw(aCanvas, ARect.Left + EXTENDED_MARG_X, ARect.Top + EXTENDED_MARG_Y, oMenuItem.ImageIndex, oMenuItem.Enabled);
    End;
  End;
End;


Procedure TUixMenuItem.BreakBar;
Begin
  Break := mbBarBreak;
End;


Procedure TUixMenuItem.BreakNone;
Begin
  Break := mbNone;
End;


Procedure TUixMenuItem.SetSeparator;
Begin
  Caption := SEPARATOR_CAPTION;
End;


Function TUixMenuItem.IsSeparator: Boolean;
Begin
  Result := Caption = SEPARATOR_CAPTION;
End;


Function TUixMenuItem.AddItem(Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem;
Begin
  Result := InsertItem(Count, sCaption, aEvent, iImage);
End;


Function TUixMenuItem.AddSeparator: TUixMenuItem;
Begin
  Result := InsertSeparator(Count);
End;


Function TUixMenuItem.AddSubmenu(Const sCaption: String; iImage: Integer): TUixMenuItem;
Begin
  Result := InsertSubmenu(Count, sCaption, iImage);
End;


Function TUixMenuItem.InsertItem(iIndex: Integer; Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem;
Begin
  Result := ClassType.Create(Self);

  Result.Caption := sCaption;
  Result.OnClick := aEvent;
  Result.ImageIndex := iImage;

  Insert(iIndex, Result);
End;


Function TUixMenuItem.InsertSeparator(iIndex: Integer): TUixMenuItem;
Begin
  Result := ClassType.Create(Self);
  Result.SetSeparator;

  Insert(iIndex, Result);
End;


Function TUixMenuItem.InsertSubmenu(iIndex: Integer; Const sCaption: String; iImage: Integer): TUixMenuItem;
Begin
  Result := ClassType.Create(Self);

  Result.Caption := sCaption;
  Result.ImageIndex := iImage;

  Insert(iIndex, Result);
End;


Function TUixMenuItem.MeasuredHeight: Integer;
Var
  iWidth : Integer;
Begin
  MeasureMenuItem(Self, Bitmap.Canvas, iWidth, Result);
End;


Function TUixMenuItem.GetSelectedImageList : TCustomImageList;
Var
  oMenuItem : TMenuItem;
  oUixMenuItem : TUixMenuItem;
  oMenu : TMenu;
Begin
  Result := Nil;

  oMenuItem := Parent;
  oUixMenuItem := Nil;
  While Not Assigned(oUixMenuItem) And Assigned(oMenuItem) Do
  Begin
    If oMenuItem.ClassType <> TUixMenuItem Then
      oMenuItem := Nil
    Else If Not Assigned(TUixMenuItem(oMenuItem).SelectedSubMenuImageList) Then
      oMenuItem := oMenuItem.Parent
    Else
      oUixMenuItem := TUixMenuItem(oMenuItem);
  End;

  If Assigned(oUixMenuItem) Then
  Begin
    Result := oUixMenuItem.SelectedSubMenuImageList;
  End
  Else
  Begin
    oMenu := GetParentMenu;

    If Assigned(oMenu) And (oMenu.ClassType = TUixPopupMenu) Then
      Result := TUixPopupMenu(oMenu).SelectedImageList;
  End;
End;


Function TUixMenuItems.GetMenuItemByIndex(iIndex : Integer) : TUixMenuItem;
Begin
  Result := TUixMenuItem(Items[iIndex]);
End;


Constructor TUixPopupMenu.Create(oOwner : TComponent);
Begin
  Inherited;

  Initialise;
End;


Destructor TUixPopupMenu.Destroy;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixPopupMenu.Initialise;
Begin
  FUseShortcuts := True;

  OwnerDraw := True;

  ManualAutoLineReduction;
End;


Procedure TUixPopupMenu.Finalise;
Begin
End;


Function TUixPopupMenu.MenuItemClass : TUixMenuItemClass;
Begin
  Result := TUixMenuItem;
End;


Function TUixPopupMenu.IsShortCut(Var Message: TWMKey): Boolean;
Begin
  Result := FUseShortcuts And Inherited IsShortcut(Message);
End;


Procedure TUixPopupMenu.AutomaticAutoLineReduction;
Begin
  AutoLineReduction := maAutomatic;
End;


Procedure TUixPopupMenu.ManualAutoLineReduction;
Begin
  AutoLineReduction := maManual;
End;


Procedure TUixPopupMenu.FireClose;
Begin
  If Assigned(FOnClosePopup) Then
    FOnClosePopup(Self);
End;


Function TUixPopupMenu.AddItem(Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer) : TUixMenuItem;
Begin
  Result := InsertItem(Items.Count, sCaption, aEvent, iImage);
End;


Function TUixPopupMenu.AddSeparator : TUixMenuItem;
Begin
  Result := InsertSeparator(Items.Count);
End;


Function TUixPopupMenu.AddSubmenu(Const sCaption: String; iImage: Integer) : TUixMenuItem;
Begin
  Result := InsertSubmenu(Items.Count, sCaption, iImage);
End;


Function TUixPopupMenu.AddRadioItem(Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem;
Begin
  Result := AddItem(sCaption, aEvent, iImage);

  Result.RadioItem := True;
End;


Function TUixPopupMenu.InsertItem(iIndex: Integer; Const sCaption: String; aEvent: TUixMenuItemEvent; iImage: Integer): TUixMenuItem;
Begin
  Result := MenuItemClass.Create(Self);

  Result.Caption := sCaption;
  Result.OnClick := aEvent;
  Result.ImageIndex := iImage;
  Result.UseShortCut := UseShortCuts;

  Items.Insert(iIndex, Result);
End;


Function TUixPopupMenu.InsertSeparator(iIndex: Integer): TUixMenuItem;
Begin
  Result := MenuItemClass.Create(Self);
  Result.SetSeparator;

  Items.Insert(iIndex, Result);
End;


Function TUixPopupMenu.InsertSubmenu(iIndex: Integer; Const sCaption: String; iImage: Integer): TUixMenuItem;
Begin
  Result := MenuItemClass.Create(Self);

  Result.Caption := sCaption;
  Result.ImageIndex := iImage;

  Items.Insert(iIndex, Result);
End;


Function TUixPopupMenu.VisibleCount : Integer;
Var
  iLoop : Integer;
  oItem : TMenuItem;
Begin
  Result := 0;

  For iLoop := 0 To Items.Count - 1 Do
  Begin
    oItem := Items[iLoop];

    If oItem.Visible Then
      Inc(Result);
  End;
End;


Function TUixPopupMenu.GetItems : TUixMenuItem;
Begin
  Result := TUixMenuItem(Inherited Items);
End;


Procedure TUixPopupMenu.SetUseShortcuts(Const Value : Boolean);
Var
  iIndex : Integer;
Begin
  FUseShortcuts := Value;

  For iIndex := 0 To Items.Count - 1 Do
    TUixMenuItem(Items[iIndex]).UseShortcut := Value;
End;


Procedure TUixPopupMenu.CloseMenu;
Begin
  EndMenu;
End;


Const
  ColorRectWidth = 30;


Constructor TUixHTMLColourComboBox.Create(AOwner: TComponent);
Begin
  Inherited Create(AOwner);

  Style := csOwnerDrawFixed;
End;


Procedure TUixHTMLColourComboBox.CreateHandle;
Var
  aLoop : THTMLColours;
Begin
  Inherited;

  Items.BeginUpdate;
  Try
    Items.Clear;

    Items.Add('Default');
    Items.Add('Custom');
    Items.Add('--');
    For aLoop := Low(THTMLColours) To High(THTMLColours) Do
      Items.Add(HTML_COLOUR_TITLES[aLoop]);
  Finally
    Items.EndUpdate;
  End;

  ItemIndex := Items.IndexOf('Black');
End;


Function TUixHTMLColourComboBox.GetValue : TColour;
Begin
  Result := ReadColor(Text, clBlack);
End;


Function TUixHTMLColourComboBox.ReadColor(sText : String; aDefault : TColour):TColour;
Begin
  If StringStartsWith(sText, 'Custom: ') Then
    Delete(sText, 1, 8);

  If StringIsHTMLColour(sText) Then
    Result := HTMLColourStringToColour(sText)
  Else If sText = 'Default' Then
    Result := MAXINT
  Else
    Result := aDefault;
End;


Procedure TUixHTMLColourComboBox.SetValue(iValue: TColour);
Var
  sValue : String;
Begin
  If iValue <> Value Then
  Begin
    sValue := ColourToHTMLColourTitleString(iValue);

    If Items.IndexOf(sValue) > -1 Then
      ItemIndex := Items.IndexOf(sValue)
    Else
    If iValue = MAXINT Then
    Begin
      itemIndex := 0;
    End
    Else
    Begin
      Items[1] := 'Custom: '+sValue;
      itemIndex := 1;
    End;
  End;
End;


Procedure TUixHTMLColourComboBox.CNDrawItem(Var Message: TWMDrawItem);
Begin
  With Message.DrawItemStruct^ Do
    rcItem.Left := rcItem.Left + ColorRectWidth + 2;

  Inherited;
End;


Procedure TUixHTMLColourComboBox.DrawItem(Index: Integer; aRect: Windows.TRect; State: TOwnerDrawState);
Var
  iColor: TColor;
  aTarget: TRect;
Begin
  If (Index < Items.Count) And (Index > 0) Then
  Begin
    iColor := Canvas.Brush.Color;

    Canvas.Brush.Color := ReadColor(Items[Index], clGray);

    aTarget := wp_graphics.Rect(aRect.Left - ColorRectWidth, aRect.Top + 1, aRect.Left - 2, aRect.Bottom - 1);

    Canvas.Rectangle(aTarget.Left, aTarget.Top, aTarget.Right, aTarget.Bottom);

    aTarget := RectInflate(aTarget, -1, -1);

    Canvas.FillRect(Windows.TRect(aTarget));

    Canvas.Brush.Color := iColor;
  End;

  Inherited;
End;


Procedure TUixHTMLColourComboBox.Change;
Var
  oDlg : TColorDialog;
Begin
  Inherited Change;
  If ItemIndex = 1 Then
  Begin
    oDlg := TColorDialog.Create(Self);
    oDlg.Options := [cdFullOpen, cdAnyColor];
    oDlg.Color := ReadColor(Items[ItemIndex], $007F7F7F);
    If oDlg.Execute Then
      SetValue(oDlg.Color);
  End;
End;


Constructor TUixControl.Create(oOwner: TComponent);
Begin
  Inherited;

End;


Destructor TUixControl.Destroy;
Begin

  Inherited;
End;


Procedure TUixControl.Error(Const sMethod, sMessage : String);
Begin
  If Assigned(Self) Then
    Raise EFslException.Create('(' + Self.ClassName + '.' + sMethod + '): ' + sMessage)
  Else
    Raise EFslException.Create('(Nil.' + sMethod + '): ' + sMessage);
End;


Procedure TUixControl.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixControl.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixControl.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixControl.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixControl.AlignTop;
Begin
  Align := alTop;
End;


Function TUixControl.Condition(Const bTruth: Boolean; Const sMethod, sMessage: String): Boolean;
Begin
  If Not bTruth Then
    Error(sMethod, sMessage);

  Result := True;
End;


Procedure TUixControl.ShuffleTop;
Begin
  Top := 0;
End;


Procedure TUixControl.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixControl.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixControl.ShuffleRight;
Begin
  Left := MaxInt;
End;


Constructor TUixGroupBox.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

End;


Destructor TUixGroupBox.Destroy;
Begin

  Inherited;
End;


Procedure TUixGroupBox.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixGroupBox.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixGroupBox.Initialise;
Begin
  Height := 57;

  FShowBorder := False;
End;


Procedure TUixGroupBox.Finalise;
Begin
End;


Procedure TUixGroupBox.AdjustClientRect(Var aRect : TRect);
Begin
  Inherited;

  Inc(aRect.Left, FLeftMargin);
End;


Procedure TUixGroupBox.Paint;
Var
  aRect : TRect;
Begin
  Canvas.Font := Font;

  If FShowBorder Then
  Begin
    Inherited;

    If Enabled Then
      Canvas.Font.Color := clNavy
    Else
      Canvas.Font.Color := clGrayText;

    Canvas.Font.Style := [fsBold];

    If Not UseRightToLeftAlignment Then
      aRect := wp_graphics.Rect(8, 0, 0, Canvas.TextHeight('0'))
    Else
      aRect := wp_graphics.Rect(aRect.Right - Canvas.TextWidth(Text) - 8, 0, 0, Canvas.TextHeight('0'));

    DrawText(Canvas.Handle, PChar(Text), Length(Text), aRect, DrawTextBiDiModeFlags(DT_SINGLELINE) Or DT_CALCRECT);
    Canvas.Brush.Color := Color;
    DrawText(Canvas.Handle, PChar(Text), Length(Text), aRect, DrawTextBiDiModeFlags(DT_SINGLELINE));
  End
  Else
  Begin
    aRect := HeaderRect;

    DrawText(Canvas.Handle, PChar(Text), Length(Text), aRect, DrawTextBiDiModeFlags(DT_SINGLELINE));

    aRect.Left := aRect.Right + 8;
    aRect.Right := Width;
    aRect.Top := ((aRect.Bottom - aRect.Top) Div 2);
    aRect.Bottom := aRect.Top + 1;

    Canvas.Brush.Color := clBtnHighlight;
    Canvas.FrameRect(aRect);

    OffsetRect(aRect, -1, -1);

    Canvas.Brush.Color := clBtnShadow;

    Canvas.FrameRect(aRect);
  End;
End;


Procedure TUixGroupBox.SetLeftMargin(Const iValue : Integer);
Begin
  If FLeftMargin <> iValue Then
  Begin
    FLeftMargin := iValue;
    Invalidate;
    Realign;
  End;
End;


Function TUixGroupBox.HeaderHeight: Integer;
Var
  aRect : TRect;
Begin
  aRect := HeaderRect;

  Result := aRect.Bottom - aRect.Top;
End;


Function TUixGroupBox.HeaderRect: TRect;
Begin
  Canvas.Brush.Color := Color;

  If Enabled Then
    Canvas.Font.Color := clNavy
  Else
    Canvas.Font.Color := clGrayText;

  Canvas.Font.Style := [fsBold];

  If Not UseRightToLeftAlignment Then
    Result := wp_graphics.Rect(8, 0, 0, 0)
  Else
    Result := wp_graphics.Rect(Result.Right - Canvas.TextWidth(Text) - 8, 0, 0, 0);

  DrawText(Canvas.Handle, PChar(Text), Length(Text), Result, DrawTextBiDiModeFlags(DT_SINGLELINE) Or DT_CALCRECT);
End;


Procedure TUixGroupBox.Reset;
Begin
  Height := RequiredHeight;
End;


Procedure TUixGroupBox.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixGroupBox.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixGroupBox.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixGroupBox.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixGroupBox.AlignTop;
Begin
  Align := alTop;
End;


Function TUixGroupBox.IsAlignedBottom : Boolean;
Begin
  Result := Align = alBottom;
End;


Function TUixGroupBox.IsAlignedClient : Boolean;
Begin
  Result := Align = alClient;
End;


Function TUixGroupBox.IsAlignedLeft : Boolean;
Begin
  Result := Align = alLeft;
End;


Function TUixGroupBox.IsAlignedRight : Boolean;
Begin
  Result := Align = alRight;
End;


Function TUixGroupBox.IsAlignedTop : Boolean;
Begin
  Result := Align = alTop;
End;


Procedure TUixGroupBox.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixGroupBox.SetEnabled(bValue : Boolean);
Begin
  Inherited;

  Paint;
End;


Function TUixGroupBoxes.GetGroupBox(Const iIndex : Integer) : TUixGroupBox;
Begin
  Result := TUixGroupBox(Items[iIndex]);
End;


Function TUixGroupBox.VisibleControlHeight : Integer;
Var
  iLoop    : Integer;
  oControl : TControl;
Begin
  Result := 0;

  For iLoop := 0 To ControlCount - 1 Do
  Begin
    oControl := Controls[iLoop];

    If oControl.Visible And (oControl.Align In [alTop, alBottom, alClient]) Then
      Inc(Result, oControl.Height);
  End;
End;


Function TUixGroupBox.VisibleControlWidth : Integer;
Var
  iLoop    : Integer;
  oControl : TControl;
Begin
  Result := 0;

  For iLoop := 0 To ControlCount - 1 Do
  Begin
    oControl := Controls[iLoop];

    If oControl.Visible And (oControl.Align In [alLeft, alRight, alClient]) Then
      Inc(Result, oControl.Width);
  End;
End;


Function TUixGroupBox.RequiredNonClientHeight: Integer;
Begin
  If FShowBorder Then
    Result := HeaderHeight + BorderWidth*2 + 4
  Else
    Result := HeaderHeight + BorderWidth*2;
End;


Function TUixGroupBox.RequiredClientHeight: Integer;
Begin
  Result := VisibleControlHeight;
End;


Function TUixGroupBox.RequiredHeight : Integer;
Begin
  Result := RequiredNonClientHeight + RequiredClientHeight;
End;


Function TUixGroupBox.RequiredNonClientWidth: Integer;
Begin
  // Don't know where the extra 6 comes from, but is required
  Result := LeftMargin + BorderWidth*2 + 6;
End;


Function TUixGroupBox.RequiredClientWidth: Integer;
Begin
  Result := VisibleControlWidth;
End;


Function TUixGroupBox.RequiredWidth: Integer;
Begin
  Result := RequiredNonClientWidth + RequiredClientWidth;
End;

Constructor TUixEdit.Create(oOwner : TComponent);
Begin
  Inherited;

  GhostText := '';

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  FAllowedCharacterList := TFslCharacterList.Create;
  FAllowedCharacterList.SortByValue;
  FAllowedCharacterList.IgnoreDuplicates;

  FDisallowedCharacterList := TFslCharacterList.Create;
  FDisallowedCharacterList.SortByValue;
  FDisallowedCharacterList.IgnoreDuplicates;
End;


Destructor TUixEdit.Destroy;
Begin
  FAllowedCharacterList.Free;
  FDisallowedCharacterList.Free;

  Inherited;
End;


Procedure TUixEdit.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixEdit.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixEdit.Initialise;
Begin
  FReadBackgroundColour := clBtnFace;
  FWriteBackgroundColour := clWindow;
  FDefaultTextColour := clWindowText;
  FGhostTextColour := clGrayText;
  FInvalidBackgroundColour := $00FFD5CF;
  FHasInvalidBackgroundColour := True;

  Prepare;
End;


Procedure TUixEdit.Finalise;
Begin
End;


Procedure TUixEdit.Change;
Begin
  If Not FShowingGhostText Then
  Begin
    If HasGhostText And (Text = GhostText) Then
      FValueIsGhostText := True
    Else
      FValueIsGhostText := False;
  End;

  Inherited;

  Prepare;
End;


Procedure TUixEdit.Prepare;
Begin
  If ReadOnly Then
    Color := ReadBackgroundColour
  Else If Not IsValidLength And HasInvalidBackgroundColour Then
    Color := InvalidBackgroundColour
  Else
    Color := WriteBackgroundColour;
End;


Procedure TUixEdit.KeyPress(Var Key: Char);
Begin
  Inherited KeyPress(Key);

  If Not CharInSet(Key, [#3, // Ctrl + C
                         #8, // Backspace
                         #13, // Enter
                         #22, // Ctrl + V
                         #24 // Ctrl + X
                         ]) Then
  Begin
    Key := TranslateCharacter(Key);

    If Not ValidateCharacter(Key) Then
    Begin
      ShowBalloon;
      Key := #0;
    End;
  End;
End;


Function TUixEdit.TranslateCharacter(Const cKey : Char) : Char;
Begin
  If EnforceCapitals Then
    Result := charUpper(cKey)
  Else
    Result := cKey;
End;


Procedure TUixEdit.DoEnter;
Begin
  Inherited;

  If HasGhostText And CanHideGhostText Then
    HideGhostText;
End;


Procedure TUixEdit.DoExit;
Begin
  Inherited;

  If HasGhostText And CanShowGhostText Then
    ShowGhostText;

  HideBalloon;
End;


Procedure TUixEdit.ValidateEdit;
Begin
// Disable TMaskEdit validation as the exceptions aren't useful.
//Inherited;
End;


Function TUixEdit.BalloonClass : TUixBalloonClass;
Begin
  Result := TUixBalloon;
End;


Function TUixEdit.BalloonNew : TUixBalloon;
Begin
  Result := BalloonClass.CreateNew(Self);
  Result.TimeoutDuration := 4000;
  Result.BalloonTypeError;
  Result.AnchorPositionTopLeft;
  Result.ControlRelativeHorizontalPositionCenter;
  Result.ControlRelativeVerticalPositionBottom;
End;


Procedure TUixEdit.HideBalloon;
Begin
  If Assigned(FBalloon) Then
  Begin
    FBalloon.Release;
    FBalloon := Nil;
  End;
End;


Procedure TUixEdit.ShowBalloon;
Begin
  HideBalloon;

  FBalloon := BalloonNew;
  FBalloon.Control := Self;
  FBalloon.Title := Title;
  FBalloon.Message := LastError;
  FBalloon.ShowBalloon;
End;


Function TUixEdit.TextLength:Integer;
Begin
  Result := Length(Text);
End;


Function TUixEdit.IsValidLength : Boolean;
Var
  sLastError : String;
Begin
  sLastError := LastError;
  Try
    Result := ValidateLength;
  Finally
    LastError := sLastError;
  End;
End;


Function TUixEdit.ValidateLength : Boolean;
Var
  iTextLength : Integer;
Begin
  Result := Not (Empty And Mandatory);

  If Not Result Then
    LastError := Title + ' is a mandatory field and must be entered.';

  If Result And Not Empty Then
  Begin
    iTextLength := TextLength;

    Result := iTextLength >= FMinLength;

    If Not Result Then
    Begin
      LastError := StringFormat('%s must be at least %d characters in length.', [Title, FMinLength]);
    End
    Else If (MaxLength <> 0) Then
    Begin
      Result := iTextLength <= MaxLength;

      If Not Result Then
        LastError := StringFormat('%s can be no more than %d %s in length.', [Title, MaxLength, StringPlural('character', MaxLength)]);
    End;
  End;
End;


Function TUixEdit.Valid : Boolean;
Begin
  LastError := '';

  Result := ValidateLength And ValidateContent;
End;


Function TUixEdit.GetNotReadOnly : Boolean;
Begin
  Result := Not ReadOnly;
End;


Procedure TUixEdit.SetNotReadOnly(Const Value : Boolean);
Begin
  ReadOnly := Not Value;
End;


Function TUixEdit.GetReadOnly : Boolean;
Begin
  Result := Inherited ReadOnly;
End;


Procedure TUixEdit.SetReadOnly(Const Value : Boolean);
Begin
  Inherited ReadOnly := Value;

  Prepare;
End;


Function TUixEdit.GetMandatory : Boolean;
Begin
  Result := FMandatory;
End;


Procedure TUixEdit.SetMandatory(Const bValue : Boolean);
Begin
  FMandatory := bValue;

  Prepare;
End;


Function TUixEdit.Empty : Boolean;
Begin
  Result := TextLength = 0;
End;


Function TUixEdit.Full : Boolean;
Begin
  Result := (MaxLength > 0) And (Length(Text) = MaxLength);
End;


Function TUixEdit.ValidateContent : Boolean;
Var
  iLoop : Integer;
  sText : String;
Begin
  Result := True;
  sText := Text;

  iLoop := 1;
  While Result And (iLoop <= Length(sText)) Do
  Begin
    Result := ValidateCharacter(sText[iLoop]);

    Inc(iLoop);
  End;
End;


Function TUixEdit.ValidateCharacter(Const cChar : Char) : Boolean;
Var
  iAllowedIndex : Integer;
  sAllowedCharacters : String;
Begin
  Result := Not DisallowedCharacterList.ExistsByValue(cChar);

  If Result And Not AllowedCharacterList.ExistsByValue(cChar) Then
  Begin
    If EnforceAlphabetic And EnforceNumeric Then
      Result := StringIsAlphanumeric(cChar)
    Else If EnforceAlphabetic Then
      Result := StringIsAlphabetic(cChar)
    Else If EnforceNumeric Then
      Result := StringIsNumeric(cChar);

    If Result And EnforceCapitals Then
      Result := TCharacter.IsUpper(cChar);
  End;

  If Not Result Then
  Begin
    sAllowedCharacters := '';

    If EnforceAlphabetic Then
    Begin
      StringAppend(sAllowedCharacters, 'A-Z', ', ');

      If Not EnforceCapitals Then
        StringAppend(sAllowedCharacters, 'a-z', ', ');
    End;

    If EnforceNumeric Then
      StringAppend(sAllowedCharacters, '0-9', ', ');

    For iAllowedIndex := 0 To AllowedCharacterList.Count - 1 Do
      sAllowedCharacters := sAllowedCharacters + ', ' + AllowedCharacterList[iAllowedIndex].Value;

    LastError := StringFormat('%s does not allow character (%s) as it can only contain characters (%s)', [Title, cChar, sAllowedCharacters]);
  End;
End;


Function TUixEdit.GetMask : String;
Begin
  Result := EditMask;
End;


Procedure TUixEdit.SetMask(Const Value : String);
Begin
  EditMask := Value;
End;


Function TUixEdits.GetEdit(Const iIndex : Integer): TUixEdit;
Begin
  Result := TUixEdit(Items[iIndex]);
End;


Procedure TUixEdits.SetEdit(Const iIndex : Integer; Const Value : TUixEdit);
Begin
  Items[iIndex] := Value;
End;


Function TUixEdit.GetAnchoredBottom: Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixEdit.GetAnchoredLeft: Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixEdit.GetAnchoredRight: Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixEdit.GetAnchoredTop: Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixEdit.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixEdit.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixEdit.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixEdit.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Function TUixEdit.GetBolded: Boolean;
Begin
  Result := fsBold In Font.Style;
End;


Procedure TUixEdit.SetBolded(Const Value: Boolean);
Begin
  If Value Then
    Font.Style := Font.Style + [fsBold]
  Else
    Font.Style := Font.Style - [fsBold];
End;


Function TUixEdit.GetValue : String;
Begin
  If (Not HasGhostText) Or FValueIsGhostText Then
  Begin
    Result := Text;
  End
  Else
  Begin
    If GhostText = Text Then
      Result := ''
    Else
      Result := Text;
  End;
End;


Procedure TUixEdit.SetValue(Const sValue : String);
Var
  oChangeDelegate : TNotifyEvent;
Begin
  oChangeDelegate := OnChange;
  Try
    OnChange := Nil;

    // Reset text colouring if the ghost text colouring is active.
    If HasGhostText And IsGhostTextColour Then
      Font.Color := DefaultTextColour;

    Text := sValue;

    If HasGhostText And CanShowGhostText And Not Focused Then
      ShowGhostText;
  Finally
    OnChange := oChangeDelegate;
  End;
End;


Procedure TUixEdit.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixEdit.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixEdit.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixEdit.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixEdit.AlignTop;
Begin
  Align := alTop;
End;


Function TUixEdit.GetHasGhostText: Boolean;
Begin
  Result := GhostText <> '';
End;


Function TUixEdit.GetGhostText: String;
Begin
  Result := FGhostText;
End;


Procedure TUixEdit.SetGhostText(Const sValue: String);
Begin
  FGhostText := sValue;
End;


Procedure TUixEdit.ShowGhostText;
Begin
  FShowingGhostText := True;

  // Set colour first, as setting Text will trigger the OnChange event, which can change the value of FValueIsGhostText
  If FValueIsGhostText Then
    Font.Color := DefaultTextColour
  Else
    Font.Color := GhostTextColour;

  // Set Text, not Value, to avoid triggering the SetValue event
  Text := GhostText;

  FShowingGhostText := False;
End;


Procedure TUixEdit.HideGhostText;
Begin
  // Set Text to avoid triggering the SetValue event
  Text := '';
  Font.Color := DefaultTextColour;
End;


Function TUixEdit.GetCanShowGhostText: Boolean;
Begin
  // Use Text, as Value is dependent on GhostText
  Result := (Text = '') And Not FValueIsGhostText;
End;


Function TUixEdit.GetCanHideGhostText: Boolean;
Begin
  Result := (Text = GhostText) And Not FValueIsGhostText;
End;


Function TUixEdit.IsGhostTextColour: Boolean;
Begin
  Result := Font.Color = GhostTextColour;
End;


Function TUixEdit.IsDisplayingGhostText: Boolean;
Begin
  Result := Text = GhostText;
End;


Procedure TUixEdit.RefreshHeight;
Var
  aDC : HDC;
  aSaveFont : HFont;
  aTextMetric : TTextMetric;
  aSystemTextMetric : TTextMetric;

  iRequiredHeight : Integer;
Begin
  aDC := GetDC(0);
  GetTextMetrics(aDC, aSystemTextMetric);
  aSaveFont := SelectObject(aDC, Font.Handle);
  GetTextMetrics(aDC, aTextMetric);
  SelectObject(aDC, aSaveFont);
  ReleaseDC(0, aDC);

  iRequiredHeight := aTextMetric.tmHeight;

  If BorderStyle <> bsNone Then
  Begin
    If Ctl3D Then
      Inc(iRequiredHeight, 4)
    Else
      Inc(iRequiredHeight, 2);

    Inc(iRequiredHeight, IntegerMin(aTextMetric.tmHeight, aSystemTextMetric.tmHeight) Div 2);
  End;

  If iRequiredHeight <> Height Then
    Height := iRequiredHeight;
End;


Procedure TUixEdit.CMMouseEnter(Var aMessage : TMessage);
Begin
  Inherited;

  If Assigned(FMouseEnterHandler) Then
    FMouseEnterHandler(Self);
End;


Procedure TUixEdit.CMMouseLeave(Var aMessage : TMessage);
Begin
  Inherited;

  If Assigned(FMouseLeaveHandler) Then
    FMouseLeaveHandler(Self);
End;

Constructor TUixCheckBox.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  ParentColor := True;
End;


Destructor TUixCheckBox.Destroy;
Begin

  Inherited;
End;


Procedure TUixCheckBox.Initialise;
Begin
End;


Procedure TUixCheckBox.Finalise;
Begin
End;


Procedure TUixCheckBox.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixCheckBox.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Function TUixCheckBox.GetReadOnly : Boolean;
Begin
  Result := Not Enabled;
End;


Procedure TUixCheckBox.SetReadOnly(Const Value : Boolean);
Begin
  Enabled := Not Value;
End;


Procedure TUixCheckBox.WMPaint(Var Message: TWMPaint);
Var
  aDC : HDC;
  aRect : TRect;
Begin
  Inherited;

  If Focused Then
  Begin
    aRect := ClientRect;

    If Alignment = taLeftJustify Then
      Dec(aRect.Right, GetSystemMetrics(SM_CXMENUCHECK))
    Else If Alignment = taRightJustify Then
      Inc(aRect.Left, GetSystemMetrics(SM_CXMENUCHECK));

    aDC := GetDC(Handle);
    Try
      DrawFocusRect(aDC, aRect);
    Finally
      ReleaseDC(Handle, aDC);
    End;
  End;
End;


Function TUixCheckbox.GetValue : Boolean;
Begin
  Result := Checked;
End;


Procedure TUixCheckbox.SetValue(Const Value : Boolean);
Begin
  ClicksDisabled := True;
  Try
    Checked := Value;
  Finally
    ClicksDisabled := False;
  End;
End;


Function TUixCheckBox.GetAnchoredBottom: Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixCheckBox.GetAnchoredLeft: Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixCheckBox.GetAnchoredRight: Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixCheckBox.GetAnchoredTop: Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixCheckBox.SetAnchoredBottom(Const Value: Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixCheckBox.SetAnchoredLeft(Const Value: Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixCheckBox.SetAnchoredRight(Const Value: Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixCheckBox.SetAnchoredTop(Const Value: Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixCheckBox.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixCheckBox.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixCheckBox.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixCheckBox.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixCheckBox.AlignTop;
Begin
  Align := alTop;
End;


Function TUixCheckBox.GetOnChange : TNotifyEvent;
Begin
  Result := OnClick;
End;


Procedure TUixCheckBox.SetOnChange(Const Value : TNotifyEvent);
Begin
  OnClick := Value;
End;


Function TUixCheckboxList.GetCheckboxByIndex(iIndex : Integer) : TUixCheckbox;
Begin
  Result := TUixCheckbox(Items[iIndex]);
End;


Constructor TUixPageControl.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Destructor TUixPageControl.Destroy;
Begin
  Inherited;
End;


Procedure TUixPageControl.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixPageControl.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixPageControl.Initialise;
Begin
End;


Procedure TUixPageControl.Finalise;
Begin
End;


Procedure TUixPageControl.AdjustClientRect(Var Rect: TRect);
Begin
  If FFullPages Then
  Begin
    Rect := ClientRect;
    SendMessage(Handle, TCM_ADJUSTRECT, 0, Integer(@Rect));
    Rect := Classes.Rect(0, 0, Width, Height);
  End
  Else
  Begin
    Inherited;
  End;
End;


Function TUixPageControl.GetDockTab(Control: TControl): TTabSheet;
Begin
  Result := GetPageFromDockClient(Control);
End;


Procedure TUixPageControl.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixPageControl.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixPageControl.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixPageControl.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixPageControl.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixPageControl.TabStyleButtons;
Begin
  Style := tsButtons;
End;


Procedure TUixPageControl.TabStyleFlatButtons;
Begin
  Style := tsFlatButtons;
End;


Procedure TUixPageControl.TabStyleTabs;
Begin
  Style := tsTabs;
End;


Function TUixPageControl.GetTabHeight : Integer;
Var
  aRect : TRect;
Begin
  If TabCtrl_GetItemRect(Handle, 0, aRect) Then
    Result := (aRect.Bottom - aRect.Top + 10) * TabCtrl_GetRowCount(Handle)
  Else
    Result := 0;
End;


Function TUixPageControl.VisibleControlHeight(oTabSheet : TTabSheet) : Integer;
Var
  iLoop    : Integer;
  oControl : TControl;
Begin
  Result := 0;

  For iLoop := 0 To oTabSheet.ControlCount - 1 Do
  Begin
    oControl := oTabSheet.Controls[iLoop];

    If oControl.Visible And (oControl.Align <> alNone) Then
      Inc(Result, oControl.Height);
  End;
End;


Procedure TUixPageControl.Reset;
Var
  iHeight : Integer;
Begin
  iHeight := RequiredHeight;

  If iHeight <> Height Then
    Height := iHeight;
End;


Function TUixPageControl.RequiredHeight : Integer;
Begin
  Result := GetTabHeight + VisibleControlHeight + (BorderWidth * 2);
End;


Function TUixPageControl.VisibleControlHeight: Integer;
Var
  iLoop : Integer;
  oTabSheet : TTabSheet;
Begin
  Result := 0;

  For iLoop := 0 To PageCount - 1 Do
  Begin
    oTabSheet := Pages[iLoop];

//  If oTabSheet.TabVisible And oTabSheet.Visible Then
      Result := IntegerMax(Result, VisibleControlHeight(oTabSheet) + (oTabSheet.BorderWidth * 2));
  End;
End;


Function TUixPageControl.IsAlignedBottom : Boolean;
Begin
  Result := Align = alBottom;
End;


Function TUixPageControl.IsAlignedLeft : Boolean;
Begin
  Result := Align = alLeft;
End;


Function TUixPageControl.IsAlignedRight : Boolean;
Begin
  Result := Align = alRight;
End;


Function TUixPageControl.IsAlignedClient : Boolean;
Begin
  Result := Align = alClient;
End;


Function TUixPageControl.IsAlignedTop : Boolean;
Begin
  Result := Align = alTop;
End;


Constructor TUixTabsheet.Create(oOwner: TComponent);
Begin

  Inherited;

  PrepareInitialise;

  // This code causes DFM'd UixWizards to A/V on creation.
//If Assigned(oOwner) And (oOwner Is TWinControl) Then
//  Parent := TWinControl(oOwner);
End;


Destructor TUixTabsheet.Destroy;
Begin
  Try
    PrepareFinalise;
  Finally
    Inherited;

  End;
End;


Procedure TUixTabsheet.AfterConstruction;
Begin
  Inherited;

  Initialise;

End;


Procedure TUixTabsheet.BeforeDestruction;
Begin

  Finalise;

  Inherited;
End;


Procedure TUixTabsheet.Error(aException: EFslExceptionClass; Const sMethod, sMessage: String);
Begin
  Raise aException.Create(Self, sMethod, sMessage);
End;


Procedure TUixTabsheet.Error(Const sMethod, sMessage: String);
Begin
  Error(EFslException, sMethod, sMessage);
End;


Function TUixTabsheet.Invariant(Const sMethod, sMessage: String): Boolean;
Begin
  // Call this method as you would the Error method to raise an exception.
  // Use this when you are not sure if self is valid as it is a non-virtual method.

  Raise EFslInvariant.Create(Self, sMethod, sMessage); // Can't use Error method here as it is virtual.

  Result := True;
End;


Function TUixTabsheet.Invariants(Const sLocation : String; oObject : TObject; aClass: TClass; Const sObject : String) : Boolean;
Begin
  If Not Assigned(aClass) Then
    Invariant('Invariants', 'aClass was not assigned.');

  // Ensure object is assigned.
  If Not Assigned(oObject) Then
    Invariant(sLocation, sObject + ' was not assigned and was expected to have been of class ' + aClass.ClassName);

  // Ensure object is of the expected class.
  If Not oObject.InheritsFrom(aClass) Then
    Invariant(sLocation, sObject + ' was of class ' + oObject.ClassName + ' and should been of class ' + aClass.ClassName);

  Result := True;
End;


Function TUixTabsheet.Invariants(Const sLocation : String; oObject : TFslObject; aClass : TFslObjectClass; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, TObject(oObject), aClass, sObject);

  Result := True;
End;


Function TUixTabsheet.Invariants(Const sLocation: String; aReference, aClass: TClass; Const sReference : String): Boolean;
Begin
  // Ensure class is assigned.
  If Not Assigned(aReference) Then
    Invariant(sLocation, sReference + ' was not assigned and was expected to have been of class type ' + aClass.ClassName);

  // Ensure class is of the expected class.
  If Not aReference.InheritsFrom(aClass) Then
    Invariant(sLocation, sReference + ' was of class type ' + aReference.ClassName + ' and should been of class type ' + aClass.ClassName);

  Result := True;
End;


Function TUixTabsheet.Condition(bCondition: Boolean; Const sLocation, sMessage: String): Boolean;
Begin
  If Not bCondition Then
    Error(sLocation, sMessage);

  Result := True;
End;


Procedure TUixTabsheet.Initialise;
Begin
  BackgroundColour := clWhite;
End;


Procedure TUixTabsheet.Finalise;
Begin
End;


Procedure TUixTabsheet.PrepareInitialise;
Begin
End;


Procedure TUixTabsheet.PrepareFinalise;
Begin
End;


Procedure TUixTabsheet.Restore;
Begin
End;


Procedure TUixTabsheet.Commit;
Begin
End;


Procedure TUixTabsheet.Refresh;
Begin
  RefreshHeading;

  RefreshClient;
End;


Procedure TUixTabsheet.RefreshHeading;
Begin
  If Caption = '' Then
    Caption := TabHeading;
End;


Procedure TUixTabsheet.RefreshClient;
Begin
End;


Function TUixTabsheet.GetBusy : Boolean;
Begin
  Result := FBusy;
End;


Procedure TUixTabsheet.SetBusy(Const Value : Boolean);
Begin
  FBusy := Value;

  If FBusy Then
    Screen.Cursor := crHourGlass
  Else
    Screen.Cursor := crDefault;
End;


Function TUixTabsheet.TabHeading : String;
Begin
  Result := StringExcludeAround(ClassName, 'T', 'Tabsheet');
End;


Function TUixTabsheetList.GetTabsheet(Const iIndex : Integer) : TUixTabsheet;
Begin
  Result := TUixTabsheet(PointerByIndex[iIndex]);
End;


procedure TUixTabsheet.WMEraseBkgnd(var Message: TWmEraseBkgnd);
Begin
  Brush.Color := Color;
  Inherited;
End;


Function TUixTabsheet.GetBackgroundColour: TColour;
Begin
  Result := Color;
End;


Procedure TUixTabsheet.SetBackgroundColour(Const Value: TColour);
Begin
  Color := Value;
End;



Constructor TUixRadioButton.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  TabStop := True;

  FCanvas := TControlCanvas.Create;
  TControlCanvas(FCanvas).Control := Self;

End;


Destructor TUixRadioButton.Destroy;
Begin
  FCanvas.Free;

  Inherited;
End;


Procedure TUixRadioButton.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixRadioButton.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixRadioButton.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixRadioButton.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixRadioButton.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixRadioButton.ShuffleBottom;
Begin
  Top := 0;
End;


Procedure TUixRadioButton.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixRadioButton.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixRadioButton.ShuffleTop;
Begin
  Top := MaxInt;
End;


Procedure TUixRadioButton.WMPaint(Var aMessage : TWMPaint);
Var
  aFocusRect : TRect;

  iClientRectHeight : Integer;
  iRequiredHeight : Integer;
Begin
  Inherited;

  If FHasFocus And (Caption <> '') Then
  Begin
    FCanvas.Lock;
    Try
      FCanvas.Handle := aMessage.DC;
      Try
        FCanvas.Font := Font;

        TControlCanvas(FCanvas).UpdateTextFlags;

        aFocusRect := ClientRect;

        iClientRectHeight := ClientRect.Bottom - ClientRect.Top;
        iRequiredHeight := IntegerMin(iClientRectHeight, FCanvas.TextHeight(Caption) + 6);

        aFocusRect.Left := 14;
        aFocusRect.Right := aFocusRect.Left + FCanvas.TextWidth(Caption) + 4;
        aFocusRect.Top := (iClientRectHeight - iRequiredHeight) Div 2;
        aFocusRect.Bottom := aFocusRect.Top + iRequiredHeight;

        FCanvas.DrawFocusRect(aFocusRect);
      Finally
        FCanvas.Handle := 0;
      End;
    Finally
      FCanvas.Unlock;
   End;
  End;
End;


Procedure TUixRadioButton.DoEnter;
Begin
  Inherited;

  FHasFocus := True;

  Invalidate;
End;


Procedure TUixRadioButton.DoExit;
Begin
  Inherited;

  FHasFocus := False;

  Invalidate;
End;


Procedure TUixRadioButton.SetChecked(bValue : Boolean);
Var
  bTabStop : Boolean;
Begin
  bTabStop := TabStop;
  Try
    Inherited;
  Finally
    TabStop := bTabStop;
  End;
End;


Function TUixRadioButton.GetValue : Boolean;
Begin
  Result := Checked;
End;


Procedure TUixRadioButton.SetValue(Const Value : Boolean);
Begin
  Checked := Value;
End;


Function TUixRadioButtonList.GetRadioButtonByIndex(iIndex: Integer): TUixRadioButton;
Begin
  Result := TUixRadioButton(Items[iIndex]);
End;


Constructor TUixScrollBox.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  OnMouseWheelUp := MouseWheelUpHandler;
  OnMouseWheelDown := MouseWheelDownHandler;
End;


Destructor TUixScrollBox.Destroy;
Begin

  Inherited;
End;


Procedure TUixScrollBox.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixScrollBox.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixScrollBox.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixScrollBox.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixScrollBox.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixScrollBox.BorderStyleNone;
Begin
  BorderStyle := bsNone;
End;


Procedure TUixScrollBox.BorderStyleSingle;
Begin
  BorderStyle := bsSingle;
End;


Procedure TUixScrollBox.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixScrollBox.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixScrollBox.Initialise;
Begin
End;


Procedure TUixScrollBox.Finalise;
Begin
End;


Procedure TUixScrollBox.Reset;
Begin
  Height := RequiredHeight;
End;


Function TUixScrollBox.RequiredHeight : Integer;
Begin
  // Scrollboxes seem to be uninclined to hiding their scrollbars when approaching the correct scroll height.

  Result := VisibleControlHeight + (2 * BorderWidth) + 15;
End;


Function TUixScrollBox.VisibleControlHeight : Integer;
Var
  iLoop    : Integer;
  oControl : TControl;
Begin
  Result := 0;

  For iLoop := 0 To ControlCount - 1 Do
  Begin
    oControl := Controls[iLoop];

    If oControl.Visible And (oControl.Align In [alTop, alBottom, alClient]) Then
      Inc(Result, oControl.Height);
  End;
End;


Procedure TUixScrollBox.MouseWheelDownHandler(oSender: TObject; aShiftState: TShiftState; aMousePosition: TPoint; Var bHandled: Boolean);
Begin
  VertScrollBar.Position := VertScrollBar.Position + RealCeiling(ClientHeight * 0.05);
  bHandled := True;
End;


Procedure TUixScrollBox.MouseWheelUpHandler(oSender: TObject; aShiftState: TShiftState; aMousePosition: TPoint; Var bHandled: Boolean);
Begin
  VertScrollBar.Position := VertScrollBar.Position - RealCeiling(ClientHeight * 0.05);
  bHandled := True;
End;



Type
  TGroupButton = Class(TUixRadioButton)
    Private
      FInClick : Boolean;

      Procedure CNCommand(Var aMessage : TWMCommand); Message CN_COMMAND;

    Protected
      Procedure KeyDown(Var iKey : Word; aShiftState : TShiftState); Override;
      Procedure KeyPress(Var cKey : Char); Override;

    Public
      Constructor InternalCreate(oRadioGroup : TUixRadioGroup);
      destructor Destroy; Override;
  End;


Constructor TGroupButton.InternalCreate(oRadioGroup : TUixRadioGroup);
Begin
  Inherited Create(oRadioGroup);

  oRadioGroup.FRadioButtonList.Add(Self);
  Visible := False;
  Enabled := oRadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := oRadioGroup.ButtonClick;
  Parent := oRadioGroup;
End;


Destructor TGroupButton.Destroy;
Begin
  TUixRadioGroup(Owner).FRadioButtonList.Remove(Self);

  Inherited Destroy;
End;


Procedure TGroupButton.CNCommand(Var aMessage : TWMCommand);
Begin
  If Not FInClick Then
  Begin
    FInClick := True;
    Try
      If ((aMessage.NotifyCode = BN_CLICKED) Or (aMessage.NotifyCode = BN_DOUBLECLICKED)) And TUixRadioGroup(Parent).CanModify Then
        Inherited;
    Except
      Application.HandleException(Self);
    End;

    FInClick := False;
  End;
End;


Procedure TGroupButton.KeyPress(Var cKey : Char);
Begin
  Inherited KeyPress(cKey);

  TUixRadioGroup(Parent).KeyPress(cKey);

  If ((cKey = #8) Or (cKey = ' ')) And Not TUixRadioGroup(Parent).CanModify Then
    cKey := #0;
End;


Procedure TGroupButton.KeyDown(Var iKey : Word; aShiftState : TShiftState);
Begin
  Inherited KeyDown(iKey, aShiftState);

  TUixRadioGroup(Parent).KeyDown(iKey, aShiftState);
End;


Constructor TUixRadioGroup.Create(oOwner : TComponent);
Begin
  Inherited;

  FAutoSize := True;

  ControlStyle := [csSetCaption, csDoubleClicks];

  FRadioButtonList := TList.Create;

  FItemStringList := TStringList.Create;
  TStringList(FItemStringList).OnChange := ItemsChange;

  FItemIndex := -1;
  FItemHeight := 21;
  FColumnCount := 1;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Destructor TUixRadioGroup.Destroy;
Begin
  SetButtonCount(0);
  TStringList(FItemStringList).OnChange := Nil;
  FItemStringList.Free;
  FRadioButtonList.Free;

  Inherited Destroy;
End;


Procedure TUixRadioGroup.FlipChildren(bAllLevels : Boolean);
Begin
  { The radio buttons are flipped using BiDiMode }
End;


Procedure TUixRadioGroup.ArrangeButtons;
Var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft: Integer;
  iFlags : Integer;
Begin
  If (FRadioButtonList.Count <> 0) And Not FReading Then
  Begin
    DC := GetDC(0);
    SaveFont := SelectObject(DC, Font.Handle);
    GetTextMetrics(DC, Metrics);
    SelectObject(DC, SaveFont);
    ReleaseDC(0, DC);
    ButtonsPerCol := (FRadioButtonList.Count + FColumnCount - 1) Div FColumnCount;
    ButtonWidth := (Width - 10) Div FColumnCount;
    I := Height - Metrics.tmHeight - 5;
    ButtonHeight := FItemHeight;//I Div ButtonsPerCol;

    If Caption <> '' Then
      TopMargin := Metrics.tmHeight + 1 + (I Mod ButtonsPerCol) Div 2
    Else
      TopMargin := 0;

    DeferHandle := BeginDeferWindowPos(FRadioButtonList.Count);
    Try
      For I := 0 To FRadioButtonList.Count - 1 Do
      Begin
        With TGroupButton(FRadioButtonList[I]) Do
        Begin
          BiDiMode := Self.BiDiMode;

          ALeft := (I Div ButtonsPerCol) * ButtonWidth + 8;

          If UseRightToLeftAlignment Then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;

          iFlags := SWP_NOZORDER Or SWP_NOACTIVATE;
          If I = FItemIndex Then
            iFlags := iFlags Or SWP_DRAWFRAME;

          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I Mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight, iFlags);

          Visible := True;
        End;
      End;
    Finally
      EndDeferWindowPos(DeferHandle);
    End;
  End;
End;


Procedure TUixRadioGroup.ButtonClick(oSender : TObject);
Begin
  If Not FUpdating Then
  Begin
    FItemIndex := FRadioButtonList.IndexOf(oSender);

    RefreshTabStop;

    Changed;
    Click;
  End;
End;

Procedure TUixRadioGroup.ItemsChange(oSender : TObject);
Begin
  If Not FReading Then
  Begin
    If FItemIndex >= FItemStringList.Count Then
    Begin
      FItemIndex := FItemStringList.Count - 1;

      RefreshTabStop;
    End;

    UpdateButtons;
  End;
End;

Procedure TUixRadioGroup.Loaded;
Begin
  Inherited Loaded;

  ArrangeButtons;
End;


Procedure TUixRadioGroup.ReadState(oReader : TReader);
Begin
  FReading := True;

  Inherited ReadState(oReader);

  FReading := False;

  UpdateButtons;
End;


Procedure TUixRadioGroup.SetButtonCount(Value : Integer);
Begin
  While FRadioButtonList.Count < Value Do
    TGroupButton.InternalCreate(Self);

  While FRadioButtonList.Count > Value Do
    TGroupButton(FRadioButtonList.Last).Free;
End;


Procedure TUixRadioGroup.SetColumnCount(Value : Integer);
Begin
  If Value < 1 Then
    Value := 1;

  If Value > 16 Then
    Value := 16;

  If FColumnCount <> Value Then
  Begin
    FColumnCount := Value;

    ArrangeButtons;

    Invalidate;
  End;
End;


Procedure TUixRadioGroup.SetItemIndex(Value : Integer);
Begin
  If FReading Then FItemIndex := Value Else
  Begin
    If Value < -1 Then
      Value := -1;

    If Value >= FRadioButtonList.Count Then
      Value := FRadioButtonList.Count - 1;

    If FItemIndex <> Value Then
    Begin
      If FItemIndex >= 0 Then
        TGroupButton(FRadioButtonList[FItemIndex]).Checked := False;

      FItemIndex := Value;

      If FItemIndex >= 0 Then
        TGroupButton(FRadioButtonList[FItemIndex]).Checked := True;

      RefreshTabStop;
    End;
  End;
End;


Procedure TUixRadioGroup.UpdateButtons;
Var
  I: Integer;
Begin
  SetButtonCount(FItemStringList.Count);

  For I := 0 To FRadioButtonList.Count - 1 Do
    TGroupButton(FRadioButtonList[I]).Caption := FItemStringList[I];

  If FItemIndex >= 0 Then
  Begin
    FUpdating := True;

    TGroupButton(FRadioButtonList[FItemIndex]).Checked := True;

    FUpdating := False;
  End;

  ArrangeButtons;
  Invalidate;
End;


Procedure TUixRadioGroup.CMEnabledChanged(Var aMessage : TMessage);
Var
  I: Integer;
Begin
  Inherited;

  For I := 0 To FRadioButtonList.Count - 1 Do
    TGroupButton(FRadioButtonList[I]).Enabled := Enabled;
End;


Procedure TUixRadioGroup.CMFontChanged(Var aMessage : TMessage);
Begin
  Inherited;

  ArrangeButtons;
End;


Procedure TUixRadioGroup.WMSize(Var aMessage : TWMSize);
Begin
  Inherited;

  ArrangeButtons;
End;


Function TUixRadioGroup.CanModify : Boolean;
Begin
  Result := True;
End;


Procedure TUixRadioGroup.GetChildren(aProc : TGetChildProc; oRoot : TComponent);
Begin
End;


Procedure TUixRadioGroup.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixRadioGroup.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixRadioGroup.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixRadioGroup.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixRadioGroup.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixRadioGroup.Paint;
Begin
End;


Procedure TUixRadioGroup.AdjustClientRect(Var aRect : TRect);
Begin
  Inherited;

  If Caption <> '' Then
  Begin
    Canvas.Font := Font;

    Inc(aRect.Top, Canvas.TextHeight('0'));
  End;

  InflateRect(aRect, -1, -1);

  If Ctl3d Then
    InflateRect(aRect, -1, -1);
End;


Procedure TUixRadioGroup.RecalculateHeight;
Begin
  Height := RequiredHeight;
End;


Procedure TUixRadioGroup.AddValue(Const sValue : String);
Begin
  ItemStringList.Add(sValue);

  If AutoSize Then
    RecalculateHeight;
End;


Procedure TUixRadioGroup.AddValues(Const aValueArray : Array Of String);
Var
  iValueArrayIndex : Integer;
Begin
  For iValueArrayIndex := Low(aValueArray) To High(aValueArray) Do
    ItemStringList.Add(aValueArray[iValueArrayIndex]);

  If AutoSize Then
    RecalculateHeight;
End;


Function TUixRadioGroup.GetValue : Integer;
Begin
  Result := ItemIndex;
End;


Procedure TUixRadioGroup.SetValue(Const Value : Integer);
Begin
  ItemIndex := Value;
End;


Function TUixRadioGroup.GetItemHeight : Integer;
Begin
  Result := FItemHeight;
End;


Procedure TUixRadioGroup.SetItemHeight(Const Value : Integer);
Begin
  FItemHeight := Value;

  If AutoSize Then
    RecalculateHeight;
End;


Procedure TUixRadioGroup.RefreshTabStop;
Var
  iItemIndex : Integer;
Begin
  For iItemIndex := 0 To IntegerMin(FItemStringList.Count, FRadioButtonList.Count) - 1 Do
    TGroupButton(FRadioButtonList[iItemIndex]).TabStop := iItemIndex = FItemIndex;
End;


Procedure TUixRadioGroup.DoExit;
Begin
  Inherited;

  RefreshTabStop;
End;


Function TUixRadioGroup.RequiredHeight : Integer;
Var
  iRows : Integer;
Begin
  iRows := RealCeiling(ItemStringList.Count / ColumnCount);

  If Caption <> '' Then
    Result := (iRows * FItemHeight) + Canvas.TextHeight('0')
  Else
    Result := (iRows * FItemHeight);
End;


Function TUixRadioGroup.GetAutoSizeProperty : Boolean;
Begin
  Result := FAutoSize;
End;


Procedure TUixRadioGroup.SetAutoSizeProperty(Const Value : Boolean);
Begin
  If FAutoSize <> Value Then
  Begin
    FAutoSize := Value;

    If FAutoSize Then
      RecalculateHeight;
  End;
End;

Constructor TUixBevel.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Procedure TUixBevel.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixBevel.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixBevel.Initialise;
Begin
End;


Procedure TUixBevel.Finalise;
Begin
End;


Procedure TUixBevel.ShapeBox;
Begin
  Shape := bsBox;
End;


Procedure TUixBevel.ShapeTopLine;
Begin
  Shape := bsTopLine;
End;


Procedure TUixBevel.ShapeBottomLine;
Begin
  Shape := bsBottomLine;
End;


Procedure TUixBevel.ShapeLeftLine;
Begin
  Shape := bsLeftLine;
End;


Procedure TUixBevel.ShapeRightLine;
Begin
  Shape := bsRightLine;
End;


Procedure TUixBevel.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixBevel.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixBevel.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixBevel.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixBevel.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixBevel.StyleLowered;
Begin
  Style := bsLowered;
End;


Procedure TUixBevel.StyleRaised;
Begin
  Style := bsRaised;
End;


Procedure TUixBevel.ShapeSpacer;
Begin
  Shape := bsSpacer;
End;



Constructor TUixSplitter.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  ResizeStylePattern;
End;


Procedure TUixSplitter.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixSplitter.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixSplitter.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixSplitter.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixSplitter.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixSplitter.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixSplitter.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixSplitter.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixSplitter.ShuffleTop;
Begin
  Top := 0;
End;


Procedure TUixSplitter.ResizeStyleLine;
Begin
  ResizeStyle := rsLine;
End;


Procedure TUixSplitter.ResizeStyleNone;
Begin
  ResizeStyle := rsNone;
End;


Procedure TUixSplitter.ResizeStylePattern;
Begin
  ResizeStyle := rsPattern;
End;


Procedure TUixSplitter.ResizeStyleUpdate;
Begin
  ResizeStyle := rsUpdate;
End;


Function TUixSplitterList.GetSplitter(Const iIndex : Integer) : TUixSplitter;
Begin
  Result := TUixSplitter(PointerByIndex[iIndex]);
End;


Procedure TUixSplitter.Paint;
Begin
  If FGradientColour <> 0 Then
  Begin
    Case Align Of
      alLeft, alRight :
      Begin
        ColourGradientVertical(Canvas.Handle, ClientRect, Color, FGradientColour);
      End;

      alTop, alBottom :
      Begin
        ColourGradientHorizontal(Canvas.Handle, ClientRect, Color, FGradientColour);
      End;
    End;
  End
  Else
  Begin
    Inherited;
  End;
End;


Procedure TUixSplitter.MouseUp(aMouseButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer);
Var
  aOnMoved : TNotifyEvent;
Begin
  aOnMoved := OnMoved;

  OnMoved := Nil;
  Try
    Inherited;
  Finally
    OnMoved := aOnMoved;
  End;

  If Assigned(OnMoved) Then
    OnMoved(Self);
End;


Constructor TUixMemo.Create(oOwner : TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Function TUixMemo.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Procedure TUixMemo.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Function TUixMemo.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Procedure TUixMemo.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Function TUixMemo.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Procedure TUixMemo.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Function TUixMemo.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixMemo.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Procedure TUixMemo.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixMemo.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixMemo.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixMemo.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixMemo.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixMemo.BorderNone;
Begin
  BorderStyle := bsNone;
End;


Procedure TUixMemo.BorderSingle;
Begin
  BorderStyle := bsSingle;
End;


Procedure TUixMemo.ScrollbarVertical;
Begin
  Scrollbars := ssVertical;
End;


Procedure TUixMemo.ScrollbarBoth;
Begin
  Scrollbars := ssBoth;
End;


Procedure TUixMemo.CreateParams(Var aParamsRecord : TCreateParams);
Begin
  SetHeightByLines;

  Inherited CreateParams(aParamsRecord);
End;


Procedure TUixMemo.SetHeightByLines;
Begin
  If FHeightInLines > 0 Then
    Height := CalculateHeightByLineCount(FHeightInLines);
End;


Function TUixMemo.GetReadOnly : Boolean;
Begin
  Result := Inherited ReadOnly;
End;


Procedure TUixMemo.SetReadOnly(Const Value : Boolean);
Begin
  Inherited ReadOnly := Value;

  If ReadOnly Then
    Color := clBtnFace
  Else
    Color := clWindow;
End;


Function TUixMemo.CalculateHeightByLineCount(Const iLineCount: Integer): Integer;
Var
  hDeviceContext : HDC;
  aTextMetricRecord : TTextMetric;
Begin
  hDeviceContext := GetDC(0);
  Try
    SelectObject(hDeviceContext, Font.Handle);
    GetTextMetrics(hDeviceContext, aTextMetricRecord);

    Result := (iLineCount * aTextMetricRecord.tmHeight) + ((aTextMetricRecord.tmDescent + 1) * 2);
  Finally
    ReleaseDC(0, hDeviceContext);
  End;
End;



Constructor TUixTreeViewIdentifiers.Create;
Begin
  Inherited;

  IgnoreDuplicates;
  Sensitive := True;
  Sorted;
End;


Function TUixTreeViewIdentifiers.Link : TUixTreeViewIdentifiers;
Begin
  Result := TUixTreeViewIdentifiers(Inherited Link);
End;


Function TUixTreeViewIdentifiers.Clone : TUixTreeViewIdentifiers;
Begin
  Result := TUixTreeViewIdentifiers(Inherited Clone);
End;


Procedure TUixTreeViewIdentifiers.Remove(Const sValue: String);
Var
  iIndex : Integer;
Begin
  If Find(sValue, iIndex) Then
    DeleteByIndex(iIndex);
End;


Procedure TUixTreeViewIdentifiers.Toggle(Const sValue: String);
Var
  iIndex : Integer;
Begin
  If Find(sValue, iIndex) Then
    DeleteByIndex(iIndex)
  Else
    Insert(iIndex, sValue);
End;


Procedure TUixTreeViewColumn.Assign(oObject: TFslObject);
Begin
  Inherited;

  FAlignment := TUixTreeViewColumn(oObject).Alignment;
  FAlwaysShow := TUixTreeViewColumn(oObject).AlwaysShow;
  FBackground := TUixTreeViewColumn(oObject).Background;
  FEnabled := TUixTreeViewColumn(oObject).Enabled;
  FFooter := TUixTreeViewColumn(oObject).Footer;
  FAlternateFooter := TUixTreeViewColumn(oObject).AlternateFooter;
  FPosition := TUixTreeViewColumn(oObject).Position;
  FSortable := TUixTreeViewColumn(oObject).Sortable;
  FVisible := TUixTreeViewColumn(oObject).Visible;
  FWidth := TUixTreeViewColumn(oObject).Width;
End;


Function TUixTreeViewColumn.Clone: TUixTreeViewColumn;
Begin
  Result := TUixTreeViewColumn(Inherited Clone);
End;


Constructor TUixTreeViewColumn.Create;
Begin
  Inherited;

  FBackground := clNone;
  FEnabled := True;
  FVisible := True;
  FWidth := 140;
End;


Function TUixTreeViewColumn.Link: TUixTreeViewColumn;
Begin
  Result := TUixTreeViewColumn(Inherited Link);
End;


Procedure TUixTreeViewNode.Clear;
Begin
  Assert(Invariants('Clear', FCaptions, TFslStringList, 'FCaptions'));
  Assert(Invariants('Clear', FCaptionVisibilities, TFslBooleanList, 'FCaptionVisibilities'));
  Assert(Invariants('Clear', FChildren, TUixTreeViewNodes, 'FChildren'));

  FParent := Nil;
  FIndex := -1;
  FID := '';
  FNode := Nil;
  FChildren.Clear;

  FHasChildren := False;
  FExpanded := False;
  FSelected := False;
  FEnabledCheck := True;
  FEnabledText := True;
  FVisible := True;

  FCheckState := htcsUncheckedNormal;
  FCheckType := htctNone;

  FCaptions.Clear;
  FCaptionVisibilities.Clear;
  FImage := -1;
  FData := Nil;
  FState := -1;
End;


Constructor TUixTreeViewColumns.Create;
Begin
  Inherited;

  FAlwaysSortable := True;
End;


Function TUixTreeViewColumns.CompareByPosition(pA, pB: Pointer): Integer;
Begin
  Result := IntegerCompare(TUixTreeViewColumn(pA).Position, TUixTreeViewColumn(pB).Position);
End;


Procedure TUixTreeViewColumns.SortedByPosition;
Begin
  SortedBy(CompareByPosition);
End;


Function TUixTreeViewColumns.IsSorted(Const sCaption : String) : Boolean;
Var
  oColumn : TUixTreeViewColumn;
Begin
  oColumn := Get(FSortColumn);

  Result := Assigned(oColumn) And oColumn.Sortable And (StringCompareSensitive(sCaption, oColumn.Name) = 0);
End;


Procedure TUixTreeViewColumns.Hide;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    Columns[iLoop].Visible := False;
End;


Procedure TUixTreeViewColumns.Show;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    Columns[iLoop].Visible := True;
End;


Procedure TUixTreeViewColumns.ClearFooters;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    Columns[iLoop].Footer := '';
End;


Procedure TUixTreeViewColumns.Reposition(Const sCaption: String; iPosition: Integer);
Var
  oColumns : TUixTreeViewColumns;
  iLoop    : Integer;
  iIndex   : Integer;
Begin
  oColumns := TUixTreeViewColumns(Duplicate);
  Try
    For iLoop := 0 To Count - 1 Do
      oColumns.Add(Columns[iLoop].Link);

    oColumns.SortedByPosition;

    iIndex := oColumns.IndexByName(sCaption);

    If oColumns.ExistsByIndex(iIndex) And (iIndex <> iPosition) Then
      oColumns.Move(iIndex, iPosition);

    For iLoop := 0 To oColumns.Count - 1 Do
      oColumns[iLoop].Position := iLoop;
  Finally
    oColumns.Free;
  End;
End;


Procedure TUixTreeViewColumns.Move(Const sCaption: String; iPosition: Integer);
Var
  iIndex : Integer;
Begin
  iIndex := IndexByName(sCaption);

  If ExistsByIndex(iIndex) And (iIndex <> iPosition) Then
    Move(iIndex, iPosition);
End;


Function TUixTreeViewNode.HasCheckedChildren : Boolean;
Var
  iCount   : Integer;
  iLoop    : Integer;
  oNode    : TUixTreeViewNode;
Begin
  // Are all of the children checked?
  iLoop := 0;
  iCount := Children.Count;

  Assert(iCount > 0, 'Node has no children.');

  Result := True;

  While Result And (iLoop < iCount) Do
  Begin
    oNode := Children[iLoop];

    Result := oNode.Checked And ((oNode.Children.Count = 0) Or oNode.HasCheckedChildren);

    Inc(iLoop);
  End;

  Result := Result And (iLoop = iCount);
End;


Function TUixTreeViewNode.HasMixedChildren : Boolean;
Var
  bChecked : Boolean;
  bUnchecked : Boolean;
  iCount : Integer;
  iLoop  : Integer;
  oNode  : TUixTreeViewNode;
Begin
  // Are some (but not all) of the children checked?
  iLoop := 0;
  iCount := Children.Count;
  bChecked := False;
  bUnchecked := False;
  Result := False;

  While Not Result And (iLoop < iCount) Do
  Begin
    oNode := Children[iLoop];

    bChecked := bChecked Or oNode.Checked;
    bUnchecked := bUnchecked Or Not oNode.Checked;

    Result := (bChecked And bUnchecked) Or oNode.HasMixedChildren;

    Inc(iLoop);
  End;
End;


Function TUixTreeViewColumns.GetColumn(Const iIndex: Integer): TUixTreeViewColumn;
Begin
  Result := TUixTreeViewColumn(Inherited ObjectByIndex[iIndex]);
End;


Procedure TUixTreeViewColumns.SetColumn(Const iIndex: Integer; Const Value: TUixTreeViewColumn);
Begin
  Inherited ObjectByIndex[iIndex] := Value;
End;


Function TUixTreeViewColumns.ItemClass: TFslObjectClass;
Begin
  Result := TUixTreeViewColumn;
End;


Constructor TUixTreeView.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);

  FUseStandardSelectionColours := True;
End;


Destructor TUixTreeView.Destroy;
Begin
  Inherited;
End;


Procedure TUixTreeView.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixTreeView.BeforeDestruction;
Begin
  Finalise;

  Inherited;
End;


Procedure TUixTreeView.Initialise;
Begin
  Inherited;

  FMatch := TFslStringObjectMatch.Create;
  FMatch.SortedByKey;

  FColumns := ColumnsClass.Create;
  FRoot := TUixTreeViewNode.Create;
  FExpandIdentifierList := TUixTreeViewIdentifiers.Create;
  FSelectIdentifierList := TUixTreeViewIdentifiers.Create;
  FReselectIdentifierList := TUixTreeViewIdentifiers.Create;
  FCheckIdentifierList := TUixTreeViewIdentifiers.Create;
  FMixedCheckIdentifierList := TUixTreeViewIdentifiers.Create;
  FRenderIdentifierList := TUixTreeViewIdentifiers.Create;

  Header.Options := Header.Options + [hoDblClickResize];
  TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toGhostedIfUnfocused];

  Inherited OnEdited := DoEdited;

  AutoTriStateTracking := False;
  CentreScrollIntoView := False;
  CheckImageKind := ckSystemDefault;
  CheckSupport := True;
  ExtendedFocus := True;
  SelectFullRow := True;
  HeaderVisible := True;
  HintMode := hmTooltip;
  HotCursor := crHandPoint;
  HotTrack := True;
  RightClickSelect := True;
  ShowHeaderSortGlyphs := True;
  ShowHint := True;
  AutoSizeColumn := -1;
End;


Procedure TUixTreeView.Finalise;
Begin
  FMatch.Free;
  FColumns.Free;
  FRoot.Free;

  // Some methods are run after Finalise in VirtualTrees that are overriden and
  // use these objects (so they must be set to Nil).
  FExpandIdentifierList.Free;
  FExpandIdentifierList := Nil;

  FSelectIdentifierList.Free;
  FSelectIdentifierList := Nil;

  FReselectIdentifierList.Free;
  FReselectIdentifierList := Nil;

  FMixedCheckIdentifierList.Free;
  FMixedCheckIdentifierList := Nil;

  FCheckIdentifierList.Free;
  FCheckIdentifierList := Nil;

  FRenderIdentifierList.Free;
  FRenderIdentifierList := Nil;

  Inherited;
End;


Function TUixTreeView.GetOptions : TStringTreeOptions;
Begin
  Result := TStringTreeOptions(Inherited TreeOptions);
End;


Procedure TUixTreeView.SetOptions(Const Value : TStringTreeOptions);
Begin
  Inherited TreeOptions.Assign(Value);
End;


Function TUixTreeView.GetOptionsClass : TTreeOptionsClass;
Begin
  Result := OptionsClass;
End;


Function TUixTreeView.OptionsClass : TTreeOptionsClass;
Begin
  Result := TStringTreeOptions;
End;


Procedure TUixTreeView.RefreshColumns;
Var
  iLoop : Integer;
  iEnabledColumnCount : Integer;
  oItem : TUixTreeViewColumn;
  oColumn : TVirtualTreeColumn;
Begin
  While (Columns.Count < Header.Columns.Count) Do
    Header.Columns.Delete(Header.Columns.Count - 1);

  While (Columns.Count > Header.Columns.Count) Do
    Header.Columns.Add;

  iEnabledColumnCount := 0;

  For iLoop := 0 To Columns.Count - 1 Do
  Begin
    oItem := Columns[iLoop];

    If oItem.Enabled Then
      Inc(iEnabledColumnCount);
  End;

  For iLoop := 0 To Columns.Count - 1 Do
  Begin
    oItem := Columns[iLoop];
    oColumn := Header.Columns[iLoop];

    oItem.Column := oColumn;

    oColumn.Alignment := oItem.Alignment;

    If oItem.Background <> clNone Then
      oColumn.Color := oItem.Background;

    If oItem.Enabled Then
    Begin
      oColumn.Position := oItem.Position;
    End
    Else
    Begin
      oColumn.Position := iEnabledColumnCount;

      Inc(iEnabledColumnCount);
    End;

    oColumn.Tag := Integer(oItem);
    oColumn.Text := oItem.Name;
//    oColumn.Visible := oItem.Visible And oItem.Enabled;
    oColumn.Width := oItem.Width;

    If oItem.Sortable Then
      oColumn.Options := oColumn.Options + [coAllowClick]
    Else
      oColumn.Options := oColumn.Options - [coAllowClick];
  End;

  Header.SortColumn := Columns.SortColumnIndex;

  If Assigned(HeaderMenu) Then
    HeaderMenu.Refresh;
End;


Procedure TUixTreeView.Get(oNode : TUixTreeViewNode);
Begin
  Assert(FRoot.Invariants('Get', oNode, TUixTreeViewNode, 'oNode'));

  oNode.CheckState := htcsUncheckedNormal;
  oNode.CheckType := CheckType;
  oNode.Captions.Clear;
  oNode.CaptionVisibilities.Clear;
  oNode.Background := Color;
  oNode.Foreground := Font.Color;
  oNode.Styles := Font.Style;
  oNode.Image := -1;
  oNode.Data := Nil;
  oNode.State := -1;

  If Not IsDestroying And Assigned(FOnGetNode) Then
    FOnGetNode(Self, oNode);

  FRenderIdentifierList.Remove(oNode.ID);
End;


Procedure TUixTreeView.RefreshState(oNode : TUixTreeViewNode);
Var
  pNode  : PVirtualNode;
  pChild : PVirtualNode;
  oChild : TUixTreeViewNode;
  sID    : String;
Begin
  Assert(FRoot.Invariants('TUixTreeView.RefreshState', TUixTreeViewNode));

  pNode := oNode.Node;

  Assert(pNode.Item = oNode, 'TUixTreeView.RefreshState : Virtual node mismatch.');

  If pNode <> RootNode Then
    InitNode(pNode);

  sID := oNode.ID;

  // Selected nodes.
  If FSelectIdentifierList.ExistsByValue(sID) Then
  Begin
    FReselectIdentifierList.Add(sID);

    AddToSelection(pNode, false);
  End;

  // Focused node
  If (FFocusedID <> '') And (sID = FFocusedID) Then
    FocusedNode := pNode;

  // Topmost node.
  If (FTopID <> '') And (FTopID = sID) Then
    TopNode := pNode;

  pChild := GetFirstChild(pNode);
  While Assigned(pChild) Do
  Begin
    oChild := Get(pChild);

    oChild.Parent := oNode;
    oChild.Index := pChild.Index;
    oChild.Node := pChild;
    pChild.Item := oChild;

    RefreshState(oChild);

    pChild := GetNextSibling(pChild);
  End;
End;


Procedure TUixTreeView.RefreshNode(oNode : TUixTreeViewNode);
Begin
  Assert(FRoot.Invariants('RefreshNode', oNode, TUixTreeViewNode, 'oNode'));

  Get(oNode);

  UpdateMatch(oNode);

  oNode.CheckState := CheckState(oNode);
End;


Procedure TUixTreeView.RefreshCheckState(oNode : TUixTreeViewNode);
Var
  iLoop : Integer;
  oChild : TUixTreeViewNode;
Begin
  Assert(FRoot.Invariants('RefreshCheckState', oNode, TUixTreeViewNode, 'oNode'));

  RefreshNode(oNode);

  For iLoop := 0 To oNode.Children.Count - 1 Do
  Begin
    oChild := oNode.Children[iLoop];

    oChild.Parent := oNode;
    oChild.Index := iLoop;

    RefreshCheckState(oChild);
  End;

  If oNode.Checked Then
    Resolve(oNode);
End;


Procedure TUixTreeView.RefreshNodes;
Begin
  Assert(FColumns.Invariants('RefreshNodes', FRoot, TUixTreeViewNode, 'oNode'));

  FMatch.Clear;
  FRoot.Clear;
  FRenderIdentifierList.Clear;

  RefreshNode(FRoot);
  RefreshChildren(FRoot);

  Count := FRoot.Children.Count;

  FRoot.Node := RootNode;
  FRoot.Node.Item := FRoot;

  FReselectIdentifierList.Clear;

  RefreshState(FRoot);

  FSelectIdentifierList.Assign(FReselectIdentifierList);
End;


Procedure TUixTreeView.Refresh;
Var
  oNode : TUixTreeViewNode;
Begin
  BeginUpdate;
  Try
    Count := 0;

    RefreshColumns;
    RefreshNodes;
  Finally
    EndUpdate;

    // Updates to TopNode while updating do not trigger scrolling, so must manually update the topnode again
    // after call to EndUpdate. The TopID property is not necessarily accurate, as it doesn't use FTopID,
    // so use FTopID to set the value of TopNode.
    If FTopID <> '' Then
    Begin
      oNode := GetByID(FTopID);

      If Assigned(oNode) Then
        TopNode := oNode.Node;
    End;
  End;
End;


Procedure TUixTreeView.Render;
Var
  iLoop : Integer;
Begin
  // Root node is not visible.
  FRenderIdentifierList.Clear;

  For iLoop := 0 To Root.Children.Count - 1 Do
    RenderNode(Root.Children[iLoop]);

  Invalidate;
End;


Procedure TUixTreeView.DoColumnResize(iColumn: TColumnIndex);
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    Columns[iColumn].Width := Header.Columns[iColumn].Width;

    If Not IsDestroying And Assigned(FOnColumnResize) Then
      FOnColumnResize(iColumn);
  End;
End;


Function TUixTreeViewNode.GetShowCheckbox: Boolean;
Begin
  Result := FCheckType = htctCheckbox;
End;


Procedure TUixTreeViewNode.SetShowCheckbox(Const Value: Boolean);
Begin
  If Value Then
    FCheckType := htctCheckbox
  Else
    FCheckType := htctNone;
End;


Function TUixTreeViewNode.GetShowRadioButton: Boolean;
Begin
  Result := FCheckType = htctRadioButton;
End;


Procedure TUixTreeViewNode.SetShowRadioButton(Const Value: Boolean);
Begin
  If Value Then
    FCheckType := htctRadioButton
  Else
    FCheckType := htctNone;
End;


Function TUixTreeViewNodes.ItemClass: TFslObjectClass;
Begin
  Result := TUixTreeViewNode;
End;


Function TUixTreeViewNodes.GetNode(Const iIndex: Integer): TUixTreeViewNode;
Begin
  If Not Assigned(ObjectByIndex[iIndex]) Then
    ObjectByIndex[iIndex] := ItemNew;

  Result := TUixTreeViewNode(Inherited ObjectByIndex[iIndex]);
End;


Procedure TUixTreeViewNodes.SetNode(Const iIndex: Integer; Const Value: TUixTreeViewNode);
Begin
  Inherited ObjectByIndex[iIndex] := Value;
End;


Constructor TUixTreeViewNode.Create;
Begin
  Inherited;

  FCaptions := TFslStringList.Create;
  FCaptionVisibilities := TFslBooleanList.Create;
  FChildren := TUixTreeViewNodes.Create;
  FEnabledCheck := True;
  FEnabledText := True;
  FImage := -1;
End;


Destructor TUixTreeViewNode.Destroy;
Begin
  FChildren.Free;
  FCaptions.Free;
  FCaptionVisibilities.Free;

  Inherited;
End;


Function TUixTreeView.GetCount: Integer;
Begin
  Result := RootNodeCount;
End;


Procedure TUixTreeView.SetCount(Const Value: Integer);
Begin
  Clear;
  RootNodeCount := Value;
End;


Function TUixTreeView.GetHeaderVisible: Boolean;
Begin
  Result := hoVisible In Header.Options;
End;


Procedure TUixTreeView.SetHeaderVisible(Const Value: Boolean);
Begin
  If Value Then
    Header.Options := Header.Options + [hoVisible]
  Else
    Header.Options := Header.Options - [hoVisible];
End;


Function TUixTreeView.GetShowRoot: Boolean;
Begin
  Result := toShowRoot In TreeOptions.PaintOptions;
End;


Procedure TUixTreeView.SetShowRoot(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowRoot]
  Else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowRoot];
End;


Function TUixTreeView.GetSelectFullRow: Boolean;
Begin
  Result := toFullRowSelect In TreeOptions.SelectionOptions;
End;


Procedure TUixTreeView.SetSelectFullRow(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toFullRowSelect]
  Else
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toFullRowSelect];
End;


Procedure TUixTreeView.DoInitNode(pParent, pNode : PVirtualNode; Var aInitStates : TVirtualNodeInitStates);
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := Prepare(pNode);

  oNode.Visible := vsVisible In pNode.States;

  pNode.CheckState := TCheckState(oNode.CheckState);
  pNode.CheckType := TCheckType(oNode.CheckType);

  If oNode.Expanded Then
    Include(aInitStates, ivsExpanded)
  Else
    Exclude(aInitStates, ivsExpanded);

  Inherited;
End;


Procedure TUixTreeView.DoGetText(var pEventArgs: TVSTGetCellTextEventArgs);
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := Render(pEventArgs.Node);

  If Assigned(oNode) Then
  Begin
    Assert(FRoot.Invariants('DoGetText', oNode, TUixTreeViewNode, 'oNode'));

    If oNode.Captions.ExistsByIndex(pEventArgs.Column) Then
    Begin
      If (Not oNode.CaptionVisibilities.ExistsByIndex(pEventArgs.Column)) Or oNode.CaptionVisibilities[pEventArgs.Column] Then
        pEventArgs.CellText := oNode.Captions[pEventArgs.Column]
      Else
        pEventArgs.CellText := '';
    End
    Else
      pEventArgs.CellText := '';
  End;

  Inherited;
End;


Procedure TUixTreeView.Render(oNode : TUixTreeViewNode);
Begin
  If Not IsDestroying And Assigned(FOnRenderNode) And Not FRenderIdentifierList.ExistsByValue(oNode.ID) Then
  Begin
    FRenderIdentifierList.Add(oNode.ID);

    FOnRenderNode(Self, oNode);
  End;
End;


Procedure TUixTreeView.RenderNode(oNode : TUixTreeViewNode);
Var
  iLoop : Integer;
Begin
  Render(oNode);

  For iLoop := 0 To oNode.Children.Count - 1 Do
    RenderNode(oNode.Children[iLoop]);
End;


Function TUixTreeView.Render(pNode : PVirtualNode) : TUixTreeViewNode;
Begin
  Result := Prepare(pNode);

  If Assigned(Result) Then
    Render(Result);
End;


Function TUixTreeView.Prepare(pNode: PVirtualNode) : TUixTreeViewNode;
Var
  oParent : TUixTreeViewNode;
Begin
  Assert(Assigned(pNode), 'Node must be assigned.');

  If Assigned(pNode.Parent) And Assigned(pNode.Parent.Item) Then
    oParent := TUixTreeViewNode(pNode.Parent.Item)
  Else
    oParent := FRoot;

  Assert(FRoot.Invariants('Prepare', oParent, TUixTreeViewNode, 'oParent'));

  If oParent.Children.ExistsByIndex(pNode.Index) Then
    Result := oParent.Children[pNode.Index]
  Else
    Result := Nil;

  Assert(FRoot.Invariants('Prepare', Result, TUixTreeViewNode, 'Result'));

  Result.Node := pNode;
  pNode.Item := Result;

  Result.Expanded := IsExpanded(Result);
  Result.Selected := IsSelected(Result);
//  Result.CheckType := CheckType;
  Result.CheckState := CheckState(Result);

  If Result.HasChildren Or (Result.Children.Count > 0) Then
    Include(pNode.States, vsHasChildren)
  Else
    Exclude(pNode.States, vsHasChildren);
End;


Function TUixTreeViewColumns.Add(Const sName: String): TUixTreeViewColumn;
Begin
  Result := TUixTreeViewColumn.Create;
  Result.Name := sName;

  Add(Result);
End;


Function TUixTreeViewColumns.Add(oValue: TFslObject): Integer;
Begin
  Result := Inherited Add(oValue);

  TUixTreeViewColumn(oValue).Position := Result;
  TUixTreeViewColumn(oValue).Sortable := FAlwaysSortable;
End;


Procedure TUixTreeView.DoCollapsed(pNode: PVirtualNode);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      oNode.CheckState := CheckState(oNode);

      If Not IsDestroying And Assigned(FOnCollapsed) Then
        FOnCollapsed(Self, oNode);
    End;
  End;
End;


Function TUixTreeView.DoCollapsing(pNode: PVirtualNode): Boolean;
Var
  oNode : TUixTreeViewNode;
Begin
  Result := Inherited DoCollapsing(pNode);

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Result And Assigned(oNode) Then
    Begin
      If Not IsDestroying And Assigned(FOnCollapsing) Then
        FOnCollapsing(Self, oNode, Result);

      If Result Then
      Begin
        FExpandIdentifierList.Remove(oNode.ID);

        oNode.Expanded := False;

        Get(oNode);

        oNode.CheckState := CheckState(oNode);

        RefreshState(oNode);
      End;
    End;
  End;
End;


Procedure TUixTreeView.DoExpanded(pNode: PVirtualNode);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      oNode.CheckState := CheckState(oNode);

      If Not IsDestroying And Assigned(FOnExpanded) Then
        FOnExpanded(Self, oNode);
    End;
  End;
End;


Function TUixTreeView.DoExpanding(pNode : PVirtualNode): Boolean;
Var
  oNode : TUixTreeViewNode;
Begin
  Result := Inherited DoExpanding(pNode);

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) And Result Then
    Begin
      If Not IsDestroying And Assigned(FOnExpanding) Then
        FOnExpanding(Self, oNode, Result);

      If Result Then
      Begin
        oNode.HasChildren := False;

        oNode.Expanded := oNode.Children.Count > 0;

        If oNode.Expanded Then
          FExpandIdentifierList.Add(oNode.ID);

        Get(oNode);

        oNode.CheckState := CheckState(oNode);

        BeginUpdate;
        Try
          RefreshChildren(oNode);
        Finally
          EndUpdate;
        End;
      End;
    End;
  End;
End;


function TUixTreeView.DoInitChildren(pNode : PVirtualNode; Var iChildCount : Cardinal) : boolean;
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := Prepare(pNode);

  Assert(FRoot.Invariants('DoInitChildren', oNode, TUixTreeViewNode, 'oNode'));

  iChildCount := oNode.Children.Count;

  result := Inherited;
End;


Function TUixTreeView.GetSelected: TUixTreeViewIdentifier;
Begin
  Result := GetFocused;
End;


Procedure TUixTreeView.SetSelected(Const Value : TUixTreeViewIdentifier);
Begin
  FSelectIdentifierList.Clear;

  If Value <> '' Then
    FSelectIdentifierList.Add(Value);

  FocusedID := Value;
End;


Function TUixTreeView.GetFocused: TUixTreeViewIdentifier;
Var
  oNode : TUixTreeViewNode;
Begin
  If IsUpdating Then
    Result := FFocusedID
  Else
  Begin
    oNode := SelectedNode;

    If Assigned(oNode) Then
      Result := oNode.ID
    Else
      Result := '';
  End;
End;


Procedure TUixTreeView.SetFocused(Const Value : TUixTreeViewIdentifier);
Begin
  FFocusedID := Value;
End;


Function TUixTreeView.GetTop : TUixTreeViewIdentifier;
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := Get(TopNode);

  If Assigned(oNode) Then
    Result := oNode.ID
  Else
    Result := FTopID;
End;


Procedure TUixTreeView.SetTop(Const Value : TUixTreeViewIdentifier);
Begin
  FTopID := Value;
End;


Procedure TUixTreeView.DoFocusChange(pNode : PVirtualNode; iColumn : TColumnIndex);
Var
  sID : String;
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      //Caution: When the focus is changed using the keyboard the selection is not updated until after DoFocusChange is called
      //         To account for this we also set the FFocusedID in DoSelectionChange
      If Selected[pNode] Then
      Begin
        sID := oNode.ID;

        FFocusedID := sID;

        FSelectIdentifierList.Add(sID);
      End;
    End
    Else
    Begin
      FFocusedID := '';

      ClearSelection;
    End;

    If Not IsDestroying And Assigned(FOnFocusChange) Then
      FOnFocusChange(Self, oNode, iColumn);
  End;
End;


Procedure TUixTreeView.DoSelectionChange;
Var
  sID : String;
  oFocusedNode : TUixTreeViewNode;
Begin
  If Not IsUpdating Then
  Begin
    If Assigned(FocusedNode) And Selected[FocusedNode] Then
    Begin
      oFocusedNode := Get(FocusedNode);

      If Assigned(oFocusedNode) Then
      Begin
        sID := oFocusedNode.ID;

        FFocusedID := sID;
      End;
    End;

    If Not IsDestroying And Assigned(FOnSelectionChange) Then
      FOnSelectionChange(Self);
  End;
End;


Function TUixTreeView.DoFocusChanging(pOldNode, pNewNode: PVirtualNode; iOldColumn, iNewColumn: TColumnIndex) : Boolean;
Var
  oNode : TUixTreeViewNode;
Begin
  Result := Inherited DoFocusChanging(pOldNode, pNewNode, iOldColumn, iNewColumn);

  If Not IsUpdating Then
  Begin
    oNode := Get(pNewNode);

    If Assigned(oNode) Then
    Begin
      If Not IsDestroying And Assigned(FOnFocusChanging) Then
        FOnFocusChanging(Self, oNode, iNewColumn, Result);
    End;
  End;
End;


Procedure TUixTreeView.MouseDown(aButton: TMouseButton; aShift: TShiftState; iX, iY: Integer);
Var
  aHitInfo : THitInfo;
Begin
  Inherited;

  GetHitTestInfoAt(iX, iY, True, aHitInfo);

  If Not Assigned(aHitInfo.HitNode) Then
    FocusedNode := Nil;
End;


Procedure TUixTreeView.AddToSelection(Const aNewItems: TNodeArray; iNewLength: Integer; bForceInsert: Boolean);
Var
  iLoop : Integer;
  oNode : TUixTreeViewNode;
Begin
  If Not IsUpdating Then
  Begin
    For iLoop := 0 To iNewLength - 1 Do
    Begin
      oNode := Get(aNewItems[iLoop]);

      If Assigned(oNode) Then
        FSelectIdentifierList.Add(oNode.ID);
    End;

    DoSelectionChange;
  End;

  Inherited;
End;


Procedure TUixTreeView.AddToSelection(pNode: PVirtualNode; NotifySynced: Boolean);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    Assert(FRoot.Invariants('AddToSelection', oNode, TUixTreeViewNode, 'oNode'));

    FSelectIdentifierList.Add(oNode.ID);

    DoSelectionChange;
  End;
End;


Procedure TUixTreeView.ClearSelection;
Begin
  Inherited;

  If Not IsUpdating And Assigned(FSelectIdentifierList) Then
  Begin
    DoSelectionChange;
  End;
End;


Procedure TUixTreeView.Clear;
Begin
  Inherited;
End;


Procedure TUixTreeView.InternalClearSelection;
Begin
  Inherited;

  If Not IsUpdating And Assigned(FSelectIdentifierList) Then
    FSelectIdentifierList.Clear;
End;


Procedure TUixTreeView.InternalRemoveFromSelection(pNode: PVirtualNode);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating And Assigned(FSelectIdentifierList) Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      RemoveFromSelects(oNode.ID);

      DoSelectionChange;
    End;
  End;
End;


Procedure TUixTreeView.RemoveFromSelection(pNode: PVirtualNode);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
      RemoveFromSelects(oNode.ID);
  End;
End;


Function TUixTreeView.IsSelected: Boolean;
Begin
  Result := FSelectIdentifierList.Count > 0;
End;


Function TUixTreeView.CellBackgroundByNode(const oNode: TUixTreeViewNode): TColor;
Begin
  If oNode.Selected Then
  Begin
    If Focused Then
    Begin
      If Not UseStandardSelectionColours And Not ShowRoot And (oNode.Foreground <> clBlack) And (oNode.Foreground <> clWindowText) Then //clWindowText is the default font colour
      Begin
        // Not ShowRoot limits the change to when the virtual tree is displaying as grid, not as a tree.

        Result := oNode.Foreground;
      End
      Else
      Begin
        Result := clHighlight;
      End;
    End
    Else
    Begin
      Result := Colors.UnfocusedSelectionColor;
    End;
  End
  Else
  Begin
    Result := Color;
  End;
End;


Procedure TUixTreeView.DoBeforeCellPaint(Canvas: TCanvas; Node: PVirtualNode; Column: TColumnIndex; CellPaintMode: TVTCellPaintMode; CellRect: TRect; var ContentRect: TRect);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  oNode := TUixTreeViewNode(Node.Item);

  Colors.FocusedSelectionColor := CellBackgroundByNode(oNode);//oNode.Foreground;
  Colors.FocusedSelectionBorderColor := Colors.FocusedSelectionColor;//oNode.Foreground;

  If Not IsDestroying And Assigned(FOnBeforeCellPaint) Then
    FOnBeforeCellPaint(Self, Get(Node), Canvas, Column, CellRect);
End;


Procedure TUixTreeView.DoAfterCellPaint(aCanvas : TCanvas; pNode : PVirtualNode; iColumn : TColumnIndex; aCellRect : TRect);
Begin
  Inherited;

  // reset the focus colours
  Colors.FocusedSelectionColor := clHighlight;
  Colors.FocusedSelectionBorderColor := clHighlight;

  If Not IsDestroying And Assigned(FOnAfterCellPaint) Then
    FOnAfterCellPaint(Self, Get(pNode), aCanvas, iColumn, aCellRect);
End;


Procedure TUixTreeView.DoBeforePaint(aCanvas: TCanvas);
Begin
  Inherited;

  Header.Font.Assign(Font);
End;


Procedure TUixTreeView.RemoveFromSelects(Const sID: String);
Var
  iIndex : Integer;
Begin
  iIndex := FSelectIdentifierList.IndexByValue(sID);

  If FSelectIdentifierList.ExistsByIndex(iIndex) Then
  Begin
    FSelectIdentifierList.DeleteByIndex(iIndex);

    DoSelectionChange;
  End;
End;


Procedure TUixTreeView.DoFocusNode(pNode: PVirtualNode; bAsk: Boolean);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    Assert(Not Assigned(oNode) Or oNode.Invariants('DoFocusNode', TUixTreeViewNode));

    If Assigned(oNode) Then
      FSelectIdentifierList.Add(oNode.ID)
    Else
      FSelectIdentifierList.Clear;
  End;
End;


Function TUixTreeView.GetExtendedFocus: Boolean;
Begin
  Result := toExtendedFocus In TreeOptions.SelectionOptions;
End;


Procedure TUixTreeView.SetExtendedFocus(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toExtendedFocus]
  Else
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toExtendedFocus];
End;


Function TUixTreeView.GetRightClickSelect: Boolean;
Begin
  Result := toRightClickSelect In TreeOptions.SelectionOptions;
End;


Procedure TUixTreeView.SetRightClickSelect(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toRightClickSelect]
  Else
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toRightClickSelect];
End;


Function TUixTreeView.GetAutoTriStateTracking: Boolean;
Begin
  Result := toAutoTriStateTracking In TreeOptions.AutoOptions;
End;


Procedure TUixTreeView.SetAutoTriStateTracking(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.AutoOptions := TreeOptions.AutoOptions + [toAutoTriStateTracking]
  Else
    TreeOptions.AutoOptions := TreeOptions.AutoOptions - [toAutoTriStateTracking];
End;


Function TUixTreeView.GetShowHeaderSortGlyphs: Boolean;
Begin
  Result := hoShowSortGlyphs In Header.Options;
End;


Procedure TUixTreeView.SetShowHeaderSortGlyphs(Const Value: Boolean);
Begin
  If Value Then
    Header.Options := Header.Options + [hoShowSortGlyphs]
  Else
    Header.Options := Header.Options - [hoShowSortGlyphs];
End;


Function TUixTreeView.GetSortColumn: TUixTreeViewColumnIndex;
Begin
  Result := Columns.SortColumnIndex;
End;


Procedure TUixTreeView.SetSortColumn(Const Value: TUixTreeViewColumnIndex);
Begin
  Columns.SortColumnIndex := Value;
End;


Function TUixTreeView.GetSortDirection: Integer;
Const
  INTEGER_DIRECTION : Array[TSortDirection] Of Integer = (1, -1);
Begin
  Result := INTEGER_DIRECTION[Header.SortDirection];
End;


Procedure TUixTreeView.SetSortDirection(Const Value: Integer);
Begin
  If Value < 0 Then
    Header.SortDirection := sdDescending
  Else
    Header.SortDirection := sdAscending;
End;


Procedure TUixTreeView.DoBeforeItemErase(aCanvas: TCanvas; pNode: PVirtualNode; aItemRect: TRect; Var aColor: TColor; Var aEraseAction: TItemEraseAction);
Var
  oNode : TUixTreeViewNode;
Begin
  aEraseAction := eaColor;

  oNode := Render(pNode);

  If Assigned(oNode) Then
    aColor := oNode.Background;

  Inherited;
End;


Function TUixTreeView.DoGetImageIndex(Node: PVirtualNode; Kind: TVTImageKind; Column: TColumnIndex; var Ghosted: Boolean; var Index: TImageIndex): TCustomImageList;
Var
  oNode : TUixTreeViewNode;
Begin
  If Column = 0 Then
  Begin
    oNode := TUixTreeViewNode(Node.Item);

    If Assigned(oNode) Then
    Begin
      If Kind = ikState Then
        Index := oNode.State
      Else If Kind = ikOverlay Then
        Index := -1
      Else If Kind In [ikSelected, ikNormal] Then
        Index := oNode.Image;

      Ghosted := Not oNode.EnabledCheck;
    End;
  End;

  Result := Inherited DoGetImageIndex(Node, Kind, column, ghosted, index);
End;


Procedure TUixTreeView.DoPaintText(pNode: PVirtualNode; Const aCanvas: TCanvas; iColumn: TColumnIndex; aTextType: TVSTTextType);
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := TUixTreeViewNode(pNode.Item);

  If Assigned(oNode) Then
  Begin
    aCanvas.Font.Style := oNode.Styles;

    If Not oNode.Selected Then
    Begin
      aCanvas.Font.Color := oNode.Foreground;

      If Not oNode.EnabledText Then
        aCanvas.Font.Color := clGray;
    End;

    If Not IsDestroying And Assigned(FOnPaintCell) Then
      FOnPaintCell(Self, oNode, iColumn, aCanvas);
  End;

  Inherited;
End;


Procedure TUixTreeView.DoHeaderClick(const HitInfo: TVTHeaderHitInfo);
Begin
  If Columns.ExistsByIndex(HitInfo.Column) And Columns[HitInfo.Column].Sortable And (HitInfo.Button = mbLeft) Then
  Begin
    If Header.SortColumn = HitInfo.Column Then
      Header.SortDirection := TSortDirection(Not Boolean(Header.SortDirection))
    Else
      Header.SortColumn := HitInfo.Column;

    SortChange(HitInfo.Column);
  End;

  Inherited;
End;


Procedure TUixTreeView.DoHeaderDragged(iColumn: TColumnIndex; iOldPosition: TColumnPosition);
Var
  iLoop : Integer;
Begin
  Inherited;

  For iLoop := 0 To IntegerMax(FColumns.Count, Header.Columns.Count) - 1 Do
    FColumns[iLoop].Position := Header.Columns[iLoop].Position;

  If Not IsDestroying And Assigned(FOnHeaderDragged) Then
    FOnHeaderDragged(iColumn, iOldPosition);
End;


Procedure TUixTreeView.DoScroll(iDeltaX, iDeltaY : Integer);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(TopNode);

    If Assigned(oNode) Then
      FTopID := oNode.ID;

    If iDeltaX <> 0 Then
      DoHorizontalScroll(OffsetX)
    Else If iDeltaY <> 0 Then
      DoVerticalScroll(OffsetY);
  End;
End;


Procedure TUixTreeView.DoHorizontalScroll(Const iOffset : Integer);
Begin
  If Not IsDestroying And Assigned(FOnHorizontalScroll) Then
    FOnHorizontalScroll(Self, iOffset);
End;


Procedure TUixTreeView.DoVerticalScroll(Const iOffset : Integer);
Begin
  If Not IsDestroying And Assigned(FOnVerticalScroll) Then
    FOnVerticalScroll(Self, iOffset);
End;


Function TUixTreeView.Index: Integer;
Begin
  If Assigned(FocusedNode) Then
    Result := FocusedNode.Index
  Else
    Result := -1;
End;


Function TUixTreeView.GetMultiSelect: Boolean;
Begin
  Result := toMultiSelect In TreeOptions.SelectionOptions;
End;


Procedure TUixTreeView.SetMultiSelect(Const Value: Boolean);
Begin
  BeginUpdate;
  Try
    If Value Then
      TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toMultiSelect]
    Else
      TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toMultiSelect];
  Finally
    EndUpdate;
  End;
End;


Procedure TUixTreeView.DoChecked(pNode : PVirtualNode);
Var
  oNode : TUixTreeViewNode;
Begin
  Inherited;

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      BeginUpdate;
      Try
        oNode.CheckState := TUixTreeViewCheckState(pNode.CheckState);

        Case oNode.CheckState Of
          htcsUncheckedNormal, htcsUncheckedPressed :
          Begin
            FCheckIdentifierList.Remove(oNode.ID);
            FMixedCheckIdentifierList.Remove(oNode.ID);
          End;

          htcsCheckedNormal, htcsCheckedPressed :
          Begin
            FCheckIdentifierList.Add(oNode.ID);
            FMixedCheckIdentifierList.Remove(oNode.ID);
          End;

          htcsMixedNormal, htcsMixedPressed :
          Begin
            FCheckIdentifierList.Remove(oNode.ID);
            FMixedCheckIdentifierList.Add(oNode.ID);
          End;
        End;

        If AutoTriStateTracking Then
          RefreshCheckState(oNode);

        If Not IsDestroying And Assigned(FOnChecked) Then
          FOnChecked(Self, oNode);
      Finally
        EndUpdate;
      End;
    End;
  End;
End;


Function TUixTreeView.DoChecking(pNode: PVirtualNode; Var aNewCheckState : TCheckState): Boolean;
Var
  aState : TUixTreeViewCheckState;
  oNode  : TUixTreeViewNode;
Begin
  Result := Inherited DoChecking(pNode, aNewCheckState);

  If Not IsUpdating Then
  Begin
    oNode := Get(pNode);

    If Assigned(oNode) Then
    Begin
      Result := oNode.EnabledCheck;

      If Result Then
      Begin
        Result := Inherited DoChecking(pNode, aNewCheckState);

        If Not IsDestroying And Assigned(FOnChecking) Then
        Begin
          aState := TUixTreeViewCheckState(aNewCheckState);

          FOnChecking(Self, oNode, aState, Result);

          aNewCheckState := TCheckState(aState);
        End;
      End;
    End;
  End;
End;


Function TUixTreeView.GetCheckSupport: Boolean;
Begin
  Result := toCheckSupport In TreeOptions.MiscOptions;
End;


Procedure TUixTreeView.SetCheckSupport(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toCheckSupport]
  Else
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toCheckSupport];
End;


Procedure TUixTreeView.Click;
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnSingleClick) Then
    FOnSingleClick(Self, Get(FocusedNode));
End;


Procedure TUixTreeView.DblClick;
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnDoubleClick) Then
    FOnDoubleClick(Self, Get(FocusedNode));
End;


Function TUixTreeView.GetAutoSizeColumn : TUixTreeViewColumnIndex;
Begin
  Result := Header.AutoSizeIndex;
End;


Procedure TUixTreeView.SetAutoSizeColumn(Const Value : TUixTreeViewColumnIndex);
Begin
  Header.AutoSizeIndex := Value;

  If Value >= 0 Then
    Header.Options := Header.Options + [hoAutoResize]
  Else
    Header.Options := Header.Options - [hoAutoResize]
End;


Function TUixTreeViewColumns.Link: TUixTreeViewColumns;
Begin
  Result := TUixTreeViewColumns(Inherited Link);
End;


Function TUixTreeView.Get(pNode: PVirtualNode): TUixTreeViewNode;
Begin
  If Assigned(pNode) Then
    Result := TUixTreeViewNode(pNode.Item)
  Else
    Result := Nil;

  If Assigned(Result) Then
    Result.Node := pNode;
End;


Function TUixTreeViewNode.GetChecked : Boolean;
Begin
  Result := FCheckState In [htcsCheckedNormal, htcsCheckedPressed];
End;


Procedure TUixTreeViewNode.SetChecked(Const Value: Boolean);
Begin
  If Value Then
    FCheckState := htcsCheckedNormal
  Else
    FCheckState := htcsUncheckedNormal;
End;


Function TUixTreeViewNode.Link: TUixTreeViewNode;
Begin
  Result := TUixTreeViewNode(Inherited Link);
End;


Function TUixTreeViewNodes.Link : TUixTreeViewNodes;
Begin
  Result := TUixTreeViewNodes(Inherited Link);
End;


Function TUixTreeViewColumns.Get(Const sName : String) : TUixTreeViewColumn;
Begin
  Result := TUixTreeViewColumn(Inherited GetByName(sName));
End;


Function TUixTreeViewColumns.Ensure(Const sName : String) : TUixTreeViewColumn;
Begin
  Result := Get(sName);

  If Not Assigned(Result) Then
    raiseError('Ensure', 'Column ' + sName + ' does not exist.');
End;


Function TUixTreeViewColumns.Force(Const sName : String) : TUixTreeViewColumn;
Begin
  Result := Get(sName);

  If Not Assigned(Result) Then
    Result := Add(sName);
End;


Function TUixTreeView.GetHeaderMenu : TUixTreeViewHeaderPopup;
Begin
  Result := TUixTreeViewHeaderPopup(Header.PopupMenu);
End;


Procedure TUixTreeView.SetHeaderMenu(Const Value : TUixTreeViewHeaderPopup);
Begin
  Header.PopupMenu := Value;
End;


Function TUixTreeView.GetImages: TUixImages;
Begin
  Result := TUixImages(Inherited Images);
End;


Procedure TUixTreeView.SetImages(Const Value: TUixImages);
Begin
  Inherited Images := Value;
End;


Procedure TUixTreeViewColumn.AlignCenter;
Begin
  FAlignment := taCenter;
End;


Procedure TUixTreeViewColumn.AlignLeft;
Begin
  FAlignment := taLeftJustify;
End;


Procedure TUixTreeViewColumn.AlignRight;
Begin
  FAlignment := taRightJustify;
End;


Function TUixTreeViewNode.GetBolded: Boolean;
Begin
  Result := fsBold In FStyles;
End;


Procedure TUixTreeViewNode.SetBolded(Const Value: Boolean);
Begin
  If Value Then
    FStyles := FStyles + [fsBold]
  Else
    FStyles := FStyles - [fsBold];
End;


Function TUixTreeViewNode.IsLeaf: Boolean;
Begin
  Result := FChildren.Count = 0;
End;


Function TUixTreeViewNode.IsRoot: Boolean;
Begin
  Result := Not Assigned(FParent);
End;


Function TUixTreeView.GetShowTreeLines: Boolean;
Begin
  Result := toShowTreeLines In TreeOptions.PaintOptions;
End;


Procedure TUixTreeView.SetShowTreeLines(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowTreeLines]
  Else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowTreeLines];
End;


Function TUixTreeView.DoBeforeDrag(pNode : PVirtualNode; iColumn : TColumnIndex) : Boolean;
Begin
  Result := Inherited DoBeforeDrag(pNode, iColumn);

  If Not IsDestroying And Assigned(FOnDragAllowed) Then
    FOnDragAllowed(Self, Get(pNode), Result);
End;


Procedure TUixTreeView.DoStartDrag(Var oDragObject: TDragObject);
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnDragStart) Then
    FOnDragStart(Self);
End;


Procedure TUixTreeView.DoEndDrag(oTarget: TObject; iX, iY: Integer);
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnDragFinish) Then
    FOnDragFinish(Self);
End;


Procedure TUixTreeView.DoDragDrop(oSource: TObject; const oDataObject: IDataObject; const aFormats: TFormatArray; aShift: TShiftState; aPoint: TPoint; Var iEffect: Integer; aMode: TDropMode);
Var
  oTarget : TUixTreeViewNode;
  aFormat : TFormatEtc;
  aMedium : TStgMedium;
  iLoop   : Integer;
Begin
  Inherited;

  iEffect := DROPEFFECT_NONE;

  oTarget := Get(GetNodeAt(aPoint.X, aPoint.Y));

  FillChar(aFormat, SizeOf(aFormat), 0);

  aFormat.cfFormat := CF_HDROP;
  aFormat.tymed := TYMED_HGLOBAL;

  If oDataObject.GetData(aFormat, aMedium) = S_OK Then
  Begin
    If Not IsDestroying And Assigned(FOnDragDropFile) Then
    Begin
      Try
        For iLoop := 0 To FilesCount(aMedium.hGlobal) - 1 Do
          FOnDragDropFile(Self, FilesGet(aMedium.hGlobal, iLoop), oTarget);
      Finally
        ReleaseStgMedium(aMedium);
      End;
    End;
  End
  Else If Not IsDestroying Then
  Begin
    Case aMode Of
      dmAbove :
      Begin
        If Assigned(FOnDragDropBeforeNode) Then
          FOnDragDropBeforeNode(Self, oSource, oTarget);
      End;
      dmBelow :
      Begin
        If Assigned(FOnDragDropAfterNode) Then
          FOnDragDropAfterNode(Self, oSource, oTarget);
      End;
    Else
      FOnDragDropNode(Self, oSource, oTarget);
    End;
  End;
End;


Function TUixTreeView.DoDragOver(oSource: TObject; aShift: TShiftState; aState: TDragState; aPoint: TPoint; aMode: TDropMode; Var iEffect: Integer): Boolean;
Var
  pTarget : PVirtualNode;
  pSource : PVirtualNode;
Begin
  Result := Inherited DoDragOver(oSource, aShift, aState, aPoint, aMode, iEffect);

  pTarget := GetNodeAt(aPoint.X, aPoint.Y);

  If (oSource Is TUixTreeView) Then
  Begin
    pSource := TUixTreeView(oSource).FocusedNode;

    Result := (pSource <> pTarget) And ((oSource <> Self) Or (Not HasAsParent(pTarget, pSource)));
  End
  Else
  Begin
    Result := Not IsDestroying And Not Assigned(oSource) And Assigned(FOnDragDropFile);
  End;

  If Not IsDestroying And Assigned(FOnDragOver) Then
    FOnDragOver(Self, oSource, Get(pTarget), Result);

  iEffect := DROPEFFECT_MOVE;
End;


Function TUixTreeView.CheckState(oNode : TUixTreeViewNode) : TUixTreeViewCheckState;
Begin
  If FCheckIdentifierList.ExistsByValue(oNode.ID) Or (AutoTriStateTracking And (oNode.Children.Count > 0) And oNode.HasCheckedChildren) Then
    Result := htcsCheckedNormal
  Else If FMixedCheckIdentifierList.ExistsByValue(oNode.ID) Or (AutoTriStateTracking And oNode.HasMixedChildren) Then
    Result := htcsMixedNormal
  Else
    Result := htcsUncheckedNormal;
End;


Procedure TUixTreeView.DoCheckClick(pNode : PVirtualNode; aNewCheckState : TCheckState);
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := Get(pNode);

  Assert(FRoot.Invariants('DoCheckClick', TUixTreeViewNode));
  Assert(oNode.ID <> '', 'ID is empty.');

  If aNewCheckState In [csCheckedNormal, csCheckedPressed] Then
  Begin
    FMixedCheckIdentifierList.Remove(oNode.ID);
    FCheckIdentifierList.Add(oNode.ID);
  End
  Else If aNewCheckState In [csMixedNormal, csMixedPressed] Then
  Begin
    FMixedCheckIdentifierList.Add(oNode.ID);
    FCheckIdentifierList.Remove(oNode.ID);
  End
  Else
  Begin
    FMixedCheckIdentifierList.Remove(oNode.ID);
    FCheckIdentifierList.Remove(oNode.ID);
  End;

  Inherited;
End;


Function TUixTreeView.CheckType : TUixTreeViewCheckType;
Begin
  If AutoTriStateTracking Or FShowTriStateCheckboxes Then
    Result := htctTriStateCheckBox
  Else If FShowCheckboxes Then
    Result := htctCheckbox
  Else
    Result := htctNone;
End;


Procedure TUixTreeView.Resolve(oNode : TUixTreeViewNode);
Var
  iLoop    : Integer;
  oCurrent : TUixTreeViewNode;
Begin
  If oNode.CheckState In [htcsUncheckedNormal, htcsUncheckedPressed, htcsCheckedNormal, htcsCheckedPressed] Then
  Begin
    For iLoop := 0 To oNode.Children.Count - 1 Do
    Begin
      oCurrent := oNode.Children[iLoop];
//      oCurrent.Checked := oNode.Checked;

      If oCurrent.Checked Then
        FCheckIdentifierList.Add(oCurrent.ID)
      Else
        FCheckIdentifierList.Remove(oCurrent.ID);

      Resolve(oCurrent);
    End;
  End;
End;


Function TUixTreeViewColumns.GetSortColumnIndex : TUixTreeViewColumnIndex;
Begin
  Result := IndexByName(FSortColumn);
End;


Procedure TUixTreeViewColumns.SetSortColumnIndex(Const Value : TUixTreeViewColumnIndex);
Begin
  If ExistsByIndex(Value) Then
    FSortColumn := Columns[Value].Name
  Else
    FSortColumn := '';
End;


Function TUixTreeView.GetHotTrack: Boolean;
Begin
  Result := hoHotTrack In Header.Options;
End;


Procedure TUixTreeView.SetHotTrack(Const Value : Boolean);
Begin
  If Value Then
    Header.Options := Header.Options + [hoHotTrack]
  Else
    Header.Options := Header.Options - [hoHotTrack];
End;


Procedure TUixTreeView.CMFontChanged(Var aMessage: TMessage);
Begin
  Inherited;

  Header.Font.Assign(Font);
End;


Procedure TUixTreeView.UpdateMatch(oNode : TUixTreeViewNode);
Var
  iIndex : Integer;
Begin
  iIndex := FMatch.IndexByKey(oNode.ID);

  If FMatch.ExistsByIndex(iIndex) Then
    FMatch.ValueByIndex[iIndex] := oNode.Link
  Else
    FMatch.Add(oNode.ID, oNode.Link);
End;


Function TUixTreeView.IsExpanded(oNode : TUixTreeViewNode) : Boolean;
Begin
  Result := FIsExpandAll Or FExpandIdentifierList.ExistsByValue(oNode.ID);
End;


Function TUixTreeView.IsSelected(oNode : TUixTreeViewNode) : Boolean;
Begin
  Result := FIsSelectAll Or FSelectIdentifierList.ExistsByValue(oNode.ID);
End;


Function TUixTreeView.GetByID(Const sID : TUixTreeViewIdentifier) : TUixTreeViewNode;
Var
  iIndex : Integer;
Begin
  iIndex := FMatch.IndexByKey(sID);

  If FMatch.ExistsByIndex(iIndex) Then
    Result := TUixTreeViewNode(FMatch.ValueByIndex[iIndex])
  Else
    Result := Nil;
End;


Function TUixTreeView.SelectedNode : TUixTreeViewNode;
Begin
  // During an update, the FocusedNode may be invalid.

  If IsUpdating Then
    Result := Nil
  Else
    Result := Get(FocusedNode);
End;


Function TUixTreeView.SelectedData : Pointer;
Var
  oNode : TUixTreeViewNode;
Begin
  oNode := SelectedNode;

  If Assigned(oNode) Then
    Result := oNode.Data
  Else
    Result := Nil;
End;


Function TUixTreeView.ColumnsClass : TUixTreeViewColumnsClass;
Begin
  Result := TUixTreeViewColumns;
End;


Function TUixTreeViewNode.GetItalicised : Boolean;
Begin
  Result := fsItalic In FStyles;
End;


Procedure TUixTreeViewNode.SetItalicised(Const Value: Boolean);
Begin
  If Value Then
    FStyles := FStyles + [fsItalic]
  Else
    FStyles := FStyles - [fsItalic];
End;


Procedure TUixTreeView.RefreshChildren(oNode : TUixTreeViewNode);
Var
  iLoop  : Integer;
  oChild : TUixTreeViewNode;
Begin
  Assert(Assigned(oNode), 'oNode must be assigned.');

  For iLoop := 0 To oNode.Children.Count - 1 Do
  Begin
    oChild := oNode.Children[iLoop];

    oChild.Parent := oNode;
    oChild.Index := iLoop;

    RefreshNode(oChild);

    If (oChild.Children.Count > 0) And (AutoTristateTracking Or IsExpanded(oChild)) Then
      RefreshChildren(oChild);
  End;
End;


Function TUixTreeView.GetCheckImage(Node: PVirtualNode; ImgCheckType: TCheckType = ctNone; ImgCheckState: TCheckState = csUncheckedNormal; ImgEnabled: Boolean = True) : Integer;
Const
  DISABLED_CHECKSTATES : Array [ckEmpty..ckButtonDisabled] Of Integer =
    (ckEmpty,
     ckRadioUncheckedDisabled, ckRadioUncheckedDisabled, ckRadioUncheckedDisabled, ckRadioUncheckedDisabled,
     ckRadioCheckedDisabled, ckRadioCheckedDisabled, ckRadioCheckedDisabled, ckRadioCheckedDisabled,
     ckCheckUncheckedDisabled, ckCheckUncheckedDisabled, ckCheckUncheckedDisabled, ckCheckUncheckedDisabled,
     ckCheckCheckedDisabled, ckCheckCheckedDisabled, ckCheckCheckedDisabled, ckCheckCheckedDisabled,
     ckCheckMixedDisabled, ckCheckMixedDisabled, ckCheckMixedDisabled, ckCheckMixedDisabled,
     ckButtonDisabled, ckButtonDisabled, ckButtonDisabled, ckButtonDisabled);
Var
  oNode : TUixTreeViewNode;
Begin
  Result := Inherited GetCheckImage(Node, ImgCheckType, ImgCheckState, ImgEnabled);

  If IntegerBetween(Integer(ckEmpty), Result, Integer(ckButtonDisabled)) Then
  Begin
    oNode := Get(Node);

    If Assigned(oNode) And Not oNode.EnabledCheck Then
      Result := DISABLED_CHECKSTATES[Result];
  End;
End;


Function TUixTreeView.GetShowButtons : Boolean;
Begin
  Result := (toShowButtons In TreeOptions.PaintOptions) And (toToggleOnDblClick In TreeOptions.MiscOptions);
End;


Procedure TUixTreeView.SetShowButtons(Const Value: Boolean);
Begin
  If Value Then
  Begin
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toShowButtons];
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toToggleOnDblClick];
  End
  Else
  Begin
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toShowButtons];
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];
  End;
End;


Function TUixTreeView.GetToggleDoubleClick: Boolean;
Begin
  Result := toToggleOnDblClick In TreeOptions.MiscOptions;
End;


Procedure TUixTreeView.SetToggleDoubleClick(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toToggleOnDblClick]
  Else
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toToggleOnDblClick];
End;


Procedure TUixTreeView.DragModeAutomatic;
Begin
  DragMode := dmAutomatic;
End;


Procedure TUixTreeView.DragModeManual;
Begin
  DragMode := dmManual;
End;


Procedure TUixTreeView.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixTreeView.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixTreeView.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixTreeView.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixTreeView.AlignTop;
Begin
  Align := alTop;
End;


Function TUixTreeView.GetRenderFocused : Boolean;
Begin
  Result := toPopupMode In TreeOptions.PaintOptions;
End;


Procedure TUixTreeView.SetRenderFocused(Const Value : Boolean);
Begin
  If Value Then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toPopupMode]
  Else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toPopupMode];
End;


Function TUixTreeView.GetShowFocused : Boolean;
Begin
  Result := Not (toHideFocusRect In TreeOptions.PaintOptions);
End;


Function TUixTreeView.GetShowSelected : Boolean;
Begin
  Result := Not (toHideSelection In TreeOptions.PaintOptions);
End;


Procedure TUixTreeView.SetShowFocused(Const Value : Boolean);
Begin
  If Value Then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toHideFocusRect]
  Else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toHideFocusRect];
End;


Procedure TUixTreeView.SetShowSelected(Const Value : Boolean);
Begin
  If Value Then
    TreeOptions.PaintOptions := TreeOptions.PaintOptions - [toHideSelection]
  Else
    TreeOptions.PaintOptions := TreeOptions.PaintOptions + [toHideSelection];
End;


Function TUixTreeView.IsSortAscending: Boolean;
Begin
  Result := SortDirection > 0;
End;


Function TUixTreeView.IsSortDescending: Boolean;
Begin
  Result := SortDirection < 0;
End;


function TUixTreeView.IsUpdating: boolean;
begin
  result := false;
end;

Function TUixTreeView.GetGridExtensions: Boolean;
Begin
  Result := toGridExtensions In TreeOptions.MiscOptions;
End;


Procedure TUixTreeView.SetGridExtensions(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toGridExtensions]
  Else
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toGridExtensions];
End;


Function TUixTreeView.GetShowEditable: Boolean;
Begin
  Result := toEditable In TreeOptions.MiscOptions;
End;


Procedure TUixTreeView.SetShowEditable(Const Value: Boolean);
Begin
  If Value Then
    TreeOptions.MiscOptions := TreeOptions.MiscOptions + [toEditable]
  Else
    TreeOptions.MiscOptions := TreeOptions.MiscOptions - [toEditable];
End;


Procedure TUixTreeView.DoCanEdit(Node: PVirtualNode; Column: TColumnIndex; Var Allowed: Boolean);
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnEditing) Then
    FOnEditing(Self, Get(Node), Column, Allowed);
End;


Procedure TUixTreeView.DoEdited(oSender : TBaseVirtualTree; Node : PVirtualNode; Column : TColumnIndex);
Begin
  If Not IsDestroying And Assigned(FOnEdited) Then
    FOnEdited(Self, Get(Node), Column);
End;


Procedure TUixTreeView.SortAscending;
Begin
  SortDirection := 1;
End;


Procedure TUixTreeView.SortDescending;
Begin
  SortDirection := -1;
End;


Function TUixTreeView.GetStateImages: TUixImages;
Begin
  Result := TUixImages(Inherited StateImages);
End;


Procedure TUixTreeView.SetStateImages(Const Value: TUixImages);
Begin
  Inherited StateImages := Value;
End;


Function TUixTreeViewColumns.HasSortColumn : Boolean;
Begin
  Result := SortColumn <> '';
End;


Procedure TUixTreeViewNode.CheckTypeButton;
Begin
  CheckType := htctButton;
End;


Procedure TUixTreeViewNode.CheckTypeCheckBox;
Begin
  CheckType := htctCheckBox;
End;


Procedure TUixTreeViewNode.CheckTypeNone;
Begin
  CheckType := htctNone;
End;


Procedure TUixTreeViewNode.CheckTypeRadioButton;
Begin
  CheckType := htctRadioButton;
End;


Procedure TUixTreeViewNode.CheckTypeTriState;
Begin
  CheckType := htctTriStateCheckBox;
End;


Function TUixTreeView.CanToggleNodes : Boolean;
Begin
  Result := ShowButtons Or ToggleDoubleClick;
End;


Function TUixTreeViewNode.GetEnabled : Boolean;
Begin
  Assert(checkCondition(FEnabledCheck = FEnabledText, 'GetEnabled', 'Mismatch in enabled states.'));

  Result := FEnabledCheck;
End;


Procedure TUixTreeViewNode.SetEnabled(Const Value : Boolean);
Begin
  FEnabledCheck := Value;
  FEnabledText := Value;
End;


Function TUixTreeViewNode.HasParent: Boolean;
Begin
  Result := Assigned(FParent);
End;


Function TUixTreeView.GetCentreScrollIntoView : Boolean;
Begin
  Result := toCenterScrollIntoView In TreeOptions.SelectionOptions;
End;


Procedure TUixTreeView.SetCentreScrollIntoView(Const Value : Boolean);
Begin
  If Value Then
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions + [toCenterScrollIntoView]
  Else
    TreeOptions.SelectionOptions := TreeOptions.SelectionOptions - [toCenterScrollIntoView];
End;


Procedure TUixTreeViewColumn.ApplyVisible(Const Value: Boolean);
Begin
  FVisible := Value;

  If Assigned(FColumn) Then
  Begin
    If FVisible Then
      FColumn.Options := FColumn.Options + [coVisible]
    Else
      FColumn.Options := FColumn.Options - [coVisible];
  End;
End;


Function TUixTreeView.ColumnClickIndex: TUixTreeViewColumnIndex;
Begin
  Result := Header.Columns.ClickIndex;
End;


Procedure TUixTreeView.SortChange(Const iColumnIndex: TUixTreeViewColumnIndex);
Begin
  Columns.SortColumnIndex := iColumnIndex;

  If Not IsDestroying And Assigned(FOnSortChange) Then
    FOnSortChange(Self, Columns[iColumnIndex]);
End;


Procedure TUixTreeView.DoAfterItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect);
Begin
  Inherited;

  If Not IsDestroying And Assigned(FOnAfterNodePaint) Then
    FOnAfterNodePaint(Self, Get(Node), Canvas, ItemRect);
End;


Function TUixTreeView.DoBeforeItemPaint(Canvas: TCanvas; Node: PVirtualNode; ItemRect: TRect): Boolean;
Begin
  Result := Inherited DoBeforeItemPaint(Canvas, Node, ItemRect);

  If Not IsDestroying And Assigned(FOnBeforeNodePaint) Then
    FOnBeforeNodePaint(Self, Get(Node), Canvas, ItemRect, Result);
End;


Procedure TUixTreeView.MultiSelectRefresh;
Begin
  Inherited;

  If Not IsUpdating And Assigned(FSelectIdentifierList) Then
    DoSelectionChange;
End;


Procedure TUixTreeView.SelectIdentifier(Const sID: TUixTreeViewIdentifier);
Begin
  SelectedID := sID;
  TopID := sID;

  Refresh;

  DoSelectionChange;
End;


Procedure TUixTreeView.SelectNone;
Begin
  SelectedID := '';
  TopID := '';

  Refresh;

  DoSelectionChange;
End;


Function TUixTreeView.IsDestroying : Boolean;
Begin
  Result := csDestroying In ComponentState;
End;


Procedure TUixTreeViewColumns.Unsortable;
Var
  iIndex : Integer;
Begin
  For iIndex := 0 To Count - 1 Do
    Columns[iIndex].Sortable := False;
End;


Procedure TUixTreeView.CollapseAll;
Begin
  Expands.Clear;
End;


Procedure TUixTreeView.ExpandAll;
Begin
  Expands.Clear;
  ExpandNode(FRoot);
End;


Procedure TUixTreeView.ExpandNode(oNode: TUixTreeViewNode);
Var
  iNodeIndex : Integer;
Begin
  Expands.Add(oNode.ID);

  For iNodeIndex := 0 To oNode.Children.Count - 1 Do
    ExpandNode(oNode.Children[iNodeIndex]);
End;


Procedure TUixTreeViewHeaderPopup.Initialise;
Begin
  Inherited;

  FInlineVisibility := True;
End;


Procedure TUixTreeViewHeaderPopup.Refresh;
Var
  iIndex : Integer;
  oMenuItem: TUixMenuItem;
  oColumn : TUixTreeViewColumn;
  oTreeView : TUixTreeView;
Begin
  If FInlineVisibility And (Owner Is TUixTreeView) And (TUixTreeView(Owner).Columns.Count > 0) Then
  Begin
    oTreeView := TUixTreeView(Owner);

    // Delete existing menu items.
    iIndex := Items.Count;
    While iIndex > 0 Do
    Begin
      Dec(iIndex);
      Items[iIndex].Free;
    End;

    For iIndex := 0 To oTreeView.Columns.Count - 1 Do
    Begin
      oColumn := oTreeView.Columns[iIndex];

      If oColumn.Enabled Then
      Begin
        oMenuItem := TUixMenuItem.Create(Self);

        oMenuItem.Tag := iIndex;
        oMenuItem.Caption := oColumn.Name;
        oMenuItem.Checked := oColumn.Visible;
        oMenuItem.OnClick := DoColumnVisbilityClick;

        Items.Add(oMenuItem);

        If (Items.Count Mod 24) = 0 Then
          oMenuItem.BreakBar;
      End;
    End;
  End;
End;


Procedure TUixTreeViewHeaderPopup.DoColumnVisbilityClick(Sender: TObject);
Var
  oColumn : TUixTreeViewColumn;
Begin
  If Assigned(PopupComponent) Then
  Begin
    oColumn := TUixTreeView(PopupComponent).Columns[TUixMenuItem(Sender).Tag];

    oColumn.ApplyVisible(Not oColumn.Visible);

    TUixMenuItem(Sender).Checked := oColumn.Visible;
  End;
End;


End.

