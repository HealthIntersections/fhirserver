Unit fui_vclx_Advanced;

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
  SysUtils, Classes, Controls, ExtCtrls, Forms, Graphics, Messages, StdCtrls, Windows, Dialogs, MultiMon,
  GdipApi, GdipObj,
  fsl_utilities, fsl_base, fsl_collections,
  fui_vclx_Base, fui_vclx_Controls,
  wp_gdiplus;


Type
  TUixAdvancedToolbar = Class;
  TUixAdvancedToolbarPresentationComponentEntity = Class;
  TUixAdvancedToolbarPresentationButtonEntity = Class;
  TUixAdvancedToolbarPresentationButtonEventPackage = Class;

  TUixAdvancedToolbarPresentationButtonClickHandler = TNotifyEvent;
  TUixAdvancedToolbarPresentationDropDownHandler = Procedure (oPackage : TUixAdvancedToolbarPresentationButtonEventPackage)Of Object;
  TUixAdvancedToolbarPresentationConfigurationClickHandler = TNotifyEvent;
  TUixAdvancedToolbarPresentationConfigurationDropDownHandler = Procedure (oPackage : TUixAdvancedToolbarPresentationButtonEventPackage) Of Object;
  TUixAdvancedToolbarPresentationButtonCustomIconHandler = Procedure(Const oGraphics : TGdiPlusExtendedGraphics; Const aRect : TGPRect; Const oButton : TUixAdvancedToolbarPresentationButtonEntity) Of Object;

  TUixAdvancedToolbarPresentationButtonEventPackage = Class(TFslObject)
    Private
      FToolbar : TUixAdvancedToolbar;
      FButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
      FComponentRectangle : TGPRect;

      Function GetButtonEntity: TUixAdvancedToolbarPresentationButtonEntity;
      Procedure SetButtonEntity(Const Value: TUixAdvancedToolbarPresentationButtonEntity);

      Function GetToolbar: TUixAdvancedToolbar;
      Procedure SetToolbar(Const Value: TUixAdvancedToolbar);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationButtonEventPackage;

      Property Toolbar : TUixAdvancedToolbar Read GetToolbar Write SetToolbar;
      Property ButtonEntity : TUixAdvancedToolbarPresentationButtonEntity Read GetButtonEntity Write SetButtonEntity;
      Property ComponentRectangle : TGPRect Read FComponentRectangle Write FComponentRectangle;
  End;

  TUixAdvancedToolbarPresentationButtonEntity = Class(TFslObject)
    Private
      FIsToggled : Boolean;
      FBitmapImage : TGdiPlusBitmapImage;
      FClickHandler : TUixAdvancedToolbarPresentationButtonClickHandler;
      FDropDownPopup : TUixPopupMenu;
      FDropDownHandler : TUixAdvancedToolbarPresentationDropDownHandler;
      FRightDropDownPopup : TUixPopupMenu;
      FShortCutKey : TUixKey;
      FCustomIconEvent : TUixAdvancedToolbarPresentationButtonCustomIconHandler;
      FContext : TFslObject;
      FCustomTextColour : TArgbColour;
      FBorderColour : TArgbColour;
      FShowBorder : Boolean;

      FParentComponent : TUixAdvancedToolbarPresentationComponentEntity;

      Function GetBitmapImage : TGdiPlusBitmapImage;
      Procedure SetBitmapImage(Const Value : TGdiPlusBitmapImage);

      Function GetHasBitmapImage : Boolean;
      Procedure SetHasBitmapImage(Const Value : Boolean);

      Function GetIsEnabled: Boolean;
      Procedure SetIsEnabled(Const Value: Boolean);

      Function GetIsVisible: Boolean;
      Procedure SetIsVisible(Const Value: Boolean);

      Function GetParentComponent: TUixAdvancedToolbarPresentationComponentEntity;
      Procedure SetParentComponent(Const Value: TUixAdvancedToolbarPresentationComponentEntity);

      Function GetHint: String;
      Procedure SetHint(Const Value: String);

      Function GetCaption: String;
      Procedure SetCaption(Const Value: String);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationButtonEntity;

      Function RequiresIcon : Boolean;

      Property ParentComponent : TUixAdvancedToolbarPresentationComponentEntity Read GetParentComponent Write SetParentComponent;
      Property IsEnabled : Boolean Read GetIsEnabled Write SetIsEnabled; // Requires ParentComponent to bet set
      Property IsVisible : Boolean Read GetIsVisible Write SetIsVisible; // Requires ParentComponent to bet set
      Property Hint : String Read GetHint Write SetHint; // Requires ParentComponent to bet set
      Property Caption : String Read GetCaption Write SetCaption; // Requires ParentComponent to bet set

      Property IsToggled : Boolean Read FIsToggled Write FIsToggled;
      Property HasBitmapImage : Boolean Read GetHasBitmapImage Write SetHasBitmapImage;
      Property BitmapImage : TGdiPlusBitmapImage Read GetBitmapImage Write SetBitmapImage;
      Property ClickHandler : TUixAdvancedToolbarPresentationButtonClickHandler Read FClickHandler Write FClickHandler;
      Property DropDownPopup : TUixPopupMenu Read FDropDownPopup Write FDropDownPopup;
      Property DropDownHandler : TUixAdvancedToolbarPresentationDropDownHandler Read FDropDownHandler Write FDropDownHandler;
      Property RightDropDownPopup : TUixPopupMenu Read FRightDropDownPopup Write FRightDropDownPopup;
      Property ShortCutKey : TUixKey Read FShortCutKey Write FShortCutKey;
      Property CustomIconEvent : TUixAdvancedToolbarPresentationButtonCustomIconHandler Read FCustomIconEvent Write FCustomIconEvent;
      Property Context : TFslObject Read FContext Write FContext;
      Property CustomTextColour : TArgbColour Read FCustomTextColour Write FCustomTextColour;
      Property BorderColour : TArgbColour Read FBorderColour Write FBorderColour;
      Property ShowBorder : Boolean Read FShowBorder Write FShowBorder;
  End;

  TUixAdvancedToolbarPresentationControlEntity = Class(TFslObject)
    Private
      FMinimumWidth : Integer;
      FMaximumWidth : Integer;
      FControl : TControl;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationControlEntity;

      Property MinimumWidth : Integer Read FMinimumWidth Write FMinimumWidth;
      Property MaximumWidth : Integer Read FMaximumWidth Write FMaximumWidth;
      Property Control : TControl Read FControl Write FControl;
  End;

  TUixAdvancedToolbarPresentationComponentEntity = Class(TFslObject)
    Private
      FCaption : String;
      FShowCaption : Boolean;
      FHint : String;
      FIsVisible : Boolean;
      FIsEnabled : Boolean;
      FPositionIndex : Integer;

      FChoice : TFslObjectChoice;

      Function GetButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
      Function GetHasButtonEntity : Boolean;
      Procedure SetHasButtonEntity(Const Value : Boolean);

      Function GetControlEntity : TUixAdvancedToolbarPresentationControlEntity;
      Function GetHasControlEntity : Boolean;
      Procedure SetHasControlEntity(Const Value : Boolean);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationComponentEntity;

      Property Caption : String Read FCaption Write FCaption;
      Property Hint : String Read FHint Write FHint;
      Property IsVisible : Boolean Read FIsVisible Write FIsVisible;
      Property IsEnabled : Boolean Read FIsEnabled Write FIsEnabled;
      Property PositionIndex : Integer Read FPositionIndex Write FPositionIndex;
      Property HasButtonEntity : Boolean Read GetHasButtonEntity Write SetHasButtonEntity;
      Property ButtonEntity : TUixAdvancedToolbarPresentationButtonEntity Read GetButtonEntity;
      Property HasControlEntity : Boolean Read GetHasControlEntity Write SetHasControlEntity;
      Property ControlEntity : TUixAdvancedToolbarPresentationControlEntity Read GetControlEntity;
      Property ShowCaption : Boolean Read FShowCaption Write FShowCaption;
  End;

  TUixAdvancedToolbarPresentationComponentEntityList = Class(TFslObjectList)
    Private
      Function GetComponentEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationComponentEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByCaption(pA, pB : Pointer) : Integer;
      Function CompareByPositionIndex(pA, pB : Pointer) : Integer;

    Public
      Procedure SortedByCaption;
      Procedure SortedByPositionIndex;

      Property ComponentEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarPresentationComponentEntity Read GetComponentEntityByIndex; Default;
  End;

  TUixAdvancedToolbarPresentationSectionEntity = Class(TFslObject)
    Private
      FComponentEntityList : TUixAdvancedToolbarPresentationComponentEntityList;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationSectionEntity;

      Function AddNewComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      Property ComponentEntityList : TUixAdvancedToolbarPresentationComponentEntityList Read FComponentEntityList;
  End;

  TUixAdvancedToolbarPresentationSectionEntityList = Class(TFslObjectList)
    Private
      Function GetSectionEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationSectionEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property SectionEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarPresentationSectionEntity Read GetSectionEntityByIndex; Default;
  End;

  TUixAdvancedToolbarPresentationGroupEntity = Class(TFslObject)
    Private
      FCaption : String;
      FSectionEntityList : TUixAdvancedToolbarPresentationSectionEntityList;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationGroupEntity;

      Function AddNewSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;

      Property Caption : String Read FCaption Write FCaption;
      Property SectionEntityList : TUixAdvancedToolbarPresentationSectionEntityList Read FSectionEntityList;
  End;

  TUixAdvancedToolbarPresentationGroupEntityList = Class(TFslObjectList)
    Private
      Function GetGroupEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationGroupEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property GroupEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarPresentationGroupEntity Read GetGroupEntityByIndex; Default;
  End;

  TUixAdvancedToolbarCaptionOrientation = (UixAdvancedToolbarCaptionOrientationRight, UixAdvancedToolbarCaptionOrientationBottom);

  TUixAdvancedToolbarPresentationSpecificationEntity = Class(TFslObject)
    Private
      FIconSize : Integer;
      FShowBorders : Boolean;
      FShowCaption : Boolean;
      FShowImages : Boolean;
      FAllowWrapping : Boolean;
      FCaptionOrientation : TUixAdvancedToolbarCaptionOrientation;
      FUseUniformButtonWidths : Boolean;
      FUseFuchsiaTransparency : Boolean;
      FButtonPadding : Integer;
      FShowConfiguration : Boolean;

    Public
      constructor Create; Override;

      Property IconSize : Integer Read FIconSize Write FIconSize;
      Property ShowBorders : Boolean Read FShowBorders Write FShowBorders;
      Property ShowCaption : Boolean Read FShowCaption Write FShowCaption;
      Property ShowImages : Boolean Read FShowImages Write FShowImages;
      Property AllowWrapping : Boolean Read FAllowWrapping Write FAllowWrapping;
      Property CaptionOrientation : TUixAdvancedToolbarCaptionOrientation Read FCaptionOrientation Write FCaptionOrientation;
      Property UseUniformButtonWidths : Boolean Read FUseUniformButtonWidths Write FUseUniformButtonWidths;
      Property UseFuchsiaTransparency : Boolean Read FUseFuchsiaTransparency Write FUseFuchsiaTransparency;
      Property ButtonPadding : Integer Read FButtonPadding Write FButtonPadding;
      Property ShowConfiguration : Boolean Read FShowConfiguration Write FShowConfiguration;
  End;

  TUixAdvancedToolbarPresentationEntity = Class(TFslObject)
    Private
      FMarginLeft : Integer;
      FMarginRight : Integer;
      FMarginTop : Integer;
      FMarginBottom : Integer;
      FGroupEntityList : TUixAdvancedToolbarPresentationGroupEntityList;
      FSpecificationEntity : TUixAdvancedToolbarPresentationSpecificationEntity;
      FClickHandler : TUixAdvancedToolbarPresentationConfigurationClickHandler;
      FDropDownPopup : TUixPopupMenu;
      FDropDownHandler : TUixAdvancedToolbarPresentationConfigurationDropDownHandler;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarPresentationEntity;

      Function AddNewGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;

      Property MarginLeft : Integer Read FMarginLeft Write FMarginLeft;
      Property MarginRight : Integer Read FMarginRight Write FMarginRight;
      Property MarginTop : Integer Read FMarginTop Write FMarginTop;
      Property MarginBottom : Integer Read FMarginBottom Write FMarginBottom;
      Property GroupEntityList : TUixAdvancedToolbarPresentationGroupEntityList Read FGroupEntityList;
      Property SpecificationEntity : TUixAdvancedToolbarPresentationSpecificationEntity Read FSpecificationEntity;
      Property ClickHandler : TUixAdvancedToolbarPresentationConfigurationClickHandler Read FClickHandler Write FClickHandler;
      Property DropDownPopup : TUixPopupMenu Read FDropDownPopup Write FDropDownPopup;
      Property DropDownHandler : TUixAdvancedToolbarPresentationConfigurationDropDownHandler Read FDropDownHandler Write FDropDownHandler;

  End;

  TUixAdvancedToolbarExecutionButtonEntity = Class(TFslObject)
    Private
      FIsFirstInSection : Boolean;
      FIsLastInSection : Boolean;

      FBorderRectangle : TGPRect;
      FBackgroundRectangle : TGPRect;

      FButtonClickRectangle : TGPRect;
      FButtonImageRectangle : TGPRect;
      FButtonCaptionRectangle : TGPRect;

      FDropDownArrowRectangle : TGPRect;
      FDropDownBorderRectangle : TGPRect;
      FDropDownBackgroundRectangle : TGPRect;

      FRequiredWidth : Integer;

      FPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;

      Function GetPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
      Procedure SetPresentationButtonEntity(Const Value : TUixAdvancedToolbarPresentationButtonEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property IsFirstInSection : Boolean Read FIsFirstInSection Write FIsFirstInSection;
      Property IsLastInSection : Boolean Read FIsLastInSection Write FIsLastInSection;

      Property BorderRectangle : TGPRect Read FBorderRectangle Write FBorderRectangle;
      Property BackgroundRectangle : TGPRect Read FBackgroundRectangle Write FBackgroundRectangle;

      Property ButtonClickRectangle : TGPRect Read FButtonClickRectangle Write FButtonClickRectangle;
      Property ButtonImageRectangle : TGPRect Read FButtonImageRectangle Write FButtonImageRectangle;
      Property ButtonCaptionRectangle : TGPRect Read FButtonCaptionRectangle Write FButtonCaptionRectangle;

      Property DropDownArrowRectangle : TGPRect Read FDropDownArrowRectangle Write FDropDownArrowRectangle;
      Property DropDownBorderRectangle : TGPRect Read FDropDownBorderRectangle Write FDropDownBorderRectangle;
      Property DropDownBackgroundRectangle : TGPRect Read FDropDownBackgroundRectangle Write FDropDownBackgroundRectangle;

      Property RequiredWidth : Integer Read FRequiredWidth Write FRequiredWidth;

      Property PresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity Read GetPresentationButtonEntity Write SetPresentationButtonEntity;
  End;

  TUixAdvancedToolbarExecutionControlEntity = Class(TFslObject)
    Private
      FEditControlRectangle : TGPRect;
      FBorderWidth : Integer;
      FBorderRectangle : TGPRect;
      FDropDownArrowRectangle : TGPRect;
      FDropDownBorderRectangle : TGPRect;
      FDropDownBackgroundRectangle : TGPRect;
      FPresentationControlEntity : TUixAdvancedToolbarPresentationControlEntity;

      Function GetPresentationControlEntity : TUixAdvancedToolbarPresentationControlEntity;
      Procedure SetPresentationControlEntity(Const Value : TUixAdvancedToolbarPresentationControlEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property EditControlRectangle : TGPRect Read FEditControlRectangle Write FEditControlRectangle;
      Property BorderWidth : Integer Read FBorderWidth Write FBorderWidth;
      Property BorderRectangle : TGPRect Read FBorderRectangle Write FBorderRectangle;
      Property DropDownBorderRectangle : TGPRect Read FDropDownBorderRectangle Write FDropDownBorderRectangle;
      Property DropDownArrowRectangle : TGPRect Read FDropDownArrowRectangle Write FDropDownArrowRectangle;
      Property DropDownBackgroundRectangle : TGPRect Read FDropDownBackgroundRectangle Write FDropDownBackgroundRectangle;
      Property PresentationControlEntity : TUixAdvancedToolbarPresentationControlEntity Read GetPresentationControlEntity Write SetPresentationControlEntity;
  End;

  TUixAdvancedToolbarExecutionComponentEntity = Class(TFslObject)
    Private
      FChoice : TFslObjectChoice;

      FPresentationComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;

      Function GetButtonEntity : TUixAdvancedToolbarExecutionButtonEntity;
      Function GetHasButtonEntity : Boolean;
      Procedure SetHasButtonEntity(Const Value : Boolean);

      Function GetControlEntity : TUixAdvancedToolbarExecutionControlEntity;
      Function GetHasControlEntity : Boolean;
      Procedure SetHasControlEntity(Const Value : Boolean);

      Function GetPresentationComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
      Procedure SetPresentationComponentEntity(Const Value : TUixAdvancedToolbarPresentationComponentEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarExecutionComponentEntity;

      Property HasButtonEntity : Boolean Read GetHasButtonEntity Write SetHasButtonEntity;
      Property ButtonEntity : TUixAdvancedToolbarExecutionButtonEntity Read GetButtonEntity;
      Property HasControlEntity : Boolean Read GetHasControlEntity Write SetHasControlEntity;
      Property ControlEntity : TUixAdvancedToolbarExecutionControlEntity Read GetControlEntity;
      Property PresentationComponentEntity : TUixAdvancedToolbarPresentationComponentEntity Read GetPresentationComponentEntity Write SetPresentationComponentEntity;
  End;

  TUixAdvancedToolbarExecutionComponentEntityList = Class(TFslObjectList)
    Private
      Function GetComponentEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionComponentEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByCaption(pA, pB : Pointer) : Integer;
      Function CompareByPositionIndex(pA, pB : Pointer) : Integer;

    Public
      Procedure SortedByCaption;
      Procedure SortedByPositionIndex;

      Property ComponentEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarExecutionComponentEntity Read GetComponentEntityByIndex; Default;
  End;

  TUixAdvancedToolbarExecutionSectionEntity = Class(TFslObject)
    Private
      FContainsVisibleComponents : Boolean;

      FComponentEntityList : TUixAdvancedToolbarExecutionComponentEntityList;
      FPresentationSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;

      Function GetPresentationSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;
      Procedure SetPresentationSectionEntity(Const Value : TUixAdvancedToolbarPresentationSectionEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property ContainsVisibleComponents : Boolean Read FContainsVisibleComponents Write FContainsVisibleComponents;
      Property ComponentEntityList : TUixAdvancedToolbarExecutionComponentEntityList Read FComponentEntityList;
      Property PresentationSectionEntity : TUixAdvancedToolbarPresentationSectionEntity Read GetPresentationSectionEntity Write SetPresentationSectionEntity;
  End;

  TUixAdvancedToolbarExecutionSectionEntityList = Class(TFslObjectList)
    Private
      Function GetSectionEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionSectionEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property SectionEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarExecutionSectionEntity Read GetSectionEntityByIndex; Default;
  End;

  TUixAdvancedToolbarExecutionGroupEntity = Class(TFslObject)
    Private
      FHasSeparator : Boolean;
      FSeparatorX : Integer;
      FSeparatorY : Integer;

      FCaptionTextRectangle : TGPRect;
      FCaptionBorderRectangle : TGPRect;
      FCaptionBackgroundRectangle : TGPRect;
      FCaptionDropDownArrowRectangle : TGPRect;

      FCollapsedToCaption : Boolean;
      FHiddenWithinChevron : Boolean;
      FContainsVisibleComponents : Boolean;

      FRequiredExpandedWidth : Integer;
      FRequiredCollapsedWidth : Integer;

      FSectionEntityList : TUixAdvancedToolbarExecutionSectionEntityList;
      FPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;

      Function GetPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
      Procedure SetPresentationGroupEntity(Const Value : TUixAdvancedToolbarPresentationGroupEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarExecutionGroupEntity;

      Property HasSeparator : Boolean Read FHasSeparator Write FHasSeparator;
      Property SeparatorX : Integer Read FSeparatorX Write FSeparatorX;
      Property SeparatorY : Integer Read FSeparatorY Write FSeparatorY;

      Property CaptionTextRectangle : TGPRect Read FCaptionTextRectangle Write FCaptionTextRectangle;
      Property CaptionBorderRectangle : TGPRect Read FCaptionBorderRectangle Write FCaptionBorderRectangle;
      Property CaptionBackgroundRectangle : TGPRect Read FCaptionBackgroundRectangle Write FCaptionBackgroundRectangle;
      Property CaptionDropDownArrowRectangle : TGPRect Read FCaptionDropDownArrowRectangle Write FCaptionDropDownArrowRectangle;

      Property CollapsedToCaption : Boolean Read FCollapsedToCaption Write FCollapsedToCaption;
      Property HiddenWithinChevron : Boolean Read FHiddenWithinChevron Write FHiddenWithinChevron;
      Property ContainsVisibleComponents : Boolean Read FContainsVisibleComponents Write FContainsVisibleComponents;

      Property RequiredExpandedWidth : Integer Read FRequiredExpandedWidth Write FRequiredExpandedWidth;
      Property RequiredCollapsedWidth : Integer Read FRequiredCollapsedWidth Write FRequiredCollapsedWidth;

      Property SectionEntityList : TUixAdvancedToolbarExecutionSectionEntityList Read FSectionEntityList;
      Property PresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity Read GetPresentationGroupEntity Write SetPresentationGroupEntity;
  End;

  TUixAdvancedToolbarExecutionGroupEntityList = Class(TFslObjectList)
    Private
      Function GetGroupEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionGroupEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByPresentationGroupEntity(pA, pB : Pointer) : Integer;

    Public
      Property GroupEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarExecutionGroupEntity Read GetGroupEntityByIndex; Default;

      Function IndexByPresentationGroupEntity(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity) : Integer;
      Function ExistsByPresentationGroupEntity(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity) : Boolean;
  End;

  TUixAdvancedToolbarExecutionEntity = Class(TFslObject)
    Private
      FGroupEntityList : TUixAdvancedToolbarExecutionGroupEntityList;
      FChevronIsVisible : Boolean;

      FHasConfiguration : Boolean;
      FConfigurationRectangle : TGPRect;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property GroupEntityList : TUixAdvancedToolbarExecutionGroupEntityList Read FGroupEntityList;
      Property ChevronIsVisible : Boolean Read FChevronIsVisible Write FChevronIsVisible;

      Property HasConfiguration : Boolean Read FHasConfiguration Write FHasConfiguration;
      Property ConfigurationRectangle : TGPRect Read FConfigurationRectangle Write FConfigurationRectangle;
  End;

  TUixAdvancedToolbarHotSpotActionStyle = (
    UixAdvancedToolbarHotSpotActionStyleConfiguration,
    UixAdvancedToolbarHotSpotActionStyleGroup,
    UixAdvancedToolbarHotSpotActionStyleButton,
    UixAdvancedToolbarHotSpotActionStyleButtonDropDown,
    UixAdvancedToolbarHotSpotActionStyleChevron
  );

  TUixAdvancedToolbarHotSpotEntity = Class(TFslObject)
    Private
      FHotSpot : TGdiPlusHotSpot;
      FActionStyle : TUixAdvancedToolbarHotSpotActionStyle;
      FGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
      FComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;

      Function GetGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
      Procedure SetGroupEntity(Const Value : TUixAdvancedToolbarExecutionGroupEntity);

      Function GetComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
      Procedure SetComponentEntity(Const Value : TUixAdvancedToolbarExecutionComponentEntity);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedToolbarHotSpotEntity;

      Function HasGroupEntity : Boolean;
      Function HasComponentEntity : Boolean;

      Function IsEquivalent(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity) : Boolean;

      Function IsGroupEntity(Const oGroupEntity : TUixAdvancedToolbarExecutionGroupEntity) : Boolean;
      Function IsComponentEntity(Const oComponentEntity : TUixAdvancedToolbarExecutionComponentEntity) : Boolean;

      Property HotSpot : TGdiPlusHotSpot Read FHotSpot;
      Property ActionStyle : TUixAdvancedToolbarHotSpotActionStyle Read FActionStyle Write FActionStyle;
      Property GroupEntity : TUixAdvancedToolbarExecutionGroupEntity Read GetGroupEntity Write SetGroupEntity;
      Property ComponentEntity : TUixAdvancedToolbarExecutionComponentEntity Read GetComponentEntity Write SetComponentEntity;
  End;

  TUixAdvancedToolbarHotSpotEntityList = Class(TFslObjectList)
    Private
      Function GetHotSpotEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarHotSpotEntity;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property HotSpotEntityByIndex[Const iIndex : Integer] : TUixAdvancedToolbarHotSpotEntity Read GetHotSpotEntityByIndex; Default;
  End;

  TUixAdvancedToolbarGroupForm = Class(TCustomForm)
    Private
      FToolBar : TUixAdvancedToolbar;
      FLabel : TUixLabel;

      Procedure PaintHandler(oSender : TObject);

      Procedure WMPrint(Var aMessage : TMessage); Message WM_PRINT;
      Procedure CMDeactivate(Var Message: TCMDeactivate); Message CM_DEACTIVATE;

    Protected
      Procedure CreateParams(Var aParams : TCreateParams); Override;

      Procedure AdjustClientRect(Var Rect: TRect); Override;

    Public
      constructor CreateNew(oOwner : TComponent; iDummy : Integer = 0); Override;
      destructor Destroy; Override;

      Procedure AnimateShow;
      Procedure AnimateClose;

      Property ToolBar : TUixAdvancedToolbar Read FToolBar;
  End;

  TUixAdvancedToolbarChevronForm = Class(TCustomForm)
    Private
      FRequiredWidth : Integer;
      FRequiredHeight : Integer;

      FPresentationEntity : TUixAdvancedToolbarPresentationEntity;

      Procedure PaintHandler(oSender : TObject);

      Procedure WMPrint(Var aMessage : TMessage); Message WM_PRINT;
      Procedure CMDeactivate(Var Message: TCMDeactivate); Message CM_DEACTIVATE;

    Protected
      Procedure CreateParams(Var aParams : TCreateParams); Override;

      Procedure AdjustClientRect(Var Rect: TRect); Override;

    Public
      constructor CreateNew(oOwner : TComponent; iDummy : Integer = 0); Override;
      destructor Destroy; Override;

      Procedure AnimateShow;
      Procedure AnimateClose;

      Property RequiredWidth : Integer Read FRequiredWidth Write FRequiredWidth;
      Property RequiredHeight : Integer Read FRequiredHeight Write FRequiredHeight;
      Property PresentationEntity : TUixAdvancedToolbarPresentationEntity Read FPresentationEntity;
  End;

  TUixAdvancedToolbar = Class(TGdiPlusCustomControl)
    Private
      FPresentationEntity : TUixAdvancedToolbarPresentationEntity;
      FExecutionEntity : TUixAdvancedToolbarExecutionEntity;

      FPen : TGPPen;
      FSolidBrush : TGPSolidBrush;
      FChevronPath : TGPGraphicsPath;
      FDefaultImageAttributes : TGPImageAttributes;
      FGrayScaleImageAttributes : TGPImageAttributes;

      FCaptionFont : TGdiPlusFont;
      FCaptionStringFormat : TGdiPlusStringFormat;

      FClientRectangle : TGPRect;

      FHotSpotEntityList : TUixAdvancedToolbarHotSpotEntityList;
      FHotSpotMouseDownEntity : TUixAdvancedToolbarHotSpotEntity;
      FHotSpotMouseHoverEntity : TUixAdvancedToolbarHotSpotEntity;
      FConfigurationHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;

      FVisibleControlGroupEntityList : TUixAdvancedToolbarExecutionGroupEntityList;
      FVisibleControlComponentEntityList : TUixAdvancedToolbarExecutionComponentEntityList;

      FRequiredRowCount : Integer;

      FPresentationExecutionButtonEntityMatch : TFslObjectMatch;

      Function GetPresentationEntity : TUixAdvancedToolbarPresentationEntity;
      Procedure SetPresentationEntity(Const Value : TUixAdvancedToolbarPresentationEntity);

      Procedure CalculateMetrics(Const oGraphics : TGdiPlusExtendedGraphics);

      Function RowHeight : Integer;

      Procedure AdjustHeight;

      Function AddNewHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
      Function DetermineHotSpotEntityAtPoint(Const iX, iY : Integer) : TUixAdvancedToolbarHotSpotEntity;

      Procedure ApplyMouseDownHotSpot(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity);
      Procedure ApplyMouseHoverHotSpot(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity);

      Procedure CMMouseLeave(Var aMessage : TMessage); Message CM_MOUSELEAVE;
      Procedure CMHintShow(Var aMessage: TCMHintShow); Message CM_HINTSHOW;

    Protected
      Procedure CreateParams(Var aParams : TCreateParams); Override;

      Procedure Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect); Override;

      Procedure MouseMove(aShiftState : TShiftState; iX, iY : Integer); Override;
      Procedure MouseDown(aButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer); Override;
      Procedure MouseUp(aButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer); Override;

      Procedure Resize; Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure Execute;

      Procedure Refresh;

      Function ConfigurationHotspotContainsPoint(Const iX, iY : Integer) : Boolean;

      Function ActivateShortCutKey(Const aUixKey : TUixKey) : Boolean;

      Function RecommendedHeight : Integer;
      Function RecommendedWidth : Integer;

      Procedure ClickButtonEntity(Const oButtonEntity : TUixAdvancedToolbarPresentationButtonEntity);

      Procedure SortByCaption(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity);
      Procedure SortByPositionIndex(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity);

      Property PopupMenu;
      Property PresentationEntity : TUixAdvancedToolbarPresentationEntity Read GetPresentationEntity Write SetPresentationEntity;
  End;

Type
  TUixAdvancedComboBox = Class(TUixComboBox)
    Private
      FPen : TGPPen;
      FSolidBrush : TGPSolidBrush;

      FDefaultFont : TGPFont;
      FDefaultStringFormat : TGdiPlusStringFormat;

      FButtonRect : TGPRect;
      FButtonHovering : Boolean;
      FButtonDown : Boolean;

      FControlHovering : Boolean;

      FDropDownWidth : Integer;
      FOverrideDropDownWidth : Boolean;

      FSuppressBackground : Boolean;
      FRoundedBorders : Boolean;

      Procedure PaintCustomComboBoxToDC(Const pInputDC : HDC; Const aClippingRectangle : TRect);

      Procedure WMPaint(Var aMessage: TWMPaint); Message WM_PAINT;
      Procedure WMEraseBkgnd(Var aMessage: TWMEraseBkgnd); Message WM_ERASEBKGND;

      Procedure CNCommand(Var aMessage: TWMCommand); Message CN_COMMAND;
      Procedure CNDrawItem(Var aMessage: TWMDrawItem); Message CN_DRAWITEM;
      Procedure CNMeasureItem(Var aMessage: TWMMeasureItem); Message CN_MEASUREITEM;

      Procedure CMMouseLeave(Var aMessage: TMessage); Message CM_MOUSELEAVE;
      Procedure CMMouseEnter(Var aMessage: TMessage); Message CM_MOUSEENTER;

      Function GetValue: Integer;
      Procedure SetValue(Const Value: Integer);

    Protected
      Procedure WndProc(Var aMessage: TMessage); Override;

      Procedure Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const aComboBoxInfo : TComboBoxInfo); Virtual;

      Procedure DrawComboBoxItem(Const oGraphics : TGdiPlusExtendedGraphics; Const iIndex : Integer; Const aItemRectangle : TGPRect; Const aItemState : TOwnerDrawState); Virtual;

      Procedure MouseMove(aShiftState : TShiftState; iX, iY: Integer); Override;

      Property Pen : TGPPen Read FPen;
      Property SolidBrush : TGPSolidBrush Read FSolidBrush;
      Property SuppressBackground : Boolean Read FSuppressBackground Write FSuppressBackground;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure MouseWheelHandler(Var aMessage: TMessage); Override;

      Procedure StyleDropDownEdit;
      Procedure StyleDropDownList;

      Procedure AddValueArray(Const aValues : Array Of String); Overload;

      Function ValueByIndex(Const iIndex : Integer) : String;
      Function ExistsByValue(Const sValue : String) : Boolean;
      Function ExistsByIndex(Const iIndex: Integer): Boolean;

      Property DropDownWidth : Integer Read FDropDownWidth Write FDropDownWidth;
      Property OverrideDropDownWidth : Boolean Read FOverrideDropDownWidth Write FOverrideDropDownWidth;
      Property RoundedBorders : Boolean Read FRoundedBorders Write FRoundedBorders;
      Property Value : Integer Read GetValue Write SetValue;
  End;


Type
  TUixAdvancedFontComboBoxFontNameHashEntry = Class(TFslHashEntry)
    Private
      FFontName : String;
      FGdiFont : TFont;
      FGdiPlusFont : TGPFont;

      Procedure SetFontName(Const Value: String);

    Protected
      Procedure Generate; Override;

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Property FontName : String Read FFontName Write SetFontName;
      Property GdiFont : TFont Read FGdiFont Write FGdiFont;
      Property GdiPlusFont : TGPFont Read FGdiPlusFont Write FGdiPlusFont;
  End;

  TUixAdvancedFontComboBoxFontNameHashTable = Class(TFslHashTable)
    Protected
      Function ItemClass : TFslHashEntryClass; Override;

      Function Equal(oA, oB : TFslHashEntry) : Integer; Override;
  End;

  TUixAdvancedFontComboBox = Class(TUixAdvancedComboBox)
    Private
      FDefaultFont : TGPFont;
      FDefaultStringFormat : TGdiPlusStringFormat;

      FMeasureGraphics : TGdiPlusExtendedGraphics;

      FEditControlInstance: Pointer;
      FEditControlHandle : HWND;
      FDefaultEditControlProc: Pointer;

      FLookupHashEntry : TUixAdvancedFontComboBoxFontNameHashEntry;
      FHashTable : TUixAdvancedFontComboBoxFontNameHashTable;
      FCachedDC : HDC;

      FAllFonts : TFslStringList;
      FSelectedFonts : TFslStringList;

      Function RetrieveFontByName(sFontName : String) : TUixAdvancedFontComboBoxFontNameHashEntry;

      Procedure EditControlWndProc(Var Message: TMessage);

      Procedure BuildItems;
    Protected
      Procedure CreateParams(Var Params: TCreateParams); Override;

      Procedure DrawComboBoxItem(Const oGraphics : TGdiPlusExtendedGraphics; Const iIndex : Integer; Const aItemRectangle : TGPRect; Const aItemState : TOwnerDrawState); Override;

      Procedure Change; Override;
    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure CreateWnd; Override;

      Procedure Cache;
  End;

  TUixAdvancedColourComboBox = Class(TUixAdvancedComboBox)
    Private
      FDefaultFont : TGPFont;
      FCustomFont : TGdiPlusFont;
      FDefaultStringFormat : TGdiPlusStringFormat;
      FSelectedColours : TFslIntegerList;
      FLastCustom : Integer;

      FMeasureGraphics : TGdiPlusExtendedGraphics;

      Function ItemValueToColour(sText : String) : TColour;

      Function GetValue: TColour;
      Procedure SetValue(Const Value: TColour);
      Procedure BuildItems;
    Protected
      Procedure CreateHandle; Override;

      Procedure Change; Override;

      Procedure DrawComboBoxItem(Const oGraphics : TGdiPlusExtendedGraphics; Const iIndex : Integer; Const aItemRectangle : TGPRect; Const aItemState : TOwnerDrawState); Override;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Property Value : TColour Read GetValue Write SetValue;
  End;



Type
  TUixAdvancedButtonHorizontalTextAlignmentType = (UixAdvancedButtonHorizontalTextAlignmentTypeLeft, UixAdvancedButtonHorizontalTextAlignmentTypeCenter, UixAdvancedButtonHorizontalTextAlignmentTypeRight);

  TUixAdvancedButtonPresentationEntity = Class(TFslObject)
    Private
      FCaption : String;
      FShowCaption : Boolean;
      FShowDropDownChevronBelow : Boolean;

      FIconBitmapImage : TGdiPlusBitmapImage;

      FTextColour : TArgbColour;

      FBackgroundColourTop : TArgbColour;
      FBackgroundColourBottom : TArgbColour;

      FDisabledBackgroundColourTop : TArgbColour;
      FDisabledBackgroundColourBottom : TArgbColour;

      FFocusedBackgroundColourTop : TArgbColour;
      FFocusedBackgroundColourBottom : TArgbColour;

      FHoverBackgroundColourTop : TArgbColour;
      FHoverBackgroundColourBottom : TArgbColour;

      FBorderColour : TArgbColour;
      FHoverBorderColour : TArgbColour;

      FFontSize : Double;
      FFontFamily : String;

      FMarginLeft : Integer;
      FMarginRight : Integer;
      FMarginTop : Integer;
      FMarginBottom : Integer;

      FHorizontalTextAlignment : TUixAdvancedButtonHorizontalTextAlignmentType;

      FFullButtonPopup : Boolean;

      Function GetHasIconBitmapImage : Boolean;
      Procedure SetHasIconBitmapImage(Const Value : Boolean);

      Function GetIconBitmapImage : TGdiPlusBitmapImage;
      Procedure SetIconBitmapImage(Const Value : TGdiPlusBitmapImage);

      Function GetMargin : Integer;
      Procedure SetMargin(Const Value : Integer);

    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TUixAdvancedButtonPresentationEntity;

      Property Caption : String Read FCaption Write FCaption;
      Property ShowCaption : Boolean Read FShowCaption Write FShowCaption;
      Property ShowDropDownChevronBelow : Boolean Read FShowDropDownChevronBelow Write FShowDropDownChevronBelow;
      Property HasIconBitmapImage : Boolean Read GetHasIconBitmapImage Write SetHasIconBitmapImage;
      Property IconBitmapImage : TGdiPlusBitmapImage Read GetIconBitmapImage Write SetIconBitmapImage;
      Property BackgroundColourTop : TArgbColour Read FBackgroundColourTop Write FBackgroundColourTop;
      Property BackgroundColourBottom : TArgbColour Read FBackgroundColourBottom Write FBackgroundColourBottom;
      Property DisabledBackgroundColourTop : TArgbColour Read FDisabledBackgroundColourTop Write FDisabledBackgroundColourTop;
      Property DisabledBackgroundColourBottom : TArgbColour Read FDisabledBackgroundColourBottom Write FDisabledBackgroundColourBottom;
      Property FocusedBackgroundColourTop : TArgbColour Read FFocusedBackgroundColourTop Write FFocusedBackgroundColourTop;
      Property FocusedBackgroundColourBottom : TArgbColour Read FFocusedBackgroundColourBottom Write FFocusedBackgroundColourBottom;
      Property HoverBackgroundColourTop : TArgbColour Read FHoverBackgroundColourTop Write FHoverBackgroundColourTop;
      Property HoverBackgroundColourBottom : TArgbColour Read FHoverBackgroundColourBottom Write FHoverBackgroundColourBottom;
      Property TextColour : TArgbColour Read FTextColour Write FTextColour;
      Property BorderColour : TArgbColour Read FBorderColour Write FBorderColour;
      Property HoverBorderColour : TArgbColour Read FHoverBorderColour Write FHoverBorderColour;
      Property FontSize : Double Read FFontSize Write FFontSize;
      Property FontFamily : String Read FFontFamily Write FFontFamily;
      Property HorizontalTextAlignment : TUixAdvancedButtonHorizontalTextAlignmentType Read FHorizontalTextAlignment Write FHorizontalTextAlignment;
      Property FullButtonPopup : Boolean Read FFullButtonPopup Write FFullButtonPopup;
      Property Margin : Integer Read GetMargin Write SetMargin;
      Property MarginLeft : Integer Read FMarginLeft Write FMarginLeft;
      Property MarginRight : Integer Read FMarginRight Write FMarginRight;
      Property MarginTop : Integer Read FMarginTop Write FMarginTop;
      Property MarginBottom : Integer Read FMarginBottom Write FMarginBottom;
  End;

  TUixAdvancedButtonShapeStyle = (UixAdvancedButtonShapeStyleRounded, UixAdvancedButtonShapeStyleSquare);

  TUixAdvancedButton = Class(TGdiPlusCustomControl)
    Private
      FPresentationEntity : TUixAdvancedButtonPresentationEntity;

      FImageAttributes : TGPImageAttributes;

      FDropDownPopup : TUixPopupMenu;

      FPen : TGPPen;
      FSolidBrush : TGPSolidBrush;

      FDefaultFont : TGdiPlusFont;
      FDefaultStringFormat : TGdiPlusStringFormat;

      FClientPath : TGPGraphicsPath;
      FClientRectangle : TGPRect;

      FMouseHover : Boolean;
      FLeftMouseDown : Boolean;
      FDropDownActivated : Boolean;
      FDropDownRectangle : TGPRect;
      FChecked : Boolean;
      FShapeStyle : TUixAdvancedButtonShapeStyle;

      FOnClick : TNotifyEvent;

      Procedure PopupMenuClose(oSender : TObject);

      Procedure WMKillFocus(Var aMessage : TMessage); Message WM_KILLFOCUS;
      Procedure WMSetFocus(Var aMessage : TMessage); Message WM_SETFOCUS;

      Procedure CMHintShow(Var aMessage: TCMHintShow); Message CM_HINTSHOW;
      Procedure CMMouseLeave(Var aMessage : TMessage); Message CM_MOUSELEAVE;

    Protected
      Procedure Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect); Override;

      Procedure MouseUp(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer); Override;
      Procedure MouseDown(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer); Override;
      Procedure MouseMove(aShift : TShiftState; iX, iY : Integer); Override;

      Procedure KeyDown(Var iKey : Word; aShift : TShiftState); Override;
      Procedure KeyUp(Var iKey : Word; aShift : TShiftState); Override;

      Function HasPopupMenuItems : Boolean;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Function RecommendedHeight : Integer;
      Function RecommendedWidth : Integer;

      Procedure Prepare;

      Procedure ShapeStyleSquare;
      Procedure ShapeStyleRounded;

      Procedure FireClick;

      Property OnClick : TNotifyEvent Read FOnClick Write FOnClick;
      Property PresentationEntity : TUixAdvancedButtonPresentationEntity Read FPresentationEntity;
      Property DropDownPopup : TUixPopupMenu Read FDropDownPopup Write FDropDownPopup;
      Property ShapeStyle : TUixAdvancedButtonShapeStyle Read FShapeStyle Write FShapeStyle;
      Property Checked : Boolean Read FChecked Write FChecked;
  End;

Implementation


Constructor TUixAdvancedToolbarPresentationButtonEntity.Create;
Begin
  Inherited;

  FParentComponent := Nil;

  FBitmapImage := Nil;
  FShortCutKey := kk_NULL;
  FContext := Nil;
  FCustomTextColour := 0;
  FBorderColour := argbDarkGray;
End;


Destructor TUixAdvancedToolbarPresentationButtonEntity.Destroy;
Begin
  FContext := Nil;
  FBitmapImage.Free;
  FParentComponent := Nil;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.Link : TUixAdvancedToolbarPresentationButtonEntity;
Begin
  Result := TUixAdvancedToolbarPresentationButtonEntity(Inherited Link);
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetBitmapImage : TGdiPlusBitmapImage;
Begin
  Assert(Invariants('GetBitmapImage', FBitmapImage, TGdiPlusBitmapImage, 'FBitmapImage'));

  Result := FBitmapImage;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetBitmapImage(Const Value : TGdiPlusBitmapImage);
Begin
  Assert(Invariants('SetBitmapImage', Value, TGdiPlusBitmapImage, 'Value'));

  FBitmapImage.Free;
  FBitmapImage := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetHasBitmapImage : Boolean;
Begin
  Result := Assigned(FBitmapImage);
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetHasBitmapImage(Const Value : Boolean);
Begin
  If Assigned(FBitmapImage) And Not Value Then
  Begin
    FBitmapImage.Free;
    FBitmapImage := Nil;
  End
  Else If Not Assigned(FBitmapImage) And Value Then
  Begin
    FBitmapImage := TGdiPlusBitmapImage.Create;
  End;
End;


Constructor TUixAdvancedToolbarPresentationControlEntity.Create;
Begin
  Inherited;

End;


Destructor TUixAdvancedToolbarPresentationControlEntity.Destroy;
Begin

  Inherited;
End;


Function TUixAdvancedToolbarPresentationControlEntity.Link : TUixAdvancedToolbarPresentationControlEntity;
Begin
  Result := TUixAdvancedToolbarPresentationControlEntity(Inherited Link);
End;


Constructor TUixAdvancedToolbarPresentationComponentEntity.Create;
Begin
  Inherited;

  FIsVisible := True;
  FIsEnabled := True;

  FShowCaption := True;

  FPositionIndex := -1;

  FChoice := TFslObjectChoice.Create;
End;


Destructor TUixAdvancedToolbarPresentationComponentEntity.Destroy;
Begin
  FChoice.Free;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationComponentEntity.Link : TUixAdvancedToolbarPresentationComponentEntity;
Begin
  Result := TUixAdvancedToolbarPresentationComponentEntity(Inherited Link);
End;


Function TUixAdvancedToolbarPresentationComponentEntity.GetButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
Begin
  Result := TUixAdvancedToolbarPresentationButtonEntity(FChoice.RetrieveObject(TUixAdvancedToolbarPresentationButtonEntity));
End;


Function TUixAdvancedToolbarPresentationComponentEntity.GetHasButtonEntity : Boolean;
Begin
  Result := FChoice.RetrieveIsObjectClass(TUixAdvancedToolbarPresentationButtonEntity);
End;


Procedure TUixAdvancedToolbarPresentationComponentEntity.SetHasButtonEntity(Const Value : Boolean);
Begin
  FChoice.StoreIsObjectClass(TUixAdvancedToolbarPresentationButtonEntity, Value);
  If Value Then
    ButtonEntity.ParentComponent := Self;
End;


Function TUixAdvancedToolbarPresentationComponentEntity.GetControlEntity : TUixAdvancedToolbarPresentationControlEntity;
Begin
  Result := TUixAdvancedToolbarPresentationControlEntity(FChoice.RetrieveObject(TUixAdvancedToolbarPresentationControlEntity));
End;


Function TUixAdvancedToolbarPresentationComponentEntity.GetHasControlEntity : Boolean;
Begin
  Result := FChoice.RetrieveIsObjectClass(TUixAdvancedToolbarPresentationControlEntity);
End;


Procedure TUixAdvancedToolbarPresentationComponentEntity.SetHasControlEntity(Const Value : Boolean);
Begin
  FChoice.StoreIsObjectClass(TUixAdvancedToolbarPresentationControlEntity, Value);
End;


Function TUixAdvancedToolbarPresentationComponentEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarPresentationComponentEntity;
End;


Function TUixAdvancedToolbarPresentationComponentEntityList.CompareByCaption(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TUixAdvancedToolbarPresentationComponentEntity(pA).Caption, TUixAdvancedToolbarPresentationComponentEntity(pB).Caption);
End;


Function TUixAdvancedToolbarPresentationComponentEntityList.CompareByPositionIndex(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(TUixAdvancedToolbarPresentationComponentEntity(pA).PositionIndex, TUixAdvancedToolbarPresentationComponentEntity(pB).PositionIndex);
End;


Procedure TUixAdvancedToolbarPresentationComponentEntityList.SortedByCaption;
Begin
  SortedBy(CompareByCaption);
End;


Procedure TUixAdvancedToolbarPresentationComponentEntityList.SortedByPositionIndex;
Begin
  SortedBy(CompareByPositionIndex);
End;


Function TUixAdvancedToolbarPresentationComponentEntityList.GetComponentEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationComponentEntity;
Begin
  Result := TUixAdvancedToolbarPresentationComponentEntity(ObjectByIndex[iIndex]);
End;


Constructor TUixAdvancedToolbarPresentationSectionEntity.Create;
Begin
  Inherited;

  FComponentEntityList := TUixAdvancedToolbarPresentationComponentEntityList.Create;
End;


Destructor TUixAdvancedToolbarPresentationSectionEntity.Destroy;
Begin
  FComponentEntityList.Free;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationSectionEntity.Link : TUixAdvancedToolbarPresentationSectionEntity;
Begin
  Result := TUixAdvancedToolbarPresentationSectionEntity(Inherited Link);
End;


Function TUixAdvancedToolbarPresentationSectionEntity.AddNewComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
Begin
  Result := TUixAdvancedToolbarPresentationComponentEntity.Create;
  FComponentEntityList.Add(Result);
End;


Function TUixAdvancedToolbarPresentationSectionEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarPresentationSectionEntity;
End;


Function TUixAdvancedToolbarPresentationSectionEntityList.GetSectionEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationSectionEntity;
Begin
  Result := TUixAdvancedToolbarPresentationSectionEntity(ObjectByIndex[iIndex]);
End;


Constructor TUixAdvancedToolbarPresentationGroupEntity.Create;
Begin
  Inherited;

  FSectionEntityList := TUixAdvancedToolbarPresentationSectionEntityList.Create;
End;


Destructor TUixAdvancedToolbarPresentationGroupEntity.Destroy;
Begin
  FSectionEntityList.Free;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationGroupEntity.Link : TUixAdvancedToolbarPresentationGroupEntity;
Begin
  Result := TUixAdvancedToolbarPresentationGroupEntity(Inherited Link);
End;


Function TUixAdvancedToolbarPresentationGroupEntity.AddNewSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;
Begin
  Result := TUixAdvancedToolbarPresentationSectionEntity.Create;
  FSectionEntityList.Add(Result);
End;


Function TUixAdvancedToolbarPresentationGroupEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarPresentationGroupEntity;
End;


Function TUixAdvancedToolbarPresentationGroupEntityList.GetGroupEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarPresentationGroupEntity;
Begin
  Result := TUixAdvancedToolbarPresentationGroupEntity(ObjectByIndex[iIndex]);
End;


Constructor TUixAdvancedToolbarPresentationSpecificationEntity.Create;
Begin
  Inherited;

  FIconSize := 16;
  FButtonPadding := 4;
  FShowBorders := True;
  FShowImages := True;
End;


Constructor TUixAdvancedToolbarPresentationEntity.Create;
Begin
  Inherited;

  FGroupEntityList := TUixAdvancedToolbarPresentationGroupEntityList.Create;
  FSpecificationEntity := TUixAdvancedToolbarPresentationSpecificationEntity.Create;
End;


Destructor TUixAdvancedToolbarPresentationEntity.Destroy;
Begin
  FSpecificationEntity.Free;
  FGroupEntityList.Free;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationEntity.Link : TUixAdvancedToolbarPresentationEntity;
Begin
  Result := TUixAdvancedToolbarPresentationEntity(Inherited Link);
End;


Function TUixAdvancedToolbarPresentationEntity.AddNewGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
Begin
  Result := TUixAdvancedToolbarPresentationGroupEntity.Create;
  FGroupEntityList.Add(Result);
End;


Constructor TUixAdvancedToolbarExecutionButtonEntity.Create;
Begin
  Inherited;

  FPresentationButtonEntity := Nil;
End;


Destructor TUixAdvancedToolbarExecutionButtonEntity.Destroy;
Begin
  FPresentationButtonEntity.Free;

  Inherited;
End;


Function TUixAdvancedToolbarExecutionButtonEntity.GetPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
Begin
  Assert(Invariants('GetPresentationButtonEntity', FPresentationButtonEntity, TUixAdvancedToolbarPresentationButtonEntity, 'FPresentationButtonEntity'));

  Result := FPresentationButtonEntity;
End;


Procedure TUixAdvancedToolbarExecutionButtonEntity.SetPresentationButtonEntity(Const Value : TUixAdvancedToolbarPresentationButtonEntity);
Begin
  Assert(Invariants('SetPresentationButtonEntity', Value, TUixAdvancedToolbarPresentationButtonEntity, 'Value'));

  FPresentationButtonEntity.Free;
  FPresentationButtonEntity := Value;
End;


Constructor TUixAdvancedToolbarExecutionControlEntity.Create;
Begin
  Inherited;

  FPresentationControlEntity := Nil;
End;


Destructor TUixAdvancedToolbarExecutionControlEntity.Destroy;
Begin
  FPresentationControlEntity.Free;

  Inherited;
End;


Function TUixAdvancedToolbarExecutionControlEntity.GetPresentationControlEntity : TUixAdvancedToolbarPresentationControlEntity;
Begin
  Assert(Invariants('GetPresentationControlEntity', FPresentationControlEntity, TUixAdvancedToolbarPresentationControlEntity, 'FPresentationControlEntity'));

  Result := FPresentationControlEntity;
End;


Procedure TUixAdvancedToolbarExecutionControlEntity.SetPresentationControlEntity(Const Value : TUixAdvancedToolbarPresentationControlEntity);
Begin
  Assert(Invariants('SetPresentationControlEntity', Value, TUixAdvancedToolbarPresentationControlEntity, 'Value'));

  FPresentationControlEntity.Free;
  FPresentationControlEntity := Value;
End;


Constructor TUixAdvancedToolbarExecutionComponentEntity.Create;
Begin
  Inherited;

  FChoice := TFslObjectChoice.Create;

  FPresentationComponentEntity := Nil;
End;


Destructor TUixAdvancedToolbarExecutionComponentEntity.Destroy;
Begin
  FPresentationComponentEntity.Free;
  FChoice.Free;

  Inherited;
End;


Function TUixAdvancedToolbarExecutionComponentEntity.Link : TUixAdvancedToolbarExecutionComponentEntity;
Begin
  Result := TUixAdvancedToolbarExecutionComponentEntity(Inherited Link);
End;


Function TUixAdvancedToolbarExecutionComponentEntity.GetButtonEntity : TUixAdvancedToolbarExecutionButtonEntity;
Begin
  Result := TUixAdvancedToolbarExecutionButtonEntity(FChoice.RetrieveObject(TUixAdvancedToolbarExecutionButtonEntity));
End;


Function TUixAdvancedToolbarExecutionComponentEntity.GetHasButtonEntity : Boolean;
Begin
  Result := FChoice.RetrieveIsObjectClass(TUixAdvancedToolbarExecutionButtonEntity);
End;


Procedure TUixAdvancedToolbarExecutionComponentEntity.SetHasButtonEntity(Const Value : Boolean);
Begin
  FChoice.StoreIsObjectClass(TUixAdvancedToolbarExecutionButtonEntity, Value);
End;


Function TUixAdvancedToolbarExecutionComponentEntity.GetControlEntity : TUixAdvancedToolbarExecutionControlEntity;
Begin
  Result := TUixAdvancedToolbarExecutionControlEntity(FChoice.RetrieveObject(TUixAdvancedToolbarExecutionControlEntity));
End;


Function TUixAdvancedToolbarExecutionComponentEntity.GetHasControlEntity : Boolean;
Begin
  Result := FChoice.RetrieveIsObjectClass(TUixAdvancedToolbarExecutionControlEntity);
End;


Procedure TUixAdvancedToolbarExecutionComponentEntity.SetHasControlEntity(Const Value : Boolean);
Begin
  FChoice.StoreIsObjectClass(TUixAdvancedToolbarExecutionControlEntity, Value);
End;


Function TUixAdvancedToolbarExecutionComponentEntity.GetPresentationComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
Begin
  Assert(Invariants('GetPresentationComponentEntity', FPresentationComponentEntity, TUixAdvancedToolbarPresentationComponentEntity, 'FPresentationComponentEntity'));

  Result := FPresentationComponentEntity;
End;


Procedure TUixAdvancedToolbarExecutionComponentEntity.SetPresentationComponentEntity(Const Value : TUixAdvancedToolbarPresentationComponentEntity);
Begin
  Assert(Invariants('SetPresentationComponentEntity', Value, TUixAdvancedToolbarPresentationComponentEntity, 'Value'));

  FPresentationComponentEntity.Free;
  FPresentationComponentEntity := Value;
End;


Function TUixAdvancedToolbarExecutionComponentEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarExecutionComponentEntity;
End;


Function TUixAdvancedToolbarExecutionComponentEntityList.CompareByCaption(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TUixAdvancedToolbarExecutionComponentEntity(pA).PresentationComponentEntity.Caption,
              TUixAdvancedToolbarExecutionComponentEntity(pB).PresentationComponentEntity.Caption);
End;


Function TUixAdvancedToolbarExecutionComponentEntityList.CompareByPositionIndex(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(TUixAdvancedToolbarExecutionComponentEntity(pA).PresentationComponentEntity.PositionIndex,
              TUixAdvancedToolbarExecutionComponentEntity(pB).PresentationComponentEntity.PositionIndex);
End;


Procedure TUixAdvancedToolbarExecutionComponentEntityList.SortedByCaption;
Begin
  SortedBy(CompareByCaption);
End;


Procedure TUixAdvancedToolbarExecutionComponentEntityList.SortedByPositionIndex;
Begin
  SortedBy(CompareByPositionIndex);
End;


Function TUixAdvancedToolbarExecutionComponentEntityList.GetComponentEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionComponentEntity;
Begin
  Result := TUixAdvancedToolbarExecutionComponentEntity(ObjectByIndex[iIndex]);
End;


Constructor TUixAdvancedToolbarExecutionSectionEntity.Create;
Begin
  Inherited;

  FComponentEntityList := TUixAdvancedToolbarExecutionComponentEntityList.Create;
  FPresentationSectionEntity := Nil;
End;


Destructor TUixAdvancedToolbarExecutionSectionEntity.Destroy;
Begin
  FPresentationSectionEntity.Free;
  FComponentEntityList.Free;

  Inherited;
End;


Function TUixAdvancedToolbarExecutionSectionEntity.GetPresentationSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;
Begin
  Assert(Invariants('GetPresentationSectionEntity', FPresentationSectionEntity, TUixAdvancedToolbarPresentationSectionEntity, 'FPresentationSectionEntity'));

  Result := FPresentationSectionEntity;
End;


Procedure TUixAdvancedToolbarExecutionSectionEntity.SetPresentationSectionEntity(Const Value : TUixAdvancedToolbarPresentationSectionEntity);
Begin
  Assert(Invariants('SetPresentationSectionEntity', Value, TUixAdvancedToolbarPresentationSectionEntity, 'Value'));

  FPresentationSectionEntity.Free;
  FPresentationSectionEntity := Value;
End;


Function TUixAdvancedToolbarExecutionSectionEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarExecutionSectionEntity;
End;


Function TUixAdvancedToolbarExecutionSectionEntityList.GetSectionEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionSectionEntity;
Begin
  Result := TUixAdvancedToolbarExecutionSectionEntity(ObjectByIndex[iIndex]);
End;


Constructor TUixAdvancedToolbarExecutionGroupEntity.Create;
Begin
  Inherited;

  FPresentationGroupEntity := Nil;
  FSectionEntityList := TUixAdvancedToolbarExecutionSectionEntityList.Create;
End;


Destructor TUixAdvancedToolbarExecutionGroupEntity.Destroy;
Begin
  FSectionEntityList.Free;
  FPresentationGroupEntity.Free;

  Inherited;
End;


Function TUixAdvancedToolbarExecutionGroupEntity.Link : TUixAdvancedToolbarExecutionGroupEntity;
Begin
  Result := TUixAdvancedToolbarExecutionGroupEntity(Inherited Link);
End;


Function TUixAdvancedToolbarExecutionGroupEntity.GetPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
Begin
  Assert(Invariants('GetPresentationGroupEntity', FPresentationGroupEntity, TUixAdvancedToolbarPresentationGroupEntity, 'FPresentationGroupEntity'));

  Result := FPresentationGroupEntity;
End;


Procedure TUixAdvancedToolbarExecutionGroupEntity.SetPresentationGroupEntity(Const Value : TUixAdvancedToolbarPresentationGroupEntity);
Begin
  Assert(Invariants('SetPresentationGroupEntity', Value, TUixAdvancedToolbarPresentationGroupEntity, 'Value'));

  FPresentationGroupEntity.Free;
  FPresentationGroupEntity := Value;
End;


Function TUixAdvancedToolbarExecutionGroupEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarExecutionGroupEntity;
End;


Function TUixAdvancedToolbarExecutionGroupEntityList.GetGroupEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarExecutionGroupEntity;
Begin
  Result := TUixAdvancedToolbarExecutionGroupEntity(ObjectByIndex[iIndex]);
End;


Function TUixAdvancedToolbarExecutionGroupEntityList.CompareByPresentationGroupEntity(pA, pB : Pointer) : Integer;
Begin
  Result := IntegerCompare(Integer(TUixAdvancedToolbarExecutionGroupEntity(pA).PresentationGroupEntity),
              Integer(TUixAdvancedToolbarExecutionGroupEntity(pB).PresentationGroupEntity));
End;


Function TUixAdvancedToolbarExecutionGroupEntityList.IndexByPresentationGroupEntity(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity) : Integer;
Var
  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
Begin
  oExecutionGroupEntity := TUixAdvancedToolbarExecutionGroupEntity.Create;
  Try
    oExecutionGroupEntity.PresentationGroupEntity := oPresentationGroupEntity.Link;

    If Not Find(oExecutionGroupEntity, Result, CompareByPresentationGroupEntity) Then
      Result := -1;
  Finally
    oExecutionGroupEntity.Free;
  End;
End;


Function TUixAdvancedToolbarExecutionGroupEntityList.ExistsByPresentationGroupEntity(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity) : Boolean;
Begin
  Result := ExistsByIndex(IndexByPresentationGroupEntity(oPresentationGroupEntity));
End;


Constructor TUixAdvancedToolbarExecutionEntity.Create;
Begin
  Inherited;

  FGroupEntityList := TUixAdvancedToolbarExecutionGroupEntityList.Create;
End;


Destructor TUixAdvancedToolbarExecutionEntity.Destroy;
Begin
  FGroupEntityList.Free;

  Inherited;
End;


Constructor TUixAdvancedToolbarHotSpotEntity.Create;
Begin
  Inherited;

  FHotSpot := TGdiPlusHotSpot.Create;
  FGroupEntity := Nil;
  FComponentEntity := Nil;
End;


Destructor TUixAdvancedToolbarHotSpotEntity.Destroy;
Begin
  FComponentEntity.Free;
  FGroupEntity.Free;
  FHotSpot.Free;

  Inherited;
End;


Function TUixAdvancedToolbarHotSpotEntity.Link : TUixAdvancedToolbarHotSpotEntity;
Begin
  Result := TUixAdvancedToolbarHotSpotEntity(Inherited Link);
End;


Function TUixAdvancedToolbarHotSpotEntity.HasComponentEntity : Boolean;
Begin
  Result := Assigned(FComponentEntity);
End;


Function TUixAdvancedToolbarHotSpotEntity.HasGroupEntity : Boolean;
Begin
  Result := Assigned(FGroupEntity);
End;


Function TUixAdvancedToolbarHotSpotEntity.IsEquivalent(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity) : Boolean;
Begin
  Result := FActionStyle = oHotSpotEntity.ActionStyle;

  If Result Then
  Begin
    Result := HasGroupEntity = oHotSpotEntity.HasGroupEntity;

    If Result Then
    Begin
      If HasGroupEntity Then
        Result := oHotSpotEntity.IsGroupEntity(FGroupEntity)
      Else
      Begin
        Result := HasComponentEntity = oHotSpotEntity.HasComponentEntity;

        If Result And HasComponentEntity Then
          Result := oHotSpotEntity.IsComponentEntity(FComponentEntity);
      End;
    End;
  End;
End;


Function TUixAdvancedToolbarHotSpotEntity.IsGroupEntity(Const oGroupEntity : TUixAdvancedToolbarExecutionGroupEntity) : Boolean;
Begin
  Result := HasGroupEntity And (FGroupEntity = oGroupEntity);
End;


Function TUixAdvancedToolbarHotSpotEntity.IsComponentEntity(Const oComponentEntity : TUixAdvancedToolbarExecutionComponentEntity) : Boolean;
Begin
  Result := HasComponentEntity And (FComponentEntity = oComponentEntity);
End;


Function TUixAdvancedToolbarHotSpotEntity.GetGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
Begin
  Assert(Invariants('GetGroupEntity', FGroupEntity, TUixAdvancedToolbarExecutionGroupEntity, 'FGroupEntity'));

  Result := FGroupEntity;
End;


Procedure TUixAdvancedToolbarHotSpotEntity.SetGroupEntity(Const Value : TUixAdvancedToolbarExecutionGroupEntity);
Begin
  Assert(Invariants('SetGroupEntity', Value, TUixAdvancedToolbarExecutionGroupEntity, 'Value'));

  FGroupEntity.Free;
  FGroupEntity := Value;
End;


Function TUixAdvancedToolbarHotSpotEntity.GetComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
Begin
  Assert(Invariants('GetComponentEntity', FComponentEntity, TUixAdvancedToolbarExecutionComponentEntity, 'FComponentEntity'));

  Result := FComponentEntity;
End;


Procedure TUixAdvancedToolbarHotSpotEntity.SetComponentEntity(Const Value : TUixAdvancedToolbarExecutionComponentEntity);
Begin
  Assert(Invariants('SetComponentEntity', Value, TUixAdvancedToolbarExecutionComponentEntity, 'Value'));

  FComponentEntity.Free;
  FComponentEntity := Value;
End;


Function TUixAdvancedToolbarHotSpotEntityList.ItemClass : TFslObjectClass;
Begin
  Result := TUixAdvancedToolbarHotSpotEntity;
End;


Function TUixAdvancedToolbarHotSpotEntityList.GetHotSpotEntityByIndex(Const iIndex : Integer) : TUixAdvancedToolbarHotSpotEntity;
Begin
  Result := TUixAdvancedToolbarHotSpotEntity(ObjectByIndex[iIndex]);
End;


Const
  UixAdvancedToolbarTextHeight = 15;
  UixAdvancedToolbarSectionPadding = 6;
  UixAdvancedToolbarGroupPadding = 6;
  UixAdvancedToolbarChevronWidth = 16;
  UixAdvancedToolbarControlDropDownWidth = 14;
  UixAdvancedToolbarControlDropDownArrowWidth = 6;
  UixAdvancedToolbarControlDropDownArrowHeight = 3;


Constructor TUixAdvancedToolbar.Create(oOwner: TComponent);
Begin
  Inherited;

  FPresentationEntity := TUixAdvancedToolbarPresentationEntity.Create;
  FExecutionEntity := TUixAdvancedToolbarExecutionEntity.Create;

  DoubleBuffered := True;
  PaintChildControls := True;
  ShowHint := True;

  FPen := TGPPen.Create(argbDodgerBlue);
  FSolidBrush := TGPSolidBrush.Create(argbDodgerBlue);
  FChevronPath := TGPGraphicsPath.Create;

  FGrayScaleImageAttributes := TGPImageAttributes.Create;
  FGrayScaleImageAttributes.SetColorMatrix(GrayScaleColorMatrixArray);

  FDefaultImageAttributes := TGPImageAttributes.Create;

  FCaptionFont := TGdiPlusFont.Create;
  FCaptionFont.Size := 8;
  FCaptionFont.FontFamily := 'Tahoma';

  FCaptionStringFormat := TGdiPlusStringFormat.Create;
  FCaptionStringFormat.TrimModeNone;
  FCaptionStringFormat.VerticalAlignmentMiddle;
  FCaptionStringFormat.HorizontalAlignmentCenter;
  FCaptionStringFormat.NoWrap := True;

  FHotSpotEntityList := TUixAdvancedToolbarHotSpotEntityList.Create;

  FHotSpotMouseDownEntity := Nil;
  FHotSpotMouseHoverEntity := Nil;

  FVisibleControlGroupEntityList := TUixAdvancedToolbarExecutionGroupEntityList.Create;
  FVisibleControlComponentEntityList := TUixAdvancedToolbarExecutionComponentEntityList.Create;

  FPresentationExecutionButtonEntityMatch := TFslObjectMatch.Create;
  FPresentationExecutionButtonEntityMatch.NominatedKeyClass := TUixAdvancedToolbarPresentationButtonEntity;
  FPresentationExecutionButtonEntityMatch.NominatedValueClass := TUixAdvancedToolbarExecutionButtonEntity;
End;


Destructor TUixAdvancedToolbar.Destroy;
Begin
  FPresentationExecutionButtonEntityMatch.Free;

  FVisibleControlComponentEntityList.Free;
  FVisibleControlGroupEntityList.Free;

  FHotSpotMouseHoverEntity.Free;
  FHotSpotMouseDownEntity.Free;

  FHotSpotEntityList.Free;

  FCaptionStringFormat.Free;
  FCaptionFont.Free;

  FDefaultImageAttributes.Free;
  FGrayScaleImageAttributes.Free;
  FChevronPath.Free;
  FSolidBrush.Free;
  FPen.Free;

  FExecutionEntity.Free;
  FPresentationEntity.Free;

  Inherited;
End;


Procedure TUixAdvancedToolbar.CreateParams(Var aParams : TCreateParams);
Begin
  Inherited;

  aParams.Style := aParams.Style Or WS_CLIPCHILDREN;
End;


Procedure TUixAdvancedToolbar.Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect);
Var
  aButtonPathRectangle : TGPRectF;
  aButtonBorderRectangle : TGPRect;
  aButtonToggleRectangle : TGPRect;
  aButtonSeparatorRectangle : TGPRect;
  aButtonTopBackgroundColour : TArgbColour;
  aButtonBottomBackgroundColour : TArgbColour;

  aChevronPointArray : TPointFDynArray;
  aChevronRectangle : TGPRect;

  bGroupIsVisible : Boolean;
  bSectionIsVisible : Boolean;
  bComponentIsVisible : Boolean;
  bRequiresVerticalSeparation : Boolean;

  iControlTop : Integer;

  iExecutionGroupEntityIndex : Integer;
  iExecutionSectionEntityIndex : Integer;
  iExecutionComponentEntityIndex : Integer;

  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oExecutionSectionEntity : TUixAdvancedToolbarExecutionSectionEntity;
  oExecutionComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
  oExecutionButtonEntity : TUixAdvancedToolbarExecutionButtonEntity;

  oExecutionPreviousComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;

  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;

  oPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;

  oButtonPath : TGPGraphicsPath;
  oLinearGradientBrush : TGPLinearGradientBrush;
  oButtonImageAttributes : TGPImageAttributes;

  rButtonPathRadius : Real;
  rButtonPathDiameter : Real;

  sButtonCaption : String;
  sGroupCaption : String;

  oControlControl : TControl;

  iConfigurationOffset : Integer;
  iConfigurationBottom : Integer;
  iConfigurationRight : Integer;
  aPointArray : TPointDynArray;
  oConfigurationPath : TGPGraphicsPath;
Const
  aPathFactorArray : Array [0..10] Of Single = (0.1, 0.1, 0.2, 0.2, 1.0, 0.9, 0.7, 0.5, 0.3, 0.3, 0.2);
  aPathPositionArray : Array [0..10] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1);

  aConfigurationFactorArray : Array [0..5] Of Single = (0.3, 0.3, 0.4, 0.5, 0.6, 0.7);
  aConfigurationPostitionArray : Array [0..5] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 1);
Begin
  Inherited;

  CalculateMetrics(oGraphics);

  FHotSpotEntityList.Clear;

  oGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

  If PresentationEntity.SpecificationEntity.ShowConfiguration Then
  Begin
    iConfigurationOffset := FExecutionEntity.ConfigurationRectangle.Height Div 4;

    SetLength(aPointArray, 3);
    aPointArray[0] := MakePoint(FExecutionEntity.ConfigurationRectangle.X + FExecutionEntity.ConfigurationRectangle.Width, FExecutionEntity.ConfigurationRectangle.Y);
    aPointArray[1] := MakePoint(FExecutionEntity.ConfigurationRectangle.X, FExecutionEntity.ConfigurationRectangle.Y + FExecutionEntity.ConfigurationRectangle.Height);
    aPointArray[2] := MakePoint(FExecutionEntity.ConfigurationRectangle.X, FExecutionEntity.ConfigurationRectangle.Y);

    oConfigurationPath := TGPGraphicsPath.Create;
    Try
      oConfigurationPath.AddPolygon(PGPPoint(aPointArray), 3);
      oGraphics.SetClip(oConfigurationPath);

      oLinearGradientBrush := TGPLinearGradientBrush.Create(FExecutionEntity.ConfigurationRectangle, argbBlack, argbLightGray, LinearGradientModeForwardDiagonal);
      Try
        oLinearGradientBrush.SetBlend(@aConfigurationFactorArray, @aConfigurationPostitionArray, 6);

        oGraphics.FillRectangle(oLinearGradientBrush, FExecutionEntity.ConfigurationRectangle);
      Finally
        oLinearGradientBrush.Free;
      End;

      If Assigned(FHotSpotMouseHoverEntity) And FHotSpotMouseHoverEntity.IsEquivalent(FConfigurationHotSpotEntity) Then
      Begin
        FPen.SetColor(argbDarkGray);
        oGraphics.DrawPath(FPen,oConfigurationPath);

        oLinearGradientBrush := TGPLinearGradientBrush.Create(FExecutionEntity.ConfigurationRectangle, $FFFFD048, $FFFEF7D5, LinearGradientModeForwardDiagonal);
        Try
          oLinearGradientBrush.SetBlend(@aConfigurationFactorArray, @aConfigurationPostitionArray, 6);

          oGraphics.FillRectangle(oLinearGradientBrush, FExecutionEntity.ConfigurationRectangle);
        Finally
          oLinearGradientBrush.Free;
        End;
      End;

      oGraphics.ResetClip;
    Finally
      oConfigurationPath.Free;
    End;

    FConfigurationHotSpotEntity := AddNewHotSpotEntity;
    FConfigurationHotSpotEntity.HotSpot.SetRegion(FExecutionEntity.ConfigurationRectangle);
    FConfigurationHotSpotEntity.ActionStyle := UixAdvancedToolbarHotSpotActionStyleConfiguration;
  End;

  For iExecutionGroupEntityIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
  Begin
    oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];

    bGroupIsVisible := oExecutionGroupEntity.ContainsVisibleComponents And Not oExecutionGroupEntity.HiddenWithinChevron;

    If bGroupIsVisible And oExecutionGroupEntity.HasSeparator Then
    Begin
      FPen.SetColor(argbDarkGray);
      oGraphics.DrawLine(FPen, oExecutionGroupEntity.SeparatorX, oExecutionGroupEntity.SeparatorY, oExecutionGroupEntity.SeparatorX, oExecutionGroupEntity.SeparatorY + RowHeight);

      FPen.SetColor(argbLightGray);
      oGraphics.DrawLine(FPen, oExecutionGroupEntity.SeparatorX + 1, oExecutionGroupEntity.SeparatorY, oExecutionGroupEntity.SeparatorX + 1, oExecutionGroupEntity.SeparatorY + RowHeight);
    End;

    If bGroupIsVisible And oExecutionGroupEntity.CollapsedToCaption Then
    Begin
      sGroupCaption := oExecutionGroupEntity.PresentationGroupEntity.Caption;

      FPen.SetColor(argbDarkGray);
      oGraphics.DrawRoundedRectangle(FPen, oExecutionGroupEntity.CaptionBorderRectangle, 2);

      If Assigned(FHotSpotMouseHoverEntity) And FHotSpotMouseHoverEntity.IsGroupEntity(oExecutionGroupEntity) Then
      Begin
        aButtonTopBackgroundColour := $FFFEF7D5;
        aButtonBottomBackgroundColour := $FFFFD048;
      End
      Else
      Begin
        aButtonTopBackgroundColour := argbTransparent;
        aButtonBottomBackgroundColour := argbTransparent;
      End;

      oLinearGradientBrush := TGPLinearGradientBrush.Create(oExecutionGroupEntity.CaptionBackgroundRectangle, aButtonTopBackgroundColour, aButtonBottomBackgroundColour, LinearGradientModeVertical);
      Try
        oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

        oGraphics.FillRectangle(oLinearGradientBrush, oExecutionGroupEntity.CaptionBackgroundRectangle);
      Finally
        oLinearGradientBrush.Free;
      End;

      FSolidBrush.SetColor(argbBlack);
      oGraphics.DrawString(sGroupCaption, Length(sGroupCaption), FCaptionFont.ProduceFont, MakeRectF(oExecutionGroupEntity.CaptionTextRectangle), FCaptionStringFormat.ProduceStringFormat, FSolidBrush);

      FSolidBrush.SetColor(argbBlack);
      oGraphics.FillTriangle(FSolidBrush, oExecutionGroupEntity.CaptionDropDownArrowRectangle);

      oHotSpotEntity := AddNewHotSpotEntity;
      oHotSpotEntity.HotSpot.SetRegion(oExecutionGroupEntity.CaptionBorderRectangle);
      oHotSpotEntity.ActionStyle := UixAdvancedToolbarHotSpotActionStyleGroup;
      oHotSpotEntity.GroupEntity := oExecutionGroupEntity.Link;
    End;

    For iExecutionSectionEntityIndex := 0 To oExecutionGroupEntity.SectionEntityList.Count - 1 Do
    Begin
      oExecutionSectionEntity := oExecutionGroupEntity.SectionEntityList[iExecutionSectionEntityIndex];

      bSectionIsVisible := bGroupIsVisible And Not oExecutionGroupEntity.CollapsedToCaption And oExecutionSectionEntity.ContainsVisibleComponents;
      oExecutionPreviousComponentEntity := Nil;

      For iExecutionComponentEntityIndex := 0 To oExecutionSectionEntity.ComponentEntityList.Count - 1 Do
      Begin
        oExecutionComponentEntity := oExecutionSectionEntity.ComponentEntityList[iExecutionComponentEntityIndex];

        bComponentIsVisible := bSectionIsVisible And oExecutionComponentEntity.PresentationComponentEntity.IsVisible;

        If oExecutionComponentEntity.HasButtonEntity Then
        Begin
          If bComponentIsVisible Then
          Begin
            oExecutionButtonEntity := oExecutionComponentEntity.ButtonEntity;
            oPresentationButtonEntity := oExecutionButtonEntity.PresentationButtonEntity;

            If oPresentationButtonEntity.ShowBorder Or PresentationEntity.SpecificationEntity.ShowBorders Or oExecutionComponentEntity.PresentationComponentEntity.ButtonEntity.IsToggled Or (Assigned(FHotSpotMouseHoverEntity) And FHotSpotMouseHoverEntity.IsComponentEntity(oExecutionComponentEntity)) Then
            Begin
              oButtonPath := TGPGraphicsPath.Create;
              Try
                If Not oExecutionButtonEntity.IsFirstInSection And Not oExecutionButtonEntity.IsLastInSection Then
                Begin
                  oButtonPath.AddRectangle(oExecutionButtonEntity.BorderRectangle);
                End
                Else If oExecutionButtonEntity.IsFirstInSection And oExecutionButtonEntity.IsLastInSection Then
                Begin
                  RoundedRectanglePath(oButtonPath, oExecutionButtonEntity.BorderRectangle, 2);
                End
                Else
                Begin
                  aButtonBorderRectangle := oExecutionButtonEntity.BorderRectangle;

                  oButtonPath.StartFigure;

                  rButtonPathRadius := 2;
                  rButtonPathDiameter := rButtonPathRadius * 2;

                  If oExecutionComponentEntity.ButtonEntity.IsFirstInSection Then
                  Begin
                    oButtonPath.AddLine(aButtonBorderRectangle.X + aButtonBorderRectangle.Width, aButtonBorderRectangle.Y, aButtonBorderRectangle.X + rButtonPathRadius, aButtonBorderRectangle.Y);

                    aButtonPathRectangle := MakeRect(aButtonBorderRectangle.X, aButtonBorderRectangle.Y, rButtonPathDiameter, rButtonPathDiameter);
                    oButtonPath.AddArc(aButtonPathRectangle, 270, -90);

                    aButtonPathRectangle.Y := aButtonBorderRectangle.Y + aButtonBorderRectangle.Height - rButtonPathDiameter;
                    oButtonPath.AddArc(aButtonPathRectangle, 180, -90);

                    oButtonPath.AddLine(aButtonBorderRectangle.X + rButtonPathRadius, aButtonBorderRectangle.Y + aButtonBorderRectangle.Height, aButtonBorderRectangle.X + aButtonBorderRectangle.Width, aButtonBorderRectangle.Y + aButtonBorderRectangle.Height);
                  End
                  Else
                  Begin
                    oButtonPath.AddLine(aButtonBorderRectangle.X, aButtonBorderRectangle.Y, aButtonBorderRectangle.X + aButtonBorderRectangle.Width - rButtonPathRadius, aButtonBorderRectangle.Y);

                    aButtonPathRectangle := MakeRect(aButtonBorderRectangle.X + aButtonBorderRectangle.Width - rButtonPathDiameter, aButtonBorderRectangle.Y, rButtonPathDiameter, rButtonPathDiameter);
                    oButtonPath.AddArc(aButtonPathRectangle, 270, 90);

                    aButtonPathRectangle.Y := aButtonBorderRectangle.Y + aButtonBorderRectangle.Height - rButtonPathDiameter;
                    oButtonPath.AddArc(aButtonPathRectangle, 0, 90);

                    oButtonPath.AddLine(aButtonBorderRectangle.X + aButtonBorderRectangle.Width - rButtonPathRadius, aButtonBorderRectangle.Y + aButtonBorderRectangle.Height, aButtonBorderRectangle.X, aButtonBorderRectangle.Y + aButtonBorderRectangle.Height);
                  End;

                  oButtonPath.CloseFigure;
                End;

                If oExecutionComponentEntity.PresentationComponentEntity.ButtonEntity.IsToggled Then
                  FPen.SetColor(argbHalfTransparentBlack)
                Else
                  FPen.SetColor(oPresentationButtonEntity.BorderColour);

                oGraphics.DrawPath(FPen, oButtonPath);
              Finally
                oButtonPath.Free;
              End;
            End;

            If Not oExecutionComponentEntity.PresentationComponentEntity.IsEnabled Then
            Begin
              aButtonTopBackgroundColour := argbTransparent;
              aButtonBottomBackgroundColour := argbTransparent;
            End
            Else If Assigned(FHotSpotMouseHoverEntity) And FHotSpotMouseHoverEntity.IsComponentEntity(oExecutionComponentEntity) Then
            Begin
              Case FHotSpotMouseHoverEntity.ActionStyle Of
                UixAdvancedToolbarHotSpotActionStyleButton :
                Begin
                  If Assigned(FHotSpotMouseDownEntity) And FHotSpotMouseDownEntity.IsComponentEntity(oExecutionComponentEntity) And (FHotSpotMouseDownEntity.ActionStyle = UixAdvancedToolbarHotSpotActionStyleButton) Then
                  Begin
                    aButtonTopBackgroundColour := $FFFEF7D5;
                    aButtonBottomBackgroundColour := $FFFFB31C;
                  End
                  Else If oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.IsToggled Then
                  Begin
                    aButtonTopBackgroundColour := $FFFBDBB5;
                    aButtonBottomBackgroundColour := $FFF7BD7B;
                  End
                  Else
                  Begin
                    aButtonTopBackgroundColour := $FFFEF7D5;
                    aButtonBottomBackgroundColour := $FFFFD048;
                  End;
                End;

                UixAdvancedToolbarHotSpotActionStyleButtonDropDown :
                Begin
                  aButtonTopBackgroundColour := $FFFFFCF0;
                  aButtonBottomBackgroundColour := $FFFFEEBE;
                End;
              Else
                Error('Paint', 'Unexpected hot spot action style.');

                aButtonTopBackgroundColour := argbWhite;
                aButtonBottomBackgroundColour := argbWhite;
              End;
            End
            Else If oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.IsToggled Then
            Begin
              aButtonTopBackgroundColour := $FFFBDBB5;
              aButtonBottomBackgroundColour := $FFFEB456;
            End
            Else
            Begin
              aButtonTopBackgroundColour := argbTransparent;
              aButtonBottomBackgroundColour := argbTransparent;
            End;

            oLinearGradientBrush := TGPLinearGradientBrush.Create(oExecutionComponentEntity.ButtonEntity.BackgroundRectangle, aButtonTopBackgroundColour, aButtonBottomBackgroundColour, LinearGradientModeVertical);
            Try
              oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

              oGraphics.FillRectangle(oLinearGradientBrush, oExecutionComponentEntity.ButtonEntity.BackgroundRectangle);
            Finally
              oLinearGradientBrush.Free;
            End;

            If Not oExecutionComponentEntity.ButtonEntity.IsFirstInSection Then
            Begin
              bRequiresVerticalSeparation := Assigned(FHotSpotMouseHoverEntity) And (FHotSpotMouseHoverEntity.IsComponentEntity(oExecutionComponentEntity) Or FHotSpotMouseHoverEntity.IsComponentEntity(oExecutionPreviousComponentEntity));

              aButtonSeparatorRectangle := oExecutionComponentEntity.ButtonEntity.BorderRectangle;

              If Not oExecutionComponentEntity.PresentationComponentEntity.ButtonEntity.IsToggled Then
              Begin
                aButtonSeparatorRectangle.Y := aButtonSeparatorRectangle.Y + 1;
                aButtonSeparatorRectangle.Height := aButtonSeparatorRectangle.Height - 2;
              End;

              If PresentationEntity.SpecificationEntity.ShowBorders Or bRequiresVerticalSeparation Then
              Begin
                If bRequiresVerticalSeparation Then
                  FPen.SetColor(argbDarkGray)
                Else
                  FPen.SetColor(argbLightGray);

                oGraphics.DrawLine(FPen, aButtonSeparatorRectangle.X, aButtonSeparatorRectangle.Y, aButtonSeparatorRectangle.X, aButtonSeparatorRectangle.Y + aButtonSeparatorRectangle.Height);
              End;
            End;

            If PresentationEntity.SpecificationEntity.ShowCaption And oExecutionComponentEntity.PresentationComponentEntity.ShowCaption Then
            Begin
              sButtonCaption := oExecutionComponentEntity.PresentationComponentEntity.Caption;

              If oExecutionComponentEntity.PresentationComponentEntity.IsEnabled Then
              Begin
                If oExecutionComponentEntity.PresentationComponentEntity.ButtonEntity.CustomTextColour <> 0 Then
                  FSolidBrush.SetColor(oExecutionComponentEntity.PresentationComponentEntity.ButtonEntity.CustomTextColour)
                Else
                  FSolidBrush.SetColor(argbBlack);
              End
              Else
                FSolidBrush.SetColor(argbDarkGray);

              oGraphics.DrawString(sButtonCaption, Length(sButtonCaption), FCaptionFont.ProduceFont, MakeRectF(oExecutionComponentEntity.ButtonEntity.ButtonCaptionRectangle), FCaptionStringFormat.ProduceStringFormat, FSolidBrush);
            End;

            If oExecutionComponentEntity.PresentationComponentEntity.IsEnabled Then
            Begin
              oHotSpotEntity := AddNewHotSpotEntity;
              oHotSpotEntity.HotSpot.SetRegion(oExecutionComponentEntity.ButtonEntity.BorderRectangle);
              oHotSpotEntity.ActionStyle := UixAdvancedToolbarHotSpotActionStyleButton;
              oHotSpotEntity.ComponentEntity := oExecutionComponentEntity.Link;

              oButtonImageAttributes := FDefaultImageAttributes;
            End
            Else
            Begin
              oButtonImageAttributes := FGrayScaleImageAttributes;
            End;

            If PresentationEntity.SpecificationEntity.UseFuchsiaTransparency Then
              oButtonImageAttributes.SetColorKey(argbFuchsia, argbFuchsia)
            Else If oPresentationButtonEntity.HasBitmapImage And (oPresentationButtonEntity.BitmapImage.TransparentColour <> argbFuchsia) Then
              oButtonImageAttributes.SetColorKey(oPresentationButtonEntity.BitmapImage.TransparentColour, oPresentationButtonEntity.BitmapImage.TransparentColour)
            Else
              oButtonImageAttributes.ClearColorKey;

            If PresentationEntity.SpecificationEntity.ShowImages Then
            Begin
              If oPresentationButtonEntity.HasBitmapImage Then
                oGraphics.DrawImage(oPresentationButtonEntity.BitmapImage.NativeBitmap, MakeRectF(oExecutionComponentEntity.ButtonEntity.ButtonImageRectangle), oButtonImageAttributes)
              Else If Assigned(oPresentationButtonEntity.CustomIconEvent) Then
                oPresentationButtonEntity.CustomIconEvent(oGraphics, oExecutionComponentEntity.ButtonEntity.ButtonImageRectangle, oPresentationButtonEntity);
            End;

            If Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownHandler) Or Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownPopup) Then
            Begin
              If Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.ClickHandler) And Assigned(FHotSpotMouseHoverEntity) And FHotSpotMouseHoverEntity.IsComponentEntity(oExecutionComponentEntity) Then
              Begin
                FPen.SetColor(argbDarkGray);
                oGraphics.DrawRectangle(FPen, oExecutionComponentEntity.ButtonEntity.DropDownBorderRectangle);

                If Assigned(FHotSpotMouseDownEntity) And FHotSpotMouseDownEntity.IsComponentEntity(oExecutionComponentEntity) And
                  (FHotSpotMouseDownEntity.ActionStyle = UixAdvancedToolbarHotSpotActionStyleButtonDropDown) And
                  (FHotSpotMouseHoverEntity.ActionStyle = UixAdvancedToolbarHotSpotActionStyleButtonDropDown) Then
                Begin
                  aButtonTopBackgroundColour := $FFFEF7D5;
                  aButtonBottomBackgroundColour := $FFFFB31C;
                End
                Else If (FHotSpotMouseHoverEntity.ActionStyle = UixAdvancedToolbarHotSpotActionStyleButton) Then
                Begin
                  aButtonTopBackgroundColour := $FFFFFCF0;
                  aButtonBottomBackgroundColour := $FFFFEEBE;
                End
                Else
                Begin
                  aButtonTopBackgroundColour := $FFFEF7D5;
                  aButtonBottomBackgroundColour := $FFFFD048;
                End;

                oLinearGradientBrush := TGPLinearGradientBrush.Create(oExecutionComponentEntity.ButtonEntity.DropDownBackgroundRectangle, aButtonTopBackgroundColour, aButtonBottomBackgroundColour, LinearGradientModeVertical);
                Try
                  oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

                  oGraphics.FillRectangle(oLinearGradientBrush, oExecutionComponentEntity.ButtonEntity.DropDownBackgroundRectangle);
                Finally
                  oLinearGradientBrush.Free;
                End;

                If oExecutionComponentEntity.PresentationComponentEntity.IsEnabled Then
                Begin
                  oHotSpotEntity := AddNewHotSpotEntity;
                  oHotSpotEntity.HotSpot.SetRegion(oExecutionComponentEntity.ButtonEntity.DropDownBorderRectangle);
                  oHotSpotEntity.ActionStyle := UixAdvancedToolbarHotSpotActionStyleButtonDropDown;
                  oHotSpotEntity.ComponentEntity := oExecutionComponentEntity.Link;
                End;
              End;

              FSolidBrush.SetColor(argbDarkGray);

              oGraphics.FillTriangle(FSolidBrush, oExecutionComponentEntity.ButtonEntity.DropDownArrowRectangle);
            End;
          End;
        End
        Else If oExecutionComponentEntity.HasControlEntity Then
        Begin
          oControlControl := oExecutionComponentEntity.PresentationComponentEntity.ControlEntity.Control;

          If bComponentIsVisible Then
          Begin
            // Reparent for collapsed group popup
            If oControlControl.Parent <> Self Then
              oControlControl.Parent := Self;

            If oControlControl.Top <> oExecutionComponentEntity.ControlEntity.EditControlRectangle.Y Then
              oControlControl.Top := oExecutionComponentEntity.ControlEntity.EditControlRectangle.Y;

            If oControlControl.Left <> oExecutionComponentEntity.ControlEntity.EditControlRectangle.X Then
              oControlControl.Left := oExecutionComponentEntity.ControlEntity.EditControlRectangle.X;

            If oControlControl.Width <> oExecutionComponentEntity.ControlEntity.EditControlRectangle.Width Then
              oControlControl.Width := oExecutionComponentEntity.ControlEntity.EditControlRectangle.Width;

            If oControlControl.Height <> oExecutionComponentEntity.ControlEntity.EditControlRectangle.Height Then
              oControlControl.Height := oExecutionComponentEntity.ControlEntity.EditControlRectangle.Height;
          End;

          If oControlControl.Visible <> bComponentIsVisible Then
            oControlControl.Visible := bComponentIsVisible;
        End;

        If bComponentIsVisible Then
          oExecutionPreviousComponentEntity := oExecutionComponentEntity;
      End;
    End;
  End;

  If FExecutionEntity.ChevronIsVisible Then
  Begin
    FChevronPath.Reset;

    aChevronRectangle.X := FClientRectangle.X + FClientRectangle.Width - UixAdvancedToolbarChevronWidth;
    aChevronRectangle.Y := FClientRectangle.Y;
    aChevronRectangle.Width := UixAdvancedToolbarChevronWidth;
    aChevronRectangle.Height := FClientRectangle.Height;

    If Assigned(FHotSpotMouseHoverEntity) And (FHotSpotMouseHoverEntity.ActionStyle = UixAdvancedToolbarHotSpotActionStyleChevron) Then
    Begin
      FPen.SetColor(argbDarkGray);
      oGraphics.DrawRectangle(FPen, aChevronRectangle);

      FPen.SetColor(argbWhiteSmoke);
      oGraphics.DrawRectangle(FPen, ContractRectangle(aChevronRectangle, 1));

      If Assigned(FHotSpotMouseDownEntity) And FHotSpotMouseDownEntity.IsEquivalent(FHotSpotMouseHoverEntity) Then
      Begin
        aButtonTopBackgroundColour := $FFFEF7D5;
        aButtonBottomBackgroundColour := $FFFFB31C;
      End
      Else
      Begin
        aButtonTopBackgroundColour := $FFFEF7D5;
        aButtonBottomBackgroundColour := $FFFFD048;
      End;

      oLinearGradientBrush := TGPLinearGradientBrush.Create(aChevronRectangle, aButtonTopBackgroundColour, aButtonBottomBackgroundColour, LinearGradientModeVertical);
      Try
        oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

        oGraphics.FillRectangle(oLinearGradientBrush, aChevronRectangle);
      Finally
        oLinearGradientBrush.Free;
      End;
    End;

    FPen.SetColor(argbDarkGray);

    SetLength(aChevronPointArray, 3);

    aChevronPointArray[0] := MakePoint(aChevronRectangle.X + 5.0, aChevronRectangle.Y + 4);
    aChevronPointArray[1] := MakePoint(aChevronRectangle.X + 7.0, aChevronRectangle.Y + 6);
    aChevronPointArray[2] := MakePoint(aChevronRectangle.X + 5.0, aChevronRectangle.Y + 8);

    oGraphics.DrawLines(FPen, PGPPointF(aChevronPointArray), 3);

    aChevronPointArray[0] := MakePoint(aChevronRectangle.X + 6.0, aChevronRectangle.Y + 4);
    aChevronPointArray[1] := MakePoint(aChevronRectangle.X + 8.0, aChevronRectangle.Y + 6);
    aChevronPointArray[2] := MakePoint(aChevronRectangle.X + 6.0, aChevronRectangle.Y + 8);

    oGraphics.DrawLines(FPen, PGPPointF(aChevronPointArray), 3);

    aChevronPointArray[0] := MakePoint(aChevronRectangle.X + 9.0, aChevronRectangle.Y + 4);
    aChevronPointArray[1] := MakePoint(aChevronRectangle.X + 11.0, aChevronRectangle.Y + 6);
    aChevronPointArray[2] := MakePoint(aChevronRectangle.X + 9.0, aChevronRectangle.Y + 8);

    oGraphics.DrawLines(FPen, PGPPointF(aChevronPointArray), 3);

    aChevronPointArray[0] := MakePoint(aChevronRectangle.X + 10.0, aChevronRectangle.Y + 4);
    aChevronPointArray[1] := MakePoint(aChevronRectangle.X + 12.0, aChevronRectangle.Y + 6);
    aChevronPointArray[2] := MakePoint(aChevronRectangle.X + 10.0, aChevronRectangle.Y + 8);

    oGraphics.DrawLines(FPen, PGPPointF(aChevronPointArray), 3);

    FSolidBrush.SetColor(argbBlack);
    oGraphics.FillPath(FSolidBrush, FChevronPath);

    oHotSpotEntity := AddNewHotSpotEntity;
    oHotSpotEntity.HotSpot.SetRegion(aChevronRectangle);
    oHotSpotEntity.ActionStyle := UixAdvancedToolbarHotSpotActionStyleChevron;
  End;
End;


Function TUixAdvancedToolbar.RowHeight : Integer;
Begin
  If PresentationEntity.SpecificationEntity.ShowImages Then
  Begin
    Result := PresentationEntity.SpecificationEntity.IconSize + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);

    If PresentationEntity.SpecificationEntity.ShowCaption And (PresentationEntity.SpecificationEntity.CaptionOrientation = UixAdvancedToolbarCaptionOrientationBottom) Then
      Inc(Result, UixAdvancedToolbarTextHeight + PresentationEntity.SpecificationEntity.ButtonPadding);
  End
  Else
  Begin
    Result := UixAdvancedToolbarTextHeight + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
  End;
End;


Procedure TUixAdvancedToolbar.CalculateMetrics(Const oGraphics : TGdiPlusExtendedGraphics);
Var
  aGroupCaptionTextRectangle : TGPRect;
  aGroupCaptionLayoutRect : TGPRectF;
  aGroupCaptionBoundingRect : TGPRectF;
  aGroupCaptionBorderRectangle : TGPRect;
  aGroupDropdownArrowRectangle : TGPRect;

  aConfigurationRectangle : TGPRect;

  aButtonRectangle : TGPRect;
  aButtonClickRectangle : TGPRect;
  aButtonImageRectangle : TGPRect;
  aButtonCaptionRectangle : TGPRect;
  aButtonCaptionLayoutRect : TGPRectF;
  aButtonCaptionBoundingRect : TGPRectF;
  aButtonDropDownBorderRectangle : TGPRect;
  aButtonDropDownArrowRectangle : TGPRect;
  aControlBorderRectangle : TGPRect;
  aControlEditControlRectangle : TGPRect;
  aControlDropDownBorderRectangle : TGPRect;
  aControlDropDownArrowRectangle : TGPRect;

  bEncounteredVisibleGroup : Boolean;
  bEncounteredVisibleSection : Boolean;

  iRequiredRowHeight : Integer;
  iRequiredClientHeight : Integer;
  iRequiredClientWidth : Integer;
  iRequiredGroupExpandedWidth : Integer;
  iRequiredButtonWidth : Integer;
  iMaximumRequiredButtonWidth : Integer;
  iClientWidthShortage : Integer;
  iClientRectangleXOffset : Integer;
  iClientRectangleYOffset : Integer;
  iExpectedButtonWidth : Integer;

  iExecutionGroupEntityIndex : Integer;
  iExecutionSectionEntityIndex : Integer;
  iExecutionComponentEntityIndex : Integer;

  iVisibleControlComponentEntityIndex : Integer;

  oPresentationControlEntity : TUixAdvancedToolbarPresentationControlEntity;

  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oExecutionSectionEntity : TUixAdvancedToolbarExecutionSectionEntity;
  oExecutionComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
  oExecutionControlEntity : TUixAdvancedToolbarExecutionControlEntity;

  oExecutionLastVisibleButtonEntity : TUixAdvancedToolbarExecutionButtonEntity;

  sGroupCaption : String;
  sButtonCaption : String;
Begin
  FRequiredRowCount := 1;

  FClientRectangle.X := ClientRect.Left + PresentationEntity.MarginLeft;
  FClientRectangle.Y := ClientRect.Top + PresentationEntity.MarginTop;
  FClientRectangle.Width := ((ClientRect.Right - PresentationEntity.MarginRight) - (ClientRect.Left + PresentationEntity.MarginLeft)) - 1;
  FClientRectangle.Height := ((ClientRect.Bottom - PresentationEntity.MarginBottom) - (ClientRect.Top + PresentationEntity.MarginTop)) - 1;

  iRequiredRowHeight := RowHeight - 1;

  FExecutionEntity.ChevronIsVisible := False;

  iMaximumRequiredButtonWidth := 0;
  If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
  Begin
    For iExecutionGroupEntityIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
    Begin
      oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];

      For iExecutionSectionEntityIndex := 0 To oExecutionGroupEntity.SectionEntityList.Count - 1 Do
      Begin
        oExecutionSectionEntity := oExecutionGroupEntity.SectionEntityList[iExecutionSectionEntityIndex];

        For iExecutionComponentEntityIndex := 0 To oExecutionSectionEntity.ComponentEntityList.Count - 1 Do
        Begin
          oExecutionComponentEntity := oExecutionSectionEntity.ComponentEntityList[iExecutionComponentEntityIndex];

          If oExecutionComponentEntity.HasButtonEntity Then
          Begin
            If PresentationEntity.SpecificationEntity.ShowCaption And oExecutionComponentEntity.PresentationComponentEntity.ShowCaption Then
            Begin
              sButtonCaption := oExecutionComponentEntity.PresentationComponentEntity.Caption;

              aButtonCaptionLayoutRect.X := 0;
              aButtonCaptionLayoutRect.Y := 0;
              aButtonCaptionLayoutRect.Width := 5000;
              aButtonCaptionLayoutRect.Height := 5000;

              oGraphics.MeasureString(sButtonCaption, Length(sButtonCaption), FCaptionFont.ProduceFont, aButtonCaptionLayoutRect, FCaptionStringFormat.ProduceStringFormat, aButtonCaptionBoundingRect);

              Case PresentationEntity.SpecificationEntity.CaptionOrientation Of
                UixAdvancedToolbarCaptionOrientationRight :
                Begin
                  iRequiredButtonWidth := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);

                  If PresentationEntity.SpecificationEntity.ShowImages Then
                    Inc(iRequiredButtonWidth, PresentationEntity.SpecificationEntity.IconSize + PresentationEntity.SpecificationEntity.ButtonPadding);
                End;

                UixAdvancedToolbarCaptionOrientationBottom :
                Begin
                  If PresentationEntity.SpecificationEntity.ShowImages And oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                    iRequiredButtonWidth := IntegerMax(PresentationEntity.SpecificationEntity.IconSize, RealCeiling(aButtonCaptionBoundingRect.Width)) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding)
                  Else
                    iRequiredButtonWidth := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
                End;
              Else
                Raise EFslException.Create(Self, 'Paint', 'Unexpected caption orientation.');
              End;
            End
            Else
            Begin
              iRequiredButtonWidth := PresentationEntity.SpecificationEntity.IconSize + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
            End;

            iMaximumRequiredButtonWidth := IntegerMax(iMaximumRequiredButtonWidth, iRequiredButtonWidth);

            oExecutionComponentEntity.ButtonEntity.RequiredWidth := iMaximumRequiredButtonWidth;
          End;
        End;
      End;
    End;
  End;

  FVisibleControlComponentEntityList.Clear;

  iRequiredClientWidth := 0;
  bEncounteredVisibleGroup := False;
  For iExecutionGroupEntityIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
  Begin
    oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];
    oExecutionGroupEntity.CollapsedToCaption := False;
    oExecutionGroupEntity.HiddenWithinChevron := False;
    oExecutionGroupEntity.ContainsVisibleComponents := False;

    iRequiredGroupExpandedWidth := 0;

    sGroupCaption := oExecutionGroupEntity.PresentationGroupEntity.Caption;

    aGroupCaptionLayoutRect.X := 0;
    aGroupCaptionLayoutRect.Y := 0;
    aGroupCaptionLayoutRect.Width := 5000;
    aGroupCaptionLayoutRect.Height := 5000;

    oGraphics.MeasureString(sGroupCaption, Length(sGroupCaption), FCaptionFont.ProduceFont, aGroupCaptionLayoutRect, FCaptionStringFormat.ProduceStringFormat, aGroupCaptionBoundingRect);

    oExecutionGroupEntity.RequiredCollapsedWidth := (2 * PresentationEntity.SpecificationEntity.ButtonPadding) + RealCeiling(aGroupCaptionBoundingRect.Width) + UixAdvancedToolbarControlDropDownWidth;

    bEncounteredVisibleSection := False;
    For iExecutionSectionEntityIndex := 0 To oExecutionGroupEntity.SectionEntityList.Count - 1 Do
    Begin
      oExecutionSectionEntity := oExecutionGroupEntity.SectionEntityList[iExecutionSectionEntityIndex];
      oExecutionSectionEntity.ContainsVisibleComponents := False;

      oExecutionLastVisibleButtonEntity := Nil;
      For iExecutionComponentEntityIndex := 0 To oExecutionSectionEntity.ComponentEntityList.Count - 1 Do
      Begin
        oExecutionComponentEntity := oExecutionSectionEntity.ComponentEntityList[iExecutionComponentEntityIndex];

        If oExecutionComponentEntity.PresentationComponentEntity.IsVisible Then
        Begin
          If oExecutionComponentEntity.HasButtonEntity Then
          Begin
            If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
            Begin
              iRequiredButtonWidth := iMaximumRequiredButtonWidth;
            End
            Else If PresentationEntity.SpecificationEntity.ShowCaption And oExecutionComponentEntity.PresentationComponentEntity.ShowCaption Then
            Begin
              sButtonCaption := oExecutionComponentEntity.PresentationComponentEntity.Caption;

              aButtonCaptionLayoutRect.X := 0;
              aButtonCaptionLayoutRect.Y := 0;
              aButtonCaptionLayoutRect.Width := 5000;
              aButtonCaptionLayoutRect.Height := 5000;

              oGraphics.MeasureString(sButtonCaption, Length(sButtonCaption), FCaptionFont.ProduceFont, aButtonCaptionLayoutRect, FCaptionStringFormat.ProduceStringFormat, aButtonCaptionBoundingRect);

              Case PresentationEntity.SpecificationEntity.CaptionOrientation Of
                UixAdvancedToolbarCaptionOrientationRight :
                Begin
                  iRequiredButtonWidth := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);

                  If PresentationEntity.SpecificationEntity.ShowImages And oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                    Inc(iRequiredButtonWidth, PresentationEntity.SpecificationEntity.IconSize + PresentationEntity.SpecificationEntity.ButtonPadding);
                End;

                UixAdvancedToolbarCaptionOrientationBottom :
                Begin
                  If PresentationEntity.SpecificationEntity.ShowImages And oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                    iRequiredButtonWidth := IntegerMax(PresentationEntity.SpecificationEntity.IconSize, RealCeiling(aButtonCaptionBoundingRect.Width)) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding)
                  Else
                    iRequiredButtonWidth := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
                End;
              Else
                Raise EFslException.Create(Self, 'Paint', 'Unexpected caption orientation.');
              End;
            End
            Else
            Begin
              If Not PresentationEntity.SpecificationEntity.ShowImages Then
                Raise EFslException.Create(Self, 'Paint', 'Cannot show button with no captions or images.');

              iRequiredButtonWidth := PresentationEntity.SpecificationEntity.IconSize + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
            End;

            If Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownHandler) Or Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownPopup) Then
              Inc(iRequiredButtonWidth, UixAdvancedToolbarControlDropDownWidth);

            Inc(iRequiredGroupExpandedWidth, iRequiredButtonWidth);

            If Not Assigned(oExecutionLastVisibleButtonEntity) Then
              oExecutionComponentEntity.ButtonEntity.IsFirstInSection := True;

            oExecutionLastVisibleButtonEntity := oExecutionComponentEntity.ButtonEntity;

            oExecutionComponentEntity.ButtonEntity.RequiredWidth := iRequiredButtonWidth;
          End
          Else If oExecutionComponentEntity.HasControlEntity Then
          Begin
            FVisibleControlGroupEntityList.Add(oExecutionGroupEntity.Link);
            FVisibleControlComponentEntityList.Add(oExecutionComponentEntity.Link);

            oExecutionComponentEntity.ControlEntity.BorderWidth := oExecutionComponentEntity.ControlEntity.PresentationControlEntity.MaximumWidth;

            Inc(iRequiredGroupExpandedWidth, oExecutionComponentEntity.ControlEntity.BorderWidth);
          End
          Else
          Begin
            Raise EFslException.Create(Self, 'CalculateMetrics', 'Unexpected component type.');
          End;

          oExecutionSectionEntity.ContainsVisibleComponents := True;
          oExecutionGroupEntity.ContainsVisibleComponents := True;
        End;
      End;

      If Assigned(oExecutionLastVisibleButtonEntity) Then
        oExecutionLastVisibleButtonEntity.IsLastInSection := True;

      If oExecutionSectionEntity.ContainsVisibleComponents Then
      Begin
        // Section separator.
        If bEncounteredVisibleSection Then
          Inc(iRequiredGroupExpandedWidth, UixAdvancedToolbarSectionPadding);

        bEncounteredVisibleSection := True;
      End;
    End;

    If oExecutionGroupEntity.ContainsVisibleComponents Then
    Begin
      oExecutionGroupEntity.RequiredExpandedWidth := iRequiredGroupExpandedWidth;

      // Group separator.
      If bEncounteredVisibleGroup Then
        Inc(iRequiredGroupExpandedWidth, (2 * UixAdvancedToolbarGroupPadding) + 1);

      bEncounteredVisibleGroup := True;

      Inc(iRequiredClientWidth, iRequiredGroupExpandedWidth);
    End;
  End;

  If Not PresentationEntity.SpecificationEntity.AllowWrapping And (iRequiredClientWidth > FClientRectangle.Width) Then
  Begin
    // We need to ensure the horizontal footprint of the toolbar does not exceed the client width.
    iClientWidthShortage := iRequiredClientWidth - FClientRectangle.Width;

    // Reduce the size of visible controls to minimum widths.
    iVisibleControlComponentEntityIndex := FVisibleControlComponentEntityList.Count - 1;
    While (iClientWidthShortage > 0) And FVisibleControlComponentEntityList.ExistsByIndex(iVisibleControlComponentEntityIndex) Do
    Begin
      oExecutionGroupEntity := FVisibleControlGroupEntityList[iVisibleControlComponentEntityIndex];
      oExecutionComponentEntity := FVisibleControlComponentEntityList[iVisibleControlComponentEntityIndex];
      oExecutionControlEntity := oExecutionComponentEntity.ControlEntity;
      oPresentationControlEntity := oExecutionControlEntity.PresentationControlEntity;

      If oExecutionComponentEntity.ControlEntity.BorderWidth > oPresentationControlEntity.MinimumWidth Then
      Begin
        If (oExecutionComponentEntity.ControlEntity.BorderWidth - oPresentationControlEntity.MinimumWidth) > iClientWidthShortage Then
        Begin
          oExecutionComponentEntity.ControlEntity.BorderWidth := oExecutionComponentEntity.ControlEntity.BorderWidth - iClientWidthShortage;
          oExecutionGroupEntity.RequiredExpandedWidth := oExecutionGroupEntity.RequiredExpandedWidth - iClientWidthShortage;

          iClientWidthShortage := 0;
        End
        Else
        Begin
          Dec(iClientWidthShortage, oExecutionComponentEntity.ControlEntity.BorderWidth - oPresentationControlEntity.MinimumWidth);

          oExecutionGroupEntity.RequiredExpandedWidth := oExecutionGroupEntity.RequiredExpandedWidth - (oExecutionComponentEntity.ControlEntity.BorderWidth - oPresentationControlEntity.MinimumWidth);

          oExecutionComponentEntity.ControlEntity.BorderWidth := oPresentationControlEntity.MinimumWidth;
        End;
      End;

      Dec(iVisibleControlComponentEntityIndex);
    End;

    // Collapse groups to conserve space.
    iExecutionGroupEntityIndex := FExecutionEntity.GroupEntityList.Count - 1;
    While (iClientWidthShortage > 0) And FExecutionEntity.GroupEntityList.ExistsByIndex(iExecutionGroupEntityIndex) Do
    Begin
      oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];
      oExecutionGroupEntity.CollapsedToCaption := True;

      Dec(iClientWidthShortage, oExecutionGroupEntity.RequiredExpandedWidth - oExecutionGroupEntity.RequiredCollapsedWidth);
      Dec(iExecutionGroupEntityIndex);
    End;

    // Move groups into chevron button to conserve space.
    iExecutionGroupEntityIndex := FExecutionEntity.GroupEntityList.Count - 1;
    While (iClientWidthShortage > 0) And FExecutionEntity.GroupEntityList.ExistsByIndex(iExecutionGroupEntityIndex) Do
    Begin
      oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];
      oExecutionGroupEntity.HiddenWithinChevron := True;

      If Not FExecutionEntity.ChevronIsVisible Then
      Begin
        FExecutionEntity.ChevronIsVisible := True;

        Inc(iClientWidthShortage, UixAdvancedToolbarChevronWidth + 6);
      End;

      Dec(iClientWidthShortage, oExecutionGroupEntity.RequiredCollapsedWidth + (2 * UixAdvancedToolbarGroupPadding) + 1);
      Dec(iExecutionGroupEntityIndex);
    End;
  End;

  iClientRectangleXOffset := 0;
  iClientRectangleYOffset := 0;
  bEncounteredVisibleGroup := False;

  If PresentationEntity.SpecificationEntity.ShowConfiguration
  Then
  Begin
    FExecutionEntity.HasConfiguration := True;

    aConfigurationRectangle.X := FClientRectangle.X + iClientRectangleXOffset;
    aConfigurationRectangle.Y := FClientRectangle.Y + iClientRectangleYOffset;
    aConfigurationRectangle.Height := RowHeight Div 2;
    aConfigurationRectangle.Width := RowHeight Div 2;

    FExecutionEntity.ConfigurationRectangle := aConfigurationRectangle;

    Inc(iClientRectangleXOffset, 15);
  End;

  For iExecutionGroupEntityIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
  Begin
    oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];

    If oExecutionGroupEntity.ContainsVisibleComponents Then
    Begin
      oExecutionGroupEntity.HasSeparator := False;

      If bEncounteredVisibleGroup Then
      Begin
        If Not PresentationEntity.SpecificationEntity.AllowWrapping Or ((iClientRectangleXOffset + (2 * UixAdvancedToolbarGroupPadding) + 1) < FClientRectangle.Width) Then
        Begin
          Inc(iClientRectangleXOffset, UixAdvancedToolbarGroupPadding);

          oExecutionGroupEntity.HasSeparator := True;
          oExecutionGroupEntity.SeparatorX := FClientRectangle.X + iClientRectangleXOffset;
          oExecutionGroupEntity.SeparatorY := FClientRectangle.Y + iClientRectangleYOffset;

          Inc(iClientRectangleXOffset, UixAdvancedToolbarGroupPadding + 1);
        End
        Else
        Begin
          iClientRectangleXOffset := 0;

          Inc(iClientRectangleYOffset, RowHeight + PresentationEntity.SpecificationEntity.ButtonPadding);
          Inc(FRequiredRowCount);
        End;
      End;

      bEncounteredVisibleGroup := True;

      If oExecutionGroupEntity.CollapsedToCaption Then
      Begin
        aGroupCaptionBorderRectangle.X := FClientRectangle.X + iClientRectangleXOffset;
        aGroupCaptionBorderRectangle.Y := FClientRectangle.Y;
        aGroupCaptionBorderRectangle.Width := oExecutionGroupEntity.RequiredCollapsedWidth;
        aGroupCaptionBorderRectangle.Height := iRequiredRowHeight;

        aGroupCaptionTextRectangle.X := aGroupCaptionBorderRectangle.X + PresentationEntity.SpecificationEntity.ButtonPadding;
        aGroupCaptionTextRectangle.Y := aGroupCaptionBorderRectangle.Y;
        aGroupCaptionTextRectangle.Width := aGroupCaptionBorderRectangle.Width - (2 * PresentationEntity.SpecificationEntity.ButtonPadding) - UixAdvancedToolbarControlDropDownWidth;
        aGroupCaptionTextRectangle.Height := aGroupCaptionBorderRectangle.Height;

        aGroupDropDownArrowRectangle.X := aGroupCaptionBorderRectangle.X + aGroupCaptionBorderRectangle.Width - UixAdvancedToolbarControlDropDownWidth + ((UixAdvancedToolbarControlDropDownWidth - UixAdvancedToolbarControlDropDownArrowWidth) Div 2);
        aGroupDropDownArrowRectangle.Y := aGroupCaptionBorderRectangle.Y + ((aGroupCaptionBorderRectangle.Height - UixAdvancedToolbarControlDropDownArrowHeight) Div 2);
        aGroupDropDownArrowRectangle.Width := UixAdvancedToolbarControlDropDownArrowWidth;
        aGroupDropDownArrowRectangle.Height := UixAdvancedToolbarControlDropDownArrowHeight;

        oExecutionGroupEntity.CaptionTextRectangle := aGroupCaptionTextRectangle;
        oExecutionGroupEntity.CaptionBorderRectangle := aGroupCaptionBorderRectangle;
        oExecutionGroupEntity.CaptionBackgroundRectangle := ContractRectangle(aGroupCaptionBorderRectangle, 1);
        oExecutionGroupEntity.CaptionDropDownArrowRectangle := aGroupDropDownArrowRectangle;

        Inc(iClientRectangleXOffset, aGroupCaptionBorderRectangle.Width);
      End
      Else
      Begin
        bEncounteredVisibleSection := False;
        For iExecutionSectionEntityIndex := 0 To oExecutionGroupEntity.SectionEntityList.Count - 1 Do
        Begin
          oExecutionSectionEntity := oExecutionGroupEntity.SectionEntityList[iExecutionSectionEntityIndex];

          If oExecutionSectionEntity.ContainsVisibleComponents Then
          Begin
            If bEncounteredVisibleSection Then
            Begin
              If Not PresentationEntity.SpecificationEntity.AllowWrapping Or ((iClientRectangleXOffset + UixAdvancedToolbarSectionPadding) < FClientRectangle.Width) Then
              Begin
                Inc(iClientRectangleXOffset, UixAdvancedToolbarSectionPadding);
              End
              Else
              Begin
                iClientRectangleXOffset := 0;

                Inc(iClientRectangleYOffset, RowHeight + PresentationEntity.SpecificationEntity.ButtonPadding);
                Inc(FRequiredRowCount);
              End;
            End;

            bEncounteredVisibleSection := True;
          End;

          For iExecutionComponentEntityIndex := 0 To oExecutionSectionEntity.ComponentEntityList.Count - 1 Do
          Begin
            oExecutionComponentEntity := oExecutionSectionEntity.ComponentEntityList[iExecutionComponentEntityIndex];

            If oExecutionComponentEntity.PresentationComponentEntity.IsVisible Then
            Begin
              If oExecutionComponentEntity.HasButtonEntity Then
              Begin
                If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
                Begin
                  iExpectedButtonWidth := iMaximumRequiredButtonWidth;

                  If Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownHandler) Or Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownPopup) Then
                    Inc(iExpectedButtonWidth, UixAdvancedToolbarControlDropDownWidth);
                End
                Else
                Begin
                  iExpectedButtonWidth := oExecutionComponentEntity.ButtonEntity.RequiredWidth;
                End;

                If PresentationEntity.SpecificationEntity.AllowWrapping And ((iClientRectangleXOffset + iExpectedButtonWidth) > FClientRectangle.Width) Then
                Begin
                  iClientRectangleXOffset := 0;

                  Inc(iClientRectangleYOffset, RowHeight + PresentationEntity.SpecificationEntity.ButtonPadding);
                  Inc(FRequiredRowCount);
                End;

                aButtonRectangle.X := FClientRectangle.X + iClientRectangleXOffset;
                aButtonRectangle.Y := FClientRectangle.Y + iClientRectangleYOffset;

                aButtonRectangle.Height := iRequiredRowHeight;

                If oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                Begin
                  aButtonImageRectangle.Y := FClientRectangle.Y + iClientRectangleYOffset + PresentationEntity.SpecificationEntity.ButtonPadding;
                  aButtonImageRectangle.Width := PresentationEntity.SpecificationEntity.IconSize;
                  aButtonImageRectangle.Height := aButtonImageRectangle.Width;
                End
                Else
                Begin
                  aButtonImageRectangle := CreateEmptyRectangle;
                End;

                If PresentationEntity.SpecificationEntity.ShowCaption And oExecutionComponentEntity.PresentationComponentEntity.ShowCaption Then
                Begin
                  sButtonCaption := oExecutionComponentEntity.PresentationComponentEntity.Caption;

                  aButtonCaptionLayoutRect.X := 0;
                  aButtonCaptionLayoutRect.Y := 0;
                  aButtonCaptionLayoutRect.Width := 5000;
                  aButtonCaptionLayoutRect.Height := 5000;

                  If oExecutionComponentEntity.PresentationComponentEntity.ShowCaption Then
                    oGraphics.MeasureString(sButtonCaption, Length(sButtonCaption), FCaptionFont.ProduceFont, aButtonCaptionLayoutRect, FCaptionStringFormat.ProduceStringFormat, aButtonCaptionBoundingRect)
                  Else
                    aButtonCaptionBoundingRect := CreateEmptyRectangleF;

                  Case PresentationEntity.SpecificationEntity.CaptionOrientation Of
                    UixAdvancedToolbarCaptionOrientationRight :
                    Begin
                      If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
                      Begin
                        aButtonRectangle.Width := iMaximumRequiredButtonWidth;
                      End
                      Else
                      Begin
                        aButtonRectangle.Width := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);

                        If PresentationEntity.SpecificationEntity.ShowImages And oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                          aButtonRectangle.Width := aButtonRectangle.Width + PresentationEntity.SpecificationEntity.IconSize + PresentationEntity.SpecificationEntity.ButtonPadding;
                      End;

                      If oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                      Begin
                        aButtonImageRectangle.X := aButtonRectangle.X + PresentationEntity.SpecificationEntity.ButtonPadding;

                        aButtonCaptionRectangle.X := aButtonImageRectangle.X + aButtonImageRectangle.Width + PresentationEntity.SpecificationEntity.ButtonPadding;
                      End
                      Else
                      Begin
                        aButtonCaptionRectangle.X := aButtonRectangle.X + PresentationEntity.SpecificationEntity.ButtonPadding;
                      End;

                      aButtonCaptionRectangle.Height := UixAdvancedToolbarTextHeight;
                      aButtonCaptionRectangle.Width := RealCeiling(aButtonCaptionBoundingRect.Width);
                      aButtonCaptionRectangle.Y := aButtonRectangle.Y + (aButtonRectangle.Height - aButtonCaptionRectangle.Height) Div 2 + 1;
                    End;

                    UixAdvancedToolbarCaptionOrientationBottom :
                    Begin
                      If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
                      Begin
                        aButtonRectangle.Width := iMaximumRequiredButtonWidth;
                      End
                      Else
                      Begin
                        If PresentationEntity.SpecificationEntity.ShowImages And oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                          aButtonRectangle.Width := IntegerMax(PresentationEntity.SpecificationEntity.IconSize, RealCeiling(aButtonCaptionBoundingRect.Width)) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding)
                        Else
                          aButtonRectangle.Width := RealCeiling(aButtonCaptionBoundingRect.Width) + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);
                      End;

                      If oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.RequiresIcon Then
                        aButtonImageRectangle.X := aButtonRectangle.X + (aButtonRectangle.Width - aButtonImageRectangle.Width) Div 2;

                      aButtonCaptionRectangle.Y := aButtonRectangle.Y + aButtonRectangle.Height - UixAdvancedToolbarTextHeight - PresentationEntity.SpecificationEntity.ButtonPadding + 1;
                      aButtonCaptionRectangle.Height := UixAdvancedToolbarTextHeight;
                      aButtonCaptionRectangle.Width := RealCeiling(aButtonCaptionBoundingRect.Width);
                      aButtonCaptionRectangle.X := aButtonRectangle.X + (aButtonRectangle.Width - aButtonCaptionRectangle.Width) Div 2;
                    End;
                  Else
                    Raise EFslException.Create(Self, 'Paint', 'Unexpected caption orientation.');
                  End;
                End
                Else
                Begin
                  If PresentationEntity.SpecificationEntity.UseUniformButtonWidths Then
                    aButtonRectangle.Width := iMaximumRequiredButtonWidth
                  Else
                    aButtonRectangle.Width := PresentationEntity.SpecificationEntity.IconSize + (2 * PresentationEntity.SpecificationEntity.ButtonPadding);

                  aButtonImageRectangle.X := aButtonRectangle.X + PresentationEntity.SpecificationEntity.ButtonPadding;
                  aButtonImageRectangle.Y := FClientRectangle.Y + iClientRectangleYOffset + ((iRequiredRowHeight - aButtonImageRectangle.Height - (2 * PresentationEntity.SpecificationEntity.ButtonPadding)) Div 2) + PresentationEntity.SpecificationEntity.ButtonPadding;
                End;

                Inc(iClientRectangleXOffset, aButtonRectangle.Width);

                aButtonClickRectangle := aButtonRectangle;

                If Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownHandler) Or Assigned(oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity.DropDownPopup) Then
                Begin
                  aButtonDropDownBorderRectangle.X := aButtonRectangle.X + aButtonRectangle.Width;
                  aButtonDropDownBorderRectangle.Y := aButtonRectangle.Y;
                  aButtonDropDownBorderRectangle.Height := aButtonRectangle.Height;
                  aButtonDropDownBorderRectangle.Width := UixAdvancedToolbarControlDropDownWidth;

                  aButtonDropDownArrowRectangle.X := aButtonDropDownBorderRectangle.X + ((UixAdvancedToolbarControlDropDownWidth - UixAdvancedToolbarControlDropDownArrowWidth) Div 2);
                  aButtonDropDownArrowRectangle.Y := aButtonDropDownBorderRectangle.Y + ((aButtonDropDownBorderRectangle.Height - UixAdvancedToolbarControlDropDownArrowHeight) Div 2);
                  aButtonDropDownArrowRectangle.Width := UixAdvancedToolbarControlDropDownArrowWidth;
                  aButtonDropDownArrowRectangle.Height := UixAdvancedToolbarControlDropDownArrowHeight;

                  oExecutionComponentEntity.ButtonEntity.DropDownArrowRectangle := aButtonDropDownArrowRectangle;
                  oExecutionComponentEntity.ButtonEntity.DropDownBorderRectangle := aButtonDropDownBorderRectangle;
                  oExecutionComponentEntity.ButtonEntity.DropDownBackgroundRectangle := ContractRectangle(aButtonDropDownBorderRectangle, 1);

                  Inc(iClientRectangleXOffset, UixAdvancedToolbarControlDropDownWidth);

                  aButtonRectangle.Width := aButtonRectangle.Width + aButtonDropDownBorderRectangle.Width;
                End;

                oExecutionComponentEntity.ButtonEntity.BorderRectangle := aButtonRectangle;
                oExecutionComponentEntity.ButtonEntity.BackgroundRectangle := ContractRectangle(aButtonRectangle, 1);
                oExecutionComponentEntity.ButtonEntity.ButtonClickRectangle := aButtonClickRectangle;
                oExecutionComponentEntity.ButtonEntity.ButtonImageRectangle := aButtonImageRectangle;
                oExecutionComponentEntity.ButtonEntity.ButtonCaptionRectangle := aButtonCaptionRectangle;
              End
              Else If oExecutionComponentEntity.HasControlEntity Then
              Begin
                If (iClientRectangleXOffset + oExecutionComponentEntity.ControlEntity.BorderWidth) > FClientRectangle.Width Then
                Begin
                  If Not PresentationEntity.SpecificationEntity.AllowWrapping Or ((iClientRectangleXOffset + oExecutionComponentEntity.ControlEntity.PresentationControlEntity.MinimumWidth) < FClientRectangle.Width) Then
                  Begin
                    oExecutionComponentEntity.ControlEntity.BorderWidth := FClientRectangle.Width - iClientRectangleXOffset;
                  End
                  Else
                  Begin
                    iClientRectangleXOffset := 0;

                    Inc(iClientRectangleYOffset, RowHeight + PresentationEntity.SpecificationEntity.ButtonPadding);
                    Inc(FRequiredRowCount);
                  End;
                End;

                aControlBorderRectangle.X := FClientRectangle.X + iClientRectangleXOffset;
                aControlBorderRectangle.Y := FClientRectangle.Y + iClientRectangleYOffset;
                aControlBorderRectangle.Width := oExecutionComponentEntity.ControlEntity.BorderWidth;
                aControlBorderRectangle.Height := iRequiredRowHeight;

                aControlEditControlRectangle.X := aControlBorderRectangle.X;
                aControlEditControlRectangle.Width := aControlBorderRectangle.Width;

                aControlEditControlRectangle.Height := oExecutionComponentEntity.PresentationComponentEntity.ControlEntity.Control.Height;
                aControlEditControlRectangle.Y := 1 + aControlBorderRectangle.Y + (aControlBorderRectangle.Height Div 2) - (aControlEditControlRectangle.Height Div 2);

                oExecutionComponentEntity.ControlEntity.BorderRectangle := aControlBorderRectangle;
                oExecutionComponentEntity.ControlEntity.EditControlRectangle := aControlEditControlRectangle;

                Inc(iClientRectangleXOffset, aControlBorderRectangle.Width);
              End
              Else
              Begin
                Raise EFslException.Create(Self, 'CalculateMetrics', 'Unexpected component type.');
              End;
            End;
          End;
        End;
      End;
    End;
  End;
End;


Function TUixAdvancedToolbar.ActivateShortCutKey(Const aUixKey: TUixKey): Boolean;
Var
  iExecutionGroupEntityIndex : Integer;
  iExecutionSectionEntityIndex : Integer;
  iExecutionComponentEntityIndex : Integer;

  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oExecutionSectionEntity : TUixAdvancedToolbarExecutionSectionEntity;
  oExecutionComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
  oExecutionButtonEntity : TUixAdvancedToolbarExecutionButtonEntity;

  oPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;

  oPackage : TUixAdvancedToolbarPresentationButtonEventPackage;
Begin
  Inherited;

  iExecutionGroupEntityIndex := 0;
  Result := False;
  While Not Result And FExecutionEntity.GroupEntityList.ExistsByIndex(iExecutionGroupEntityIndex) Do
  Begin
    oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];

    iExecutionSectionEntityIndex := 0;
    While Not Result And oExecutionGroupEntity.SectionEntityList.ExistsByIndex(iExecutionSectionEntityIndex) Do
    Begin
      oExecutionSectionEntity := oExecutionGroupEntity.SectionEntityList[iExecutionSectionEntityIndex];

      iExecutionComponentEntityIndex := 0;
      While Not Result And oExecutionSectionEntity.ComponentEntityList.ExistsByIndex(iExecutionComponentEntityIndex) Do
      Begin
        oExecutionComponentEntity := oExecutionSectionEntity.ComponentEntityList[iExecutionComponentEntityIndex];

        If oExecutionComponentEntity.HasButtonEntity Then
        Begin
          oExecutionButtonEntity := oExecutionComponentEntity.ButtonEntity;
          oPresentationButtonEntity := oExecutionButtonEntity.PresentationButtonEntity;

          Result := (aUixKey = oPresentationButtonEntity.ShortCutKey);

          If Result Then
          Begin
            If Assigned(oPresentationButtonEntity.ClickHandler) Then
            Begin
              oPresentationButtonEntity.ClickHandler(oPresentationButtonEntity);
            End
            Else If Assigned(oPresentationButtonEntity.DropDownHandler) Then
            Begin
              oPackage := TUixAdvancedToolbarPresentationButtonEventPackage.Create;
              Try
                oPackage.ButtonEntity := oPresentationButtonEntity.Link;
                oPackage.Toolbar := Self;
                oPackage.ComponentRectangle := oExecutionButtonEntity.BorderRectangle;

                oPresentationButtonEntity.DropDownHandler(oPackage)
              Finally
                oPackage.Free;
              End;
            End
            Else If Assigned(oPresentationButtonEntity.DropDownPopup) Then
            Begin
              oPresentationButtonEntity.DropDownPopup.Popup(0, 0);
            End;
          End;
        End;

        Inc(iExecutionComponentEntityIndex);
      End;

      Inc(iExecutionSectionEntityIndex);
    End;

    Inc(iExecutionGroupEntityIndex);
  End;
End;


Procedure TUixAdvancedToolbar.MouseMove(aShiftState : TShiftState; iX, iY : Integer);
Var
  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
Begin
  Inherited;

  oHotSpotEntity := DetermineHotSpotEntityAtPoint(iX, iY);

  If Assigned(FHotSpotMouseDownEntity) Then
  Begin
    If Assigned(oHotSpotEntity) And oHotSpotEntity.IsEquivalent(FHotSpotMouseDownEntity) Then
    Begin
      // We are hovering over the down item.

      ApplyMouseHoverHotSpot(oHotSpotEntity);

      Invalidate;
    End
    Else If Assigned(FHotSpotMouseHoverEntity) Then
    Begin
      // We are not hovering over the down item.

      ApplyMouseHoverHotSpot(Nil);

      Invalidate;
    End;
  End
  Else If Assigned(oHotSpotEntity) Then
  Begin
    If Not Assigned(FHotSpotMouseHoverEntity) Or Not FHotSpotMouseHoverEntity.IsEquivalent(oHotSpotEntity) Then
    Begin
      ApplyMouseHoverHotSpot(oHotSpotEntity);

      Invalidate;
    End;
  End
  Else If Assigned(FHotSpotMouseHoverEntity) Then
  Begin
    ApplyMouseHoverHotSpot(Nil);

    Invalidate;
  End;
End;


Procedure TUixAdvancedToolbar.MouseDown(aButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer);
Var
  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
  oPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;

  aClientDropDownPoint : TPoint;
  aScreenDropDownPoint : TPoint;
Begin
  Inherited;

  If aButton = mbLeft Then
  Begin
    oHotSpotEntity := DetermineHotSpotEntityAtPoint(iX, iY);

    If Assigned(oHotSpotEntity) Then
    Begin
      Case oHotSpotEntity.ActionStyle Of
        UixAdvancedToolbarHotSpotActionStyleGroup,
        UixAdvancedToolbarHotSpotActionStyleChevron :
        Begin
          ApplyMouseDownHotSpot(oHotSpotEntity);

          Invalidate;
        End;

        UixAdvancedToolbarHotSpotActionStyleButton,
        UixAdvancedToolbarHotSpotActionStyleButtonDropDown :
        Begin
          If oHotSpotEntity.ComponentEntity.PresentationComponentEntity.IsEnabled Then
          Begin
            ApplyMouseDownHotSpot(oHotSpotEntity);

            Invalidate;
          End;
        End;

        UixAdvancedToolbarHotSpotActionStyleConfiguration:
        Begin
          ApplyMouseDownHotSpot(oHotSpotEntity);

          Invalidate;
        End;
      Else
        Error('MouseDown', 'Unexpected action style.');
      End;
    End;
  End
  Else If aButton = mbRight Then
  Begin
    oHotSpotEntity := DetermineHotSpotEntityAtPoint(iX, iY);

    If Assigned(oHotSpotEntity) And oHotSpotEntity.HasComponentEntity And oHotSpotEntity.ComponentEntity.HasButtonEntity And oHotSpotEntity.ComponentEntity.PresentationComponentEntity.IsEnabled Then
    Begin
      oPresentationButtonEntity := oHotSpotEntity.ComponentEntity.ButtonEntity.PresentationButtonEntity;

      If Assigned(oPresentationButtonEntity.RightDropDownPopup) Then
      Begin
        aClientDropDownPoint.X := iX;
        aClientDropDownPoint.Y := iY;
        aScreenDropDownPoint := ClientToScreen(aClientDropDownPoint);

        oPresentationButtonEntity.RightDropDownPopup.Popup(aScreenDropDownPoint.X, aScreenDropDownPoint.Y);
      End;

      Invalidate;
    End;
  End;
End;


Procedure TUixAdvancedToolbar.MouseUp(aButton : TMouseButton; aShiftState : TShiftState; iX, iY : Integer);
Var
  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;

  oPresentationButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
  oGroupForm : TUixAdvancedToolbarGroupForm;

  oChevronForm : TUixAdvancedToolbarChevronForm;

  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  iExecutionGroupIndex : Integer;

  aClientDropDownPoint : TPoint;
  aScreenDropDownPoint : TPoint;
  aScreenLocationRect : TGPRect;

  oPackage : TUixAdvancedToolbarPresentationButtonEventPackage;
Begin
  Inherited;

  If aButton = mbLeft Then
  Begin
    oHotSpotEntity := DetermineHotSpotEntityAtPoint(iX, iY);

    If Assigned(oHotSpotEntity) And Assigned(FHotSpotMouseDownEntity) And FHotSpotMouseDownEntity.IsEquivalent(oHotSpotEntity) Then
    Begin
      Case oHotSpotEntity.ActionStyle Of
        UixAdvancedToolbarHotSpotActionStyleGroup :
        Begin
          oExecutionGroupEntity := oHotSpotEntity.GroupEntity;
          aScreenLocationRect := GdiPlusClientToScreen(oExecutionGroupEntity.CaptionBackgroundRectangle);

          oGroupForm := TUixAdvancedToolbarGroupForm.CreateNew(Self);
          oGroupForm.Color := Color;
          oGroupForm.Top := aScreenLocationRect.Y + aScreenLocationRect.Height + 3;
          oGroupForm.Left := aScreenLocationRect.X;
          oGroupForm.ClientWidth := IntegerMin(400, oExecutionGroupEntity.RequiredExpandedWidth + PresentationEntity.MarginLeft + PresentationEntity.MarginRight + 1);

          oGroupForm.ToolBar.PresentationEntity.SpecificationEntity.IconSize := PresentationEntity.SpecificationEntity.IconSize;
          oGroupForm.ToolBar.PresentationEntity.SpecificationEntity.ShowCaption := PresentationEntity.SpecificationEntity.ShowCaption;
          oGroupForm.ToolBar.PresentationEntity.SpecificationEntity.UseFuchsiaTransparency := PresentationEntity.SpecificationEntity.UseFuchsiaTransparency;
          oGroupForm.ToolBar.PresentationEntity.SpecificationEntity.CaptionOrientation := PresentationEntity.SpecificationEntity.CaptionOrientation;
          oGroupForm.ToolBar.PresentationEntity.MarginLeft := PresentationEntity.MarginLeft;
          oGroupForm.ToolBar.PresentationEntity.MarginRight := PresentationEntity.MarginRight;
          oGroupForm.ToolBar.PresentationEntity.MarginTop := PresentationEntity.MarginTop;
          oGroupForm.ToolBar.PresentationEntity.MarginBottom := PresentationEntity.MarginBottom;

          oGroupForm.ToolBar.PresentationEntity.GroupEntityList.Clear;
          oGroupForm.ToolBar.PresentationEntity.GroupEntityList.Add(oExecutionGroupEntity.PresentationGroupEntity.Link);

          oGroupForm.AnimateShow;
        End;

        UixAdvancedToolbarHotSpotActionStyleConfiguration :
        Begin
          If Assigned(PresentationEntity.ClickHandler) Then
          Begin
            PresentationEntity.ClickHandler(PresentationEntity);
          End
          Else If Assigned(PresentationEntity.DropDownPopup) Then
          Begin
            aClientDropDownPoint.X := FExecutionEntity.ConfigurationRectangle.X;
            aClientDropDownPoint.Y := FExecutionEntity.ConfigurationRectangle.Y + FExecutionEntity.ConfigurationRectangle.Height;
            aScreenDropDownPoint := ClientToScreen(aClientDropDownPoint);

            PresentationEntity.DropDownPopup.Popup(aScreenDropDownPoint.X, aScreenDropDownPoint.Y);
          End
          Else If Assigned(PresentationEntity.DropDownHandler) Then
          Begin
            oPackage := TUixAdvancedToolbarPresentationButtonEventPackage.Create;
            Try
              oPackage.Toolbar := Self;
              oPackage.ComponentRectangle := FExecutionEntity.ConfigurationRectangle;

              PresentationEntity.DropDownHandler(oPackage)
            Finally
              oPackage.Free;
            End;
          End;
        End;

        UixAdvancedToolbarHotSpotActionStyleButton :
        Begin
          oPresentationButtonEntity := oHotSpotEntity.ComponentEntity.ButtonEntity.PresentationButtonEntity;

          If Assigned(oPresentationButtonEntity.ClickHandler) Then
          Begin
            oPresentationButtonEntity.ClickHandler(oPresentationButtonEntity);
          End
          Else If Assigned(oPresentationButtonEntity.DropDownPopup) Then
          Begin
            aClientDropDownPoint.X := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.X;
            aClientDropDownPoint.Y := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.Y + oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.Height;
            aScreenDropDownPoint := ClientToScreen(aClientDropDownPoint);

            oPresentationButtonEntity.DropDownPopup.Popup(aScreenDropDownPoint.X, aScreenDropDownPoint.Y);
          End
          Else If Assigned(oPresentationButtonEntity.DropDownHandler) Then
          Begin
            oPackage := TUixAdvancedToolbarPresentationButtonEventPackage.Create;
            Try
              oPackage.Toolbar := Self;
              oPackage.ButtonEntity := oPresentationButtonEntity.Link;
              oPackage.ComponentRectangle := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle;

              oPresentationButtonEntity.DropDownHandler(oPackage)
            Finally
              oPackage.Free;
            End;
          End;
        End;

        UixAdvancedToolbarHotSpotActionStyleButtonDropDown :
        Begin
          oPresentationButtonEntity := oHotSpotEntity.ComponentEntity.ButtonEntity.PresentationButtonEntity;

          If Assigned(oPresentationButtonEntity.DropDownPopup) Then
          Begin
            aClientDropDownPoint.X := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.X;
            aClientDropDownPoint.Y := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.Y + oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle.Height;
            aScreenDropDownPoint := ClientToScreen(aClientDropDownPoint);

            oPresentationButtonEntity.DropDownPopup.Popup(aScreenDropDownPoint.X, aScreenDropDownPoint.Y);
          End
          Else If Assigned(oPresentationButtonEntity.DropDownHandler) Then
          Begin
            oPackage := TUixAdvancedToolbarPresentationButtonEventPackage.Create;
            Try
              oPackage.Toolbar := Self;
              oPackage.ButtonEntity := oPresentationButtonEntity.Link;
              oPackage.ComponentRectangle := oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle;

              oPresentationButtonEntity.DropDownHandler(oPackage)
            Finally
              oPackage.Free;
            End;
          End;
        End;

        UixAdvancedToolbarHotSpotActionStyleChevron :
        Begin
          aScreenLocationRect := GdiPlusClientToScreen(ClientRectangle);

          oChevronForm := TUixAdvancedToolbarChevronForm.CreateNew(Self);
          oChevronForm.Color := Color;
          oChevronForm.Top := aScreenLocationRect.Y + aScreenLocationRect.Height;
          oChevronForm.Left := aScreenLocationRect.X + aScreenLocationRect.Width - 20;

          For iExecutionGroupIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
          Begin
            oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupIndex];

            If oExecutionGroupEntity.HiddenWithinChevron Then
              oChevronForm.PresentationEntity.GroupEntityList.Add(oExecutionGroupEntity.PresentationGroupEntity.Link);
          End;

          oChevronForm.AnimateShow;
        End;
      Else
        Error('MouseUp', 'Unexpected action style.');
      End;
    End;
  End;

  ApplyMouseDownHotSpot(Nil);

  Invalidate;
End;


Procedure TUixAdvancedToolbar.Resize;
Begin
  Inherited;

  AdjustHeight;

  Invalidate;
End;


Procedure TUixAdvancedToolbar.AdjustHeight;
Var
  iRequiredClientHeight : Integer;
Begin
  If PresentationEntity.SpecificationEntity.AllowWrapping And (Align In [alTop, alBottom, alNone]) Then
  Begin
    iRequiredClientHeight := RecommendedHeight;

    If iRequiredClientHeight <> Height Then
      Height := iRequiredClientHeight;
  End;
End;


Procedure TUixAdvancedToolbar.CMMouseLeave(Var aMessage : TMessage);
Begin
  ApplyMouseHoverHotSpot(Nil);

  Invalidate;
End;


Procedure TUixAdvancedToolbar.Execute;
Var
  iPresentationGroupEntityIndex : Integer;
  iPresentationSectionEntityIndex : Integer;
  iPresentationComponentEntityIndex : Integer;

  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oExecutionSectionEntity : TUixAdvancedToolbarExecutionSectionEntity;
  oExecutionComponentEntity : TUixAdvancedToolbarExecutionComponentEntity;
  oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
  oPresentationSectionEntity : TUixAdvancedToolbarPresentationSectionEntity;
  oPresentationComponentEntity : TUixAdvancedToolbarPresentationComponentEntity;
Begin
  FExecutionEntity.GroupEntityList.Clear;
  FPresentationExecutionButtonEntityMatch.Clear;

  For iPresentationGroupEntityIndex := 0 To PresentationEntity.GroupEntityList.Count - 1 Do
  Begin
    oPresentationGroupEntity := PresentationEntity.GroupEntityList[iPresentationGroupEntityIndex];

    oExecutionGroupEntity := TUixAdvancedToolbarExecutionGroupEntity.Create;
    oExecutionGroupEntity.PresentationGroupEntity := oPresentationGroupEntity.Link;
    FExecutionEntity.GroupEntityList.Add(oExecutionGroupEntity);

    For iPresentationSectionEntityIndex := 0 To oPresentationGroupEntity.SectionEntityList.Count - 1 Do
    Begin
      oPresentationSectionEntity := oPresentationGroupEntity.SectionEntityList[iPresentationSectionEntityIndex];

      oExecutionSectionEntity := TUixAdvancedToolbarExecutionSectionEntity.Create;
      oExecutionSectionEntity.PresentationSectionEntity := oPresentationSectionEntity.Link;
      oExecutionGroupEntity.SectionEntityList.Add(oExecutionSectionEntity);

      For iPresentationComponentEntityIndex := 0 To oPresentationSectionEntity.ComponentEntityList.Count - 1 Do
      Begin
        oPresentationComponentEntity := oPresentationSectionEntity.ComponentEntityList[iPresentationComponentEntityIndex];

        oExecutionComponentEntity := TUixAdvancedToolbarExecutionComponentEntity.Create;
        oExecutionComponentEntity.PresentationComponentEntity := oPresentationComponentEntity.Link;
        oExecutionSectionEntity.ComponentEntityList.Add(oExecutionComponentEntity);

        If oPresentationComponentEntity.HasButtonEntity Then
        Begin
          oExecutionComponentEntity.HasButtonEntity := True;
          oExecutionComponentEntity.ButtonEntity.PresentationButtonEntity := oPresentationComponentEntity.ButtonEntity.Link;

          FPresentationExecutionButtonEntityMatch.Add(oPresentationComponentEntity.ButtonEntity.Link, oExecutionComponentEntity.ButtonEntity.Link);
        End
        Else If oPresentationComponentEntity.HasControlEntity Then
        Begin
          oExecutionComponentEntity.HasControlEntity := True;
          oExecutionComponentEntity.ControlEntity.PresentationControlEntity := oPresentationComponentEntity.ControlEntity.Link;

          Assert(Condition(Assigned(oPresentationComponentEntity.ControlEntity.Control), 'Execute', 'oPresentationComponentEntity.ControlEntity.Control must be assigned.'));
        End
        Else
        Begin
          Raise EFslException.Create(Self, 'Execute', 'Unexpected component type.');
        End;
      End;
    End;
  End;

  If Not PresentationEntity.SpecificationEntity.AllowWrapping Then
    Height := RecommendedHeight;

  Invalidate;
End;


Function TUixAdvancedToolbar.RecommendedHeight : Integer;
Var
  oGraphics : TGdiPlusExtendedGraphics;

  iRequiredRowCount : Integer;
Begin
  If PresentationEntity.SpecificationEntity.AllowWrapping Then
  Begin
    oGraphics := TGdiPlusExtendedGraphics.Create(Handle, False);
    Try
      CalculateMetrics(oGraphics)
    Finally
      oGraphics.Free;
    End;

    iRequiredRowCount := FRequiredRowCount;
  End
  Else
  Begin
    iRequiredRowCount := 1;
  End;

  Result := (iRequiredRowCount * RowHeight) + ((iRequiredRowCount - 1) * PresentationEntity.SpecificationEntity.ButtonPadding);

  Inc(Result, PresentationEntity.MarginTop + PresentationEntity.MarginBottom);
End;


Function TUixAdvancedToolbar.RecommendedWidth : Integer;
Var
  iExecutionGroupEntityIndex : Integer;
  oExecutionGroupEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oGraphics : TGdiPlusExtendedGraphics;
Begin
  oGraphics := TGdiPlusExtendedGraphics.Create(Handle, False);
  Try
    CalculateMetrics(oGraphics)
  Finally
    oGraphics.Free;
  End;

  Result := 0;
  For iExecutionGroupEntityIndex := 0 To FExecutionEntity.GroupEntityList.Count - 1 Do
  Begin
    oExecutionGroupEntity := FExecutionEntity.GroupEntityList[iExecutionGroupEntityIndex];

    If oExecutionGroupEntity.ContainsVisibleComponents Then
    Begin
      Inc(Result, oExecutionGroupEntity.RequiredExpandedWidth);

      If oExecutionGroupEntity.HasSeparator Then
      Begin
        Inc(Result, UixAdvancedToolbarGroupPadding * 2);
        Inc(Result, 1); // ???
      End;
    End;
  End;

  Inc(Result, PresentationEntity.MarginLeft + PresentationEntity.MarginRight);
  Inc(Result, 1); // ???
End;


Function TUixAdvancedToolbar.AddNewHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
Begin
  Result := TUixAdvancedToolbarHotSpotEntity.Create;
  FHotSpotEntityList.Add(Result);
End;


Function TUixAdvancedToolbar.DetermineHotSpotEntityAtPoint(Const iX, iY : Integer) : TUixAdvancedToolbarHotSpotEntity;
Var
  iHotSpotEntityIndex : Integer;

  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
Begin
  Result := Nil;

  iHotSpotEntityIndex := FHotSpotEntityList.Count - 1;

  While Not Assigned(Result) And FHotSpotEntityList.ExistsByIndex(iHotSpotEntityIndex) Do
  Begin
    oHotSpotEntity := FHotSpotEntityList[iHotSpotEntityIndex];

    If oHotSpotEntity.HotSpot.PointInRegion(iX, iY) Then
      Result := oHotSpotEntity;

    Dec(iHotSpotEntityIndex);
  End;
End;


Procedure TUixAdvancedToolbar.ApplyMouseDownHotSpot(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity);
Begin
  Assert(Not Assigned(oHotSpotEntity) Or Invariants('ApplyMouseDownHotSpot', oHotSpotEntity, TUixAdvancedToolbarHotSpotEntity, 'oHotSpotEntity'));

  FHotSpotMouseDownEntity.Free;
  FHotSpotMouseDownEntity := oHotSpotEntity.Link;
End;


Procedure TUixAdvancedToolbar.ApplyMouseHoverHotSpot(Const oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity);
Begin
  Assert(Not Assigned(oHotSpotEntity) Or Invariants('ApplyMouseHoverHotSpot', oHotSpotEntity, TUixAdvancedToolbarHotSpotEntity, 'oHotSpotEntity'));

  FHotSpotMouseHoverEntity.Free;
  FHotSpotMouseHoverEntity := oHotSpotEntity.Link;
End;


Function TUixAdvancedToolbar.GetPresentationEntity : TUixAdvancedToolbarPresentationEntity;
Begin
  Assert(Invariants('GetPresentationEntity', FPresentationEntity, TUixAdvancedToolbarPresentationEntity, 'FPresentationEntity'));

  Result := FPresentationEntity;
End;


Procedure TUixAdvancedToolbar.SetPresentationEntity(Const Value : TUixAdvancedToolbarPresentationEntity);
Begin
  Assert(Invariants('SetPresentationEntity', Value, TUixAdvancedToolbarPresentationEntity, 'Value'));

  FPresentationEntity.Free;
  FPresentationEntity := Value;
End;


Procedure TUixAdvancedToolbar.CMHintShow(Var aMessage: TCMHintShow);
Var
  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;
  oButtonEntity : TUixAdvancedToolbarPresentationButtonEntity;
  sHint : String;
Begin
  oHotSpotEntity := DetermineHotSpotEntityAtPoint(aMessage.HintInfo.CursorPos.X, aMessage.HintInfo.CursorPos.Y);

  If Assigned(oHotSpotEntity) And oHotSpotEntity.HasComponentEntity And oHotSpotEntity.ComponentEntity.HasButtonEntity Then
  Begin
    oButtonEntity := oHotSpotEntity.ComponentEntity.ButtonEntity.PresentationButtonEntity;

    If oHotSpotEntity.ComponentEntity.PresentationComponentEntity.Hint <> '' Then
      sHint := oHotSpotEntity.ComponentEntity.PresentationComponentEntity.Hint
    Else
      sHint := oHotSpotEntity.ComponentEntity.PresentationComponentEntity.Caption;

    If oButtonEntity.ShortCutKey <> kk_Null Then
      StringAppend(sHint, StringFormat('(%s)', [UixKeyToString(oButtonEntity.ShortCutKey)]), ' ');

    aMessage.HintInfo.HintStr := sHint;
    aMessage.HintInfo.CursorRect := GDIPlusRectToVCLRect(oHotSpotEntity.ComponentEntity.ButtonEntity.BorderRectangle);
    aMessage.Result := 0;
  End
  Else
  Begin
    aMessage.Result := 1;
  End;
End;


Procedure TUixAdvancedToolbar.Refresh;
Begin
  AdjustHeight;

  Invalidate;
End;


Constructor TUixAdvancedToolbarGroupForm.CreateNew(oOwner: TComponent; iDummy: Integer);
Begin
  Inherited;

  FormStyle := fsStayOnTop;
  KeyPreview := True;

  FToolBar := TUixAdvancedToolbar.Create(Self);
  FToolBar.Parent := Self;
  FToolBar.AlignTop;
  FToolBar.PresentationEntity.SpecificationEntity.AllowWrapping := True;

  FLabel := TUixLabel.Create(Self);
  FLabel.Parent := Self;
  FLabel.AlignBottom;
  FLabel.VerticalAlignCenter;
  FLabel.HorizontalAlignCenter;
  FLabel.Height := 18;
  FLabel.Transparent := False;
  FLabel.AutoSize := False;
  FLabel.Font.Name := 'Tahoma';
  FLabel.Font.Color := clBlack;
  FLabel.Font.Style := [fsBold];

  OnPaint := PaintHandler;
End;


Destructor TUixAdvancedToolbarGroupForm.Destroy;
Begin
  Inherited;
End;


Procedure TUixAdvancedToolbarGroupForm.CreateParams(Var aParams: TCreateParams);
Const
  CS_DROPSHADOW = $00020000;
Begin
  Inherited;

  aParams.Style := WS_POPUP Or WS_CLIPCHILDREN;
  aParams.WindowClass.Style := aParams.WindowClass.Style Or CS_DROPSHADOW;
End;


Procedure TUixAdvancedToolbarGroupForm.CMDeactivate(Var Message: TCMDeactivate);
Begin
  AnimateClose;
  Close;
End;


Procedure TUixAdvancedToolbarGroupForm.PaintHandler(oSender: TObject);
Var
  pRegion : HRGN;
Begin
  Canvas.Brush.Color := clDkGray;
  Canvas.Brush.Style := bsSolid;

  pRegion := CreateRectRgn(0, 0, 1, 1);
  GetWindowRgn(Handle, pRegion);
  Try
    FrameRgn(Canvas.Handle, pRegion, Canvas.Brush.Handle, 1, 1);
  Finally
    DeleteObject(pRegion);
  End;
End;


Procedure TUixAdvancedToolbarGroupForm.AdjustClientRect(Var Rect: TRect);
Begin
  Inherited;

  // Allow for the custom drawn region border
  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := Rect.Right - 2;
  Rect.Bottom := Rect.Bottom - 2;
End;


Procedure TUixAdvancedToolbarGroupForm.AnimateShow;
Var
  pRegion : HRGN;
Begin
  FToolbar.Execute;

  Width := ClientWidth + 3;
  Height := FToolbar.RecommendedHeight + 3 + FLabel.Height;

  FLabel.Caption := FToolBar.PresentationEntity.GroupEntityList[0].Caption;

  pRegion := CreateRoundRectRgn(0, 0, Width, Height, 6, 1);
  SetWindowRgn(Handle, pRegion, True);
  DeleteObject(pRegion);

  AnimateWindow(Handle, 50, AW_BLEND Or AW_ACTIVATE);
  Visible := True;
End;


Procedure TUixAdvancedToolbarGroupForm.AnimateClose;
Begin
  AnimateWindow(Handle, 100, AW_BLEND Or AW_HIDE);
  Visible := False;
End;


Procedure TUixAdvancedToolbarGroupForm.WMPrint(Var aMessage: TMessage);
Begin
  PaintTo(HDC(aMessage.WParam), 0, 0);
End;


Constructor TUixAdvancedToolbarChevronForm.CreateNew(oOwner: TComponent; iDummy: Integer);
Begin
  Inherited;

  FPresentationEntity := TUixAdvancedToolbarPresentationEntity.Create;

  OnPaint := PaintHandler;
End;


Destructor TUixAdvancedToolbarChevronForm.Destroy;
Begin
  FPresentationEntity.Free;

  Inherited;
End;


Procedure TUixAdvancedToolbarChevronForm.CreateParams(Var aParams: TCreateParams);
Const
  CS_DROPSHADOW = $00020000;
Begin
  Inherited;

  aParams.Style := WS_POPUP Or WS_CLIPCHILDREN;
  aParams.WindowClass.Style := aParams.WindowClass.Style Or CS_DROPSHADOW;
End;


Procedure TUixAdvancedToolbarChevronForm.AdjustClientRect(Var Rect: TRect);
Begin
  Inherited;

  // Allow for the custom drawn region border
  Rect.Left := 1;
  Rect.Top := 1;
  Rect.Right := Rect.Right - 2;
  Rect.Bottom := Rect.Bottom - 2;
End;


Procedure TUixAdvancedToolbarChevronForm.AnimateClose;
Begin
  AnimateWindow(Handle, 100, AW_BLEND Or AW_HIDE);
  Visible := False;
End;


Procedure TUixAdvancedToolbarChevronForm.AnimateShow;
Var
  pRegion : HRGN;

  oToolBar : TUixAdvancedToolbar;
  oGroupEntity : TUixAdvancedToolbarPresentationGroupEntity;
  iGroupIndex : Integer;
  iRequiredHeight : Integer;
  iRequiredWidth : Integer;
Begin
  iRequiredHeight := 0;
  iRequiredWidth := 0;
  For iGroupIndex := 0 To PresentationEntity.GroupEntityList.Count - 1 Do
  Begin
    oGroupEntity := PresentationEntity.GroupEntityList[iGroupIndex];

    oToolBar := TUixAdvancedToolbar.Create(Self);
    oToolBar.Parent := Self;
    oToolBar.AlignTop;
    oToolBar.ShuffleBottom;
    oToolBar.Height := 100;

    oToolBar.PresentationEntity.MarginLeft := 5;
    oToolBar.PresentationEntity.MarginRight := 5;
    oToolBar.PresentationEntity.MarginTop := 3;
    oToolBar.PresentationEntity.MarginBottom := 2;

    oToolBar.PresentationEntity.GroupEntityList.Add(oGroupEntity.Link);
    oToolBar.Execute;

    Inc(iRequiredHeight, oToolBar.RecommendedHeight);
    iRequiredWidth := IntegerMax(iRequiredWidth, oToolBar.RecommendedWidth);
  End;

  Width := iRequiredWidth + 5;
  Height := iRequiredHeight + 5;

  pRegion := CreateRoundRectRgn(0, 0, Width, Height, 6, 1);
  SetWindowRgn(Handle, pRegion, True);
  DeleteObject(pRegion);

  AnimateWindow(Handle, 50, AW_BLEND Or AW_ACTIVATE);
  Visible := True;
End;


Procedure TUixAdvancedToolbarChevronForm.CMDeactivate(Var Message: TCMDeactivate);
Begin
  AnimateClose;
  Close;
End;


Procedure TUixAdvancedToolbarChevronForm.PaintHandler(oSender: TObject);
Var
  pRegion : HRGN;
Begin
  Canvas.Brush.Color := clDkGray;
  Canvas.Brush.Style := bsSolid;

  pRegion := CreateRectRgn(0, 0, 1, 1);
  GetWindowRgn(Handle, pRegion);
  Try
    FrameRgn(Canvas.Handle, pRegion, Canvas.Brush.Handle, 1, 1);
  Finally
    DeleteObject(pRegion);
  End;
End;


Procedure TUixAdvancedToolbarChevronForm.WMPrint(Var aMessage: TMessage);
Begin
  PaintTo(HDC(aMessage.WParam), 0, 0);
End;


Procedure TUixAdvancedToolbar.ClickButtonEntity(Const oButtonEntity : TUixAdvancedToolbarPresentationButtonEntity);
Var
  aClientDropDownPoint : TPoint;
  aScreenDropDownPoint : TPoint;
  oPackage : TUixAdvancedToolbarPresentationButtonEventPackage;
  oExecutionButton : TUixAdvancedToolbarExecutionButtonEntity;
Begin
  Assert(Invariants('ClickButtonEntity', oButtonEntity, TUixAdvancedToolbarPresentationButtonEntity, 'oButtonEntity'));

  If Visible And Enabled Then
  Begin
    oExecutionButton := TUixAdvancedToolbarExecutionButtonEntity(FPresentationExecutionButtonEntityMatch.GetValueByKey(oButtonEntity));

    If Assigned(oButtonEntity.ClickHandler) Then
    Begin
      oButtonEntity.ClickHandler(oButtonEntity);
    End
    Else If Assigned(oButtonEntity.DropDownPopup) Then
    Begin
      aClientDropDownPoint.X := oExecutionButton.BorderRectangle.X;
      aClientDropDownPoint.Y := oExecutionButton.BorderRectangle.Y + oExecutionButton.BorderRectangle.Height;
      aScreenDropDownPoint := ClientToScreen(aClientDropDownPoint);

      oButtonEntity.DropDownPopup.Popup(aScreenDropDownPoint.X, aScreenDropDownPoint.Y);
    End
    Else If Assigned(oButtonEntity.DropDownHandler) Then
    Begin
      oPackage := TUixAdvancedToolbarPresentationButtonEventPackage.Create;
      Try
        oPackage.Toolbar := Self;
        oPackage.ButtonEntity := oButtonEntity.Link;
        oPackage.ComponentRectangle := oExecutionButton.BorderRectangle;

        oButtonEntity.DropDownHandler(oPackage)
      Finally
        oPackage.Free;
      End;
    End;

    Invalidate;
  End;
End;


Procedure TUixAdvancedToolbar.SortByCaption(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity);
Var
  iGroupIndex : Integer;
  iSectionIndex : Integer;

  oGroupExecutionEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oSectionExecutionEntity : TUixAdvancedToolbarExecutionSectionEntity;
Begin
  iGroupIndex := FExecutionEntity.GroupEntityList.IndexByPresentationGroupEntity(oPresentationGroupEntity);

  If FExecutionEntity.GroupEntityList.ExistsByIndex(iGroupIndex) Then
  Begin
    oGroupExecutionEntity := FExecutionEntity.GroupEntityList[iGroupIndex];

    For iSectionIndex := 0 To oGroupExecutionEntity.SectionEntityList.Count - 1 Do
    Begin
      oSectionExecutionEntity := oGroupExecutionEntity.SectionEntityList[iSectionIndex];

      oSectionExecutionEntity.ComponentEntityList.SortedByCaption;
      oSectionExecutionEntity.ComponentEntityList.UnSorted;
    End;
  End;
End;


Procedure TUixAdvancedToolbar.SortByPositionIndex(Const oPresentationGroupEntity : TUixAdvancedToolbarPresentationGroupEntity);
Var
  iGroupIndex : Integer;
  iSectionIndex : Integer;

  oGroupExecutionEntity : TUixAdvancedToolbarExecutionGroupEntity;
  oSectionExecutionEntity : TUixAdvancedToolbarExecutionSectionEntity;
Begin
  iGroupIndex := FExecutionEntity.GroupEntityList.IndexByPresentationGroupEntity(oPresentationGroupEntity);

  If FExecutionEntity.GroupEntityList.ExistsByIndex(iGroupIndex) Then
  Begin
    oGroupExecutionEntity := FExecutionEntity.GroupEntityList[iGroupIndex];

    For iSectionIndex := 0 To oGroupExecutionEntity.SectionEntityList.Count - 1 Do
    Begin
      oSectionExecutionEntity := oGroupExecutionEntity.SectionEntityList[iSectionIndex];

      oSectionExecutionEntity.ComponentEntityList.SortedByPositionIndex;
      oSectionExecutionEntity.ComponentEntityList.UnSorted;
    End;
  End;
End;


Function TUixAdvancedToolbar.ConfigurationHotspotContainsPoint(Const iX, iY: Integer): Boolean;
Var
  oHotSpotEntity : TUixAdvancedToolbarHotSpotEntity;

  aScreenPoint : TPoint;
  aClientPoint : TPoint;
Begin
  Inherited;

  Result := Assigned(FConfigurationHotspotEntity);

  If Result Then
  Begin
    aScreenPoint.X := iX;
    aScreenPoint.Y := iY;

    aClientPoint := ScreenToClient(aScreenPoint);

    oHotSpotEntity := DetermineHotSpotEntityAtPoint(aClientPoint.X, aClientPoint.Y);

    Result := Assigned(oHotSpotEntity) And FConfigurationHotSpotEntity.IsEquivalent(oHotSpotEntity);
  End;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.RequiresIcon: Boolean;
Begin
  Result := Assigned(FCustomIconEvent) Or HasBitmapImage;
End;


{ TUixAdvancedToolbarPresentationButtonEventPackage }


Constructor TUixAdvancedToolbarPresentationButtonEventPackage.Create;
Begin
  Inherited;

  FButtonEntity := Nil;
End;


Destructor TUixAdvancedToolbarPresentationButtonEventPackage.Destroy;
Begin
  FButtonEntity.Free;

  Inherited;
End;


Function TUixAdvancedToolbarPresentationButtonEventPackage.GetButtonEntity: TUixAdvancedToolbarPresentationButtonEntity;
Begin
  Result := FButtonEntity;
End;


Function TUixAdvancedToolbarPresentationButtonEventPackage.GetToolbar: TUixAdvancedToolbar;
Begin
  Result := FToolbar;
End;


Function TUixAdvancedToolbarPresentationButtonEventPackage.Link: TUixAdvancedToolbarPresentationButtonEventPackage;
Begin
  Result := TUixAdvancedToolbarPresentationButtonEventPackage(Inherited Link);
End;


Procedure TUixAdvancedToolbarPresentationButtonEventPackage.SetButtonEntity(Const Value: TUixAdvancedToolbarPresentationButtonEntity);
Begin
  FButtonEntity.Free;
  FButtonEntity := Value;
End;


Procedure TUixAdvancedToolbarPresentationButtonEventPackage.SetToolbar(Const Value: TUixAdvancedToolbar);
Begin
  FToolbar := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetIsEnabled: Boolean;
Begin
  Result := ParentComponent.IsEnabled;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetIsEnabled(Const Value: Boolean);
Begin
  ParentComponent.IsEnabled := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetIsVisible: Boolean;
Begin
  Result := ParentComponent.IsVisible;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetIsVisible(Const Value: Boolean);
Begin
  ParentComponent.IsVisible := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetParentComponent: TUixAdvancedToolbarPresentationComponentEntity;
Begin
  Assert(Invariants('GetParentComponent', FParentComponent, TUixAdvancedToolbarPresentationComponentEntity, 'FParentComponent'));

  Result := FParentComponent;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetParentComponent(Const Value: TUixAdvancedToolbarPresentationComponentEntity);
Begin
  Assert(Invariants('SetParentComponent', Value, TUixAdvancedToolbarPresentationComponentEntity, 'Value'));

  FParentComponent := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetHint: String;
Begin
  Result := ParentComponent.Hint;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetHint(Const Value: String);
Begin
  ParentComponent.Hint := Value;
End;


Function TUixAdvancedToolbarPresentationButtonEntity.GetCaption: String;
Begin
  Result := ParentComponent.Caption;
End;


Procedure TUixAdvancedToolbarPresentationButtonEntity.SetCaption(Const Value: String);
Begin
  ParentComponent.Caption := Value;
End;


Const
  BorderColour = argbDkGray;
  TriangleColour = argbDkGray;
  ButtonBackgroundColour = argbWhite;


Const
  aPathFactorArray : Array [0..10] Of Single = (0.1, 0.1, 0.2, 0.2, 1.0, 0.9, 0.7, 0.5, 0.3, 0.3, 0.2);
  aPathPositionArray : Array [0..10] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1);


Constructor TUixAdvancedComboBox.Create(oOwner: TComponent);
Begin
  Inherited;

  DoubleBuffered := True;

  FPen := TGPPen.Create(argbBlack);
  FPen.SetAlignment(PenAlignmentInset);

  FSolidBrush := TGPSolidBrush.Create(argbBlack);

  FDefaultFont := TGPFont.Create('Tahoma', 9);

  FDefaultStringFormat := TGdiPlusStringFormat.Create;
  FDefaultStringFormat.HorizontalAlignmentLeft;
  FDefaultStringFormat.VerticalAlignmentMiddle;
  FDefaultStringFormat.TrimModeEllipsisCharacter;

  CustomBackgoundHandling := True;
  Color := clWhite;
End;


Destructor TUixAdvancedComboBox.Destroy;
Begin
  FDefaultFont.Free;
  FDefaultStringFormat.Free;
  FSolidBrush.Free;
  FPen.Free;

  Inherited;
End;


Procedure TUixAdvancedComboBox.CMMouseEnter(Var aMessage: TMessage);
Begin
  FControlHovering := True;
End;


Procedure TUixAdvancedComboBox.CMMouseLeave(Var aMessage: TMessage);
Begin
  If Not FButtonDown Then
    Color := GDIPlusColourToVCLColour(ButtonBackgroundColour);

  FButtonHovering := False;
  FControlHovering := False;

  If Not FButtonDown Then
    Invalidate;
End;


Procedure TUixAdvancedComboBox.MouseMove(aShiftState: TShiftState; iX, iY: Integer);
Var
  bOldButtonHovering : Boolean;
Begin
  Inherited;

  bOldButtonHovering := FButtonHovering;
  FButtonHovering := RectangleContainsPoint(FButtonRect, iX, iY);

  If bOldButtonHovering <> FButtonHovering Then
    Invalidate;
End;


Procedure TUixAdvancedComboBox.Paint(Const oGraphics: TGdiPlusExtendedGraphics; Const aComboBoxInfo : TComboBoxInfo);
Var
  aClientRect : TGPRect;
  aButtonTriangleRect : TGPRect;
  oLinearGradientBrush : TGPLinearGradientBrush;
  aOriginalButtonRect : TGPRect;
Begin
  aClientRect := VCLRectToGDIPlusRect(ClientRect);

  aOriginalButtonRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcButton);

  FButtonRect.X := aOriginalButtonRect.X;
  FButtonRect.Y := aClientRect.Y;
  FButtonRect.Width := aOriginalButtonRect.Width + 2;
  FButtonRect.Height := aClientRect.Height;

  If FButtonHovering Or FButtonDown Then
  Begin
    oLinearGradientBrush := TGPLinearGradientBrush.Create(FButtonRect, $FFfffdec, $FFffe6a0, LinearGradientModeVertical);
    Try
      oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

      oGraphics.FillRectangle(oLinearGradientBrush, FButtonRect);
    Finally
      oLinearGradientBrush.Free;
    End;
  End
  Else
  Begin
    FSolidBrush.SetColor(ButtonBackgroundColour);
    oGraphics.FillRectangle(FSolidBrush, FButtonRect);
  End;

  FPen.SetColor(BorderColour);
  oGraphics.DrawLine(FPen, FButtonRect.X, FButtonRect.Y, FButtonRect.X, FButtonRect.Y + FButtonRect.Height);

  aButtonTriangleRect.Width := 6;
  aButtonTriangleRect.Height := 3;
  aButtonTriangleRect.X := FButtonRect.X + (FButtonRect.Width Div 2) - (aButtonTriangleRect.Width Div 2);
  aButtonTriangleRect.Y := FButtonRect.Y + (FButtonRect.Height Div 2) - (aButtonTriangleRect.Height Div 2);

  FSolidBrush.SetColor(TriangleColour);
  oGraphics.FillTriangle(FSolidBrush, aButtonTriangleRect);

  FPen.SetColor(BorderColour);

  If RoundedBorders Then
    oGraphics.DrawRoundedRectangle(FPen, aClientRect, 2)
  Else
    oGraphics.DrawValidRectangle(FPen, aClientRect)
End;


Procedure TUixAdvancedComboBox.PaintCustomComboBoxToDC(Const pInputDC: HDC; Const aClippingRectangle: TRect);
Var
  hTemporaryDC : HDC;
  hMemoryDC : HDC;
  hMemoryBitmap : HBITMAP;
  hPreviousBitmap : HBITMAP;
  oMemoryGraphics : TGdiPlusExtendedGraphics;
  aComboBoxInfo : TComboBoxInfo;
  aButtonRect : TGPRect;
  aItemRect : TGPRect;
Begin
  FillChar(aComboBoxInfo, SizeOf(aComboBoxInfo), 0);
  aComboBoxInfo.cbSize := SizeOf(aComboBoxInfo);
  GetComboBoxInfo(Handle, aComboBoxInfo);

  aButtonRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcButton);
  aItemRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcItem);

  If DoubleBuffered Then
  Begin
    hTemporaryDC := GetDC(0);
    Try
      hMemoryBitmap := CreateCompatibleBitmap(hTemporaryDC, ClientRect.Right, ClientRect.Bottom);
    Finally
      ReleaseDC(0, hTemporaryDC);
    End;

    hMemoryDC := CreateCompatibleDC(0);
    hPreviousBitmap := SelectObject(hMemoryDC, hMemoryBitmap);
    Try
      oMemoryGraphics := TGdiPlusExtendedGraphics.Create(hMemoryDC);
      Try
        oMemoryGraphics.SetClip(MakeRect(aClippingRectangle));
        oMemoryGraphics.ExcludeClip(aItemRect);
        oMemoryGraphics.Clear(ColorRefToARGB(ColorToRGB(Color)));

        Paint(oMemoryGraphics, aComboBoxInfo);
      Finally
        oMemoryGraphics.Free;
      End;

      BitBlt(pInputDC, 0, 0, ClientRect.Right, ClientRect.Bottom, hMemoryDC, 0, 0, SRCCOPY);
    Finally
      SelectObject(hMemoryDC, hPreviousBitmap);
      DeleteDC(hMemoryDC);
      DeleteObject(hMemoryBitmap);
    End;
  End
  Else
  Begin
    oMemoryGraphics := TGdiPlusExtendedGraphics.Create(pInputDC);
    Try
      oMemoryGraphics.SetClip(MakeRect(aClippingRectangle));
      oMemoryGraphics.ExcludeClip(aItemRect);
      oMemoryGraphics.Clear(ColorRefToARGB(ColorToRGB(Color)));

      Paint(oMemoryGraphics, aComboBoxInfo);
    Finally
      oMemoryGraphics.Free;
    End;
  End;
End;


Procedure TUixAdvancedComboBox.WMPaint(Var aMessage: TWMPaint);
Var
  hPaintDC : HDC;
  aPaintStructure : TPaintStruct;
  aClippingRect : TRect;

  aComboBoxInfo : TComboBoxInfo;
  aButtonRect : TGPRect;
  aItemRect : TGPRect;
Begin
  FillChar(aComboBoxInfo, SizeOf(aComboBoxInfo), 0);
  aComboBoxInfo.cbSize := SizeOf(aComboBoxInfo);
  GetComboBoxInfo(Handle, aComboBoxInfo);

  aButtonRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcButton);
  aItemRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcItem);

  If (aMessage.DC = 0) Then
  Begin
    hPaintDC := BeginPaint(Handle, aPaintStructure);
    Try
      PaintCustomComboBoxToDC(hPaintDC, aPaintStructure.rcPaint);

      IntersectClipRect(hPaintDC, aItemRect.X, aItemRect.Y, aItemRect.X + aItemRect.Width, aItemRect.y + aItemRect.Height);
      PaintWindow(hPaintDC);
    Finally
      EndPaint(Handle, aPaintStructure)
    End;
  End
  Else
  Begin
    GetClipBox(aMessage.DC, aClippingRect);
    PaintCustomComboBoxToDC(aMessage.DC, aClippingRect);
  End;

  aMessage.Result := 0;
End;


Procedure TUixAdvancedComboBox.CNCommand(Var aMessage: TWMCommand);
Begin
  Inherited;

  {Case aMessage.NotifyCode Of
    CBN_SELCHANGE  : ConsoleWriteLine('CBN_SELCHANGE');
    CBN_DBLCLK     : ConsoleWriteLine('CBN_DBLCLK');
    CBN_SETFOCUS   : ConsoleWriteLine('CBN_SETFOCUS');
    CBN_KILLFOCUS  : ConsoleWriteLine('CBN_KILLFOCUS');
    CBN_EDITCHANGE : ConsoleWriteLine('CBN_EDITCHANGE');
    CBN_EDITUPDATE : ConsoleWriteLine('CBN_EDITUPDATE');
    CBN_DROPDOWN   : ConsoleWriteLine('CBN_DROPDOWN');
    CBN_CLOSEUP    : ConsoleWriteLine('CBN_CLOSEUP');
    CBN_SELENDOK   : ConsoleWriteLine('CBN_SELENDOK');
    CBN_SELENDCANCEL : ConsoleWriteLine('CBN_SELENDCANCEL');
  End;}

  Case aMessage.NotifyCode Of
    CBN_CLOSEUP :
    Begin
      FButtonDown := False;

      If Not FControlHovering Then
        Color := GDIPlusColourToVCLColour(ButtonBackgroundColour);

      Invalidate;
    End;

    CBN_DROPDOWN :
    Begin
      FButtonDown := True;

      If OverrideDropDownWidth And (DropDownWidth > 0) Then
        Perform(CB_SETDROPPEDWIDTH, DropDownWidth, 0);

      Invalidate;
    End;
  End;
End;


Procedure TUixAdvancedComboBox.CNDrawItem(Var aMessage: TWMDrawItem);
Var
  aBufferItemRect : TGPRect;
  aDrawItemStruct : TDrawItemStruct;
  aItemRect : TGPRect;
  oBufferBitmap : TGPBitmap;
  oBufferGraphics : TGdiPlusExtendedGraphics;
  oGraphics : TGdiPlusExtendedGraphics;
Begin
  aDrawItemStruct := aMessage.DrawItemStruct^;
  aItemRect := VCLRectToGDIPlusRect(aDrawItemStruct.rcItem);

  oBufferBitmap := TGPBitmap.Create(aItemRect.Width, aItemRect.Height);
  Try
    oBufferGraphics := TGdiPlusExtendedGraphics.Create(oBufferBitmap);
    Try
      aBufferItemRect.X := 0;
      aBufferItemRect.Y := 0;
      aBufferItemRect.Width := aItemRect.Width;
      aBufferItemRect.Height := aItemRect.Height;

      DrawComboBoxItem(oBufferGraphics, Integer(aDrawItemStruct.itemID), aBufferItemRect, TOwnerDrawState(LongRec(aDrawItemStruct.itemState).Lo));
    Finally
      oBufferGraphics.Free;
    End;

    oGraphics := TGdiPlusExtendedGraphics.Create(aDrawItemStruct.hDC);
    Try
      oGraphics.SetClip(aItemRect);

      oGraphics.DrawImage(oBufferBitmap, MakeRectF(aItemRect), Nil);
    Finally
      oGraphics.Free;
    End;
  Finally
    oBufferBitmap.Free;
  End;
End;


Procedure TUixAdvancedComboBox.CNMeasureItem(Var aMessage: TWMMeasureItem);
Var
  iItemIdentifier : Integer;
Begin
  iItemIdentifier := Integer(aMessage.MeasureItemStruct^.itemID);

  If iItemIdentifier = -1 Then
    aMessage.MeasureItemStruct^.itemHeight := 15
  Else
    aMessage.MeasureItemStruct^.itemHeight := ItemHeight;

  {$R-}

  If (Style = csOwnerDrawVariable) Then
    MeasureItem(iItemIdentifier, Integer(aMessage.MeasureItemStruct^.itemHeight));

  Assert(aMessage.MeasureItemStruct^.itemHeight > 0, 'CNMeasureItem');

  {$R+}
End;


Procedure TUixAdvancedComboBox.DrawComboBoxItem(Const oGraphics: TGdiPlusExtendedGraphics; Const iIndex : Integer; Const aItemRectangle: TGPRect; Const aItemState: TOwnerDrawState);
Var
  oLinearGradientBrush : TGPLinearGradientBrush;
  aFocusRect : TGPRect;
  aPaddingRect : TGPRect;
Begin
  oGraphics.Clear(VCLColourToGDIPlusColour(Color));

  If ((odFocused In aItemState) Or (odSelected In aItemState)) And Not (odComboBoxEdit In aItemState) Then
  Begin
    aFocusRect.X := aItemRectangle.X + 1;
    aFocusRect.Y := aItemRectangle.Y;
    aFocusRect.Width := aItemRectangle.Width - 1;
    aFocusRect.Height := aItemRectangle.Height;

    oLinearGradientBrush := TGPLinearGradientBrush.Create(aFocusRect, $FFffebb5, $FFffd66c, LinearGradientModeVertical);
    Try
      oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

      oGraphics.FillRectangle(oLinearGradientBrush, aFocusRect);
    Finally
      oLinearGradientBrush.Free;
    End;

    FPen.SetColor(argbDarkGray);
    oGraphics.DrawRoundedRectangle(FPen, aFocusRect, 1);
  End;

  aPaddingRect.X := aItemRectangle.X + 2;
  aPaddingRect.Y := aItemRectangle.Y;
  aPaddingRect.Width := aItemRectangle.Width - 2;
  aPaddingRect.Height := aItemRectangle.Height;

  FSolidBrush.SetColor(argbBlack);
  oGraphics.DrawValidatedString(Items[iIndex], FDefaultFont, FDefaultStringFormat.ProduceStringFormat, MakeRectF(aPaddingRect), FSolidBrush);
End;


Procedure TUixAdvancedComboBox.WMEraseBkgnd(Var aMessage: TWMEraseBkgnd);
Begin
  aMessage.Result := 1;
End;


Procedure TUixAdvancedComboBox.WndProc(Var aMessage: TMessage);
Begin
  {$R-}

  Case aMessage.Msg Of
    WM_CTLCOLORMSGBOX..WM_CTLCOLORSTATIC:
    Begin
      If (Style In [csOwnerDrawVariable, csOwnerDrawFixed]) And Not SuppressBackground Then
      Begin
        // Suppress the background painting

        If (Items.Count = 0) Then
        Begin
          Brush.Color := Color;
          Brush.Style := bsSolid;
        End
        Else
        Begin
          Brush.Style := bsClear;
        End;

        aMessage.Result := Brush.Handle;
      End
    End;
  End;

  {$R+}

  Inherited WndProc(aMessage);
End;


Procedure TUixAdvancedComboBox.MouseWheelHandler(Var aMessage: TMessage);
Begin
  If FButtonDown Then
    aMessage.Result := Perform(CM_MOUSEWHEEL, aMessage.WParam, aMessage.LParam)
  Else
    Inherited MouseWheelHandler(aMessage);
End;


Procedure TUixAdvancedComboBox.StyleDropDownEdit;
Begin
  Style := csDropDown;
End;


Procedure TUixAdvancedComboBox.StyleDropDownList;
Begin
  Style := csDropDownList;
End;


Procedure TUixAdvancedComboBox.AddValueArray(Const aValues: Array Of String);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aValues) To High(aValues) Do
    Items.Add(aValues[iLoop]);
End;


Function TUixAdvancedComboBox.GetValue: Integer;
Begin
  Result := ItemIndex;
End;


Procedure TUixAdvancedComboBox.SetValue(Const Value: Integer);
Begin
  ItemIndex := Value;
End;


Function TUixAdvancedComboBox.ValueByIndex(Const iIndex : Integer): String;
Begin
  Result := Items[iIndex];
End;


Function TUixAdvancedComboBox.ExistsByValue(Const sValue : String): Boolean;
Begin
  Result := ExistsByIndex(IndexByValue(sValue));
End;


Function TUixAdvancedComboBox.ExistsByIndex(Const iIndex: Integer): Boolean;
Begin
  Result := (iIndex >= 0) And (iIndex < ItemCount);
End;


{ TUixAdvancedFontComboBox }


Const
  FontSize = 11;


Constructor TUixAdvancedFontComboBox.Create(oOwner: TComponent);
Begin
  Inherited;

  FSelectedFonts := TFslStringList.Create;
  FAllFonts := TFslStringList.Create;
  DoubleBuffered := True;
  Style := csOwnerDrawFixed;
  SuppressBackground := True;
  ItemHeight := 24;
  OverrideDropDownWidth := True;

  FDefaultFont := TGPFont.Create('Arial', FontSize);

  FDefaultStringFormat := TGdiPlusStringFormat.Create;
  FDefaultStringFormat.HorizontalAlignmentLeft;
  FDefaultStringFormat.VerticalAlignmentMiddle;

  FLookupHashEntry := TUixAdvancedFontComboBoxFontNameHashEntry.Create;
  FHashTable := TUixAdvancedFontComboBoxFontNameHashTable.Create;

  FCachedDC := 0;
End;


Destructor TUixAdvancedFontComboBox.Destroy;
Begin
  DeleteDC(FCachedDC);

  FHashTable.Free;
  FLookupHashEntry.Free;

  FDefaultFont.Free;
  FMeasureGraphics.Free;
  FDefaultStringFormat.Free;

  FSelectedFonts.Free;
  FAllFonts.Free;
  Inherited;
End;


Procedure TUixAdvancedFontComboBox.CreateWnd;
Var
  aComboBoxInfo : TComboBoxInfo;
  aItemRect : TGPRect;
Begin
  Inherited;

  FillChar(aComboBoxInfo, SizeOf(aComboBoxInfo), 0);
  aComboBoxInfo.cbSize := SizeOf(aComboBoxInfo);
  GetComboBoxInfo(Handle, aComboBoxInfo);

  aItemRect := VCLRectToGDIPlusRect(aComboBoxInfo.rcItem);

  // Beacuse we replace the edit control from TCustomControl via (CBS_DROPDOWN Or CBS_OWNERDRAWFIXED)
  // the VCL doesn't route the message correctly.

  FEditControlHandle := aComboBoxInfo.hwndItem;
  FEditControlInstance := MakeObjectInstance(EditControlWndProc);
  FDefaultEditControlProc := Pointer(GetWindowLong(FEditControlHandle, GWL_WNDPROC));

  SetWindowLong(FEditControlHandle, GWL_WNDPROC, LongInt(FEditControlInstance));
End;


Procedure TUixAdvancedFontComboBox.EditControlWndProc(Var Message: TMessage);
Var
  P: TPoint;
  Form: TCustomForm;
Begin
  // Copied directly from TCustomComboBox.EditWndProc.
  // Passes the EditControl messages to ComboWndProc and does some other handling.

  If Message.Msg = WM_SYSCOMMAND Then
  Begin
    WndProc(Message);
    Exit;
  End
  Else If (Message.Msg >= WM_KEYFIRST) And (Message.Msg <= WM_KEYLAST) Then
  Begin
    Form := GetParentForm(Self);
    If (Form <> Nil) And Form.WantChildKey(Self, Message) Then Exit;
  End;

  ComboWndProc(Message, FEditControlHandle, FDefaultEditControlProc);

  Case Message.Msg Of
    WM_LBUTTONDOWN, WM_LBUTTONDBLCLK:
      Begin
        If DragMode = dmAutomatic Then
        Begin
          GetCursorPos(P);
          P := ScreenToClient(P);
          SendMessage(FEditControlHandle, WM_LBUTTONUP, 0,LongInt(PointToSmallPoint(P)));
          BeginDrag(False);
        End;
      End;

    WM_SETFONT:
      If NewStyleControls Then
        SendMessage(FEditControlHandle, EM_SETMARGINS, EC_LEFTMARGIN Or EC_RIGHTMARGIN, 0);
  End;
End;


Procedure TUixAdvancedFontComboBox.CreateParams(Var Params: TCreateParams);
Const
  ComboBoxStyleFlagArray : Array[TComboBoxStyle] Of DWORD = (
    CBS_DROPDOWN, CBS_SIMPLE, CBS_DROPDOWNLIST,
    CBS_DROPDOWN Or CBS_OWNERDRAWFIXED,
    CBS_DROPDOWN Or CBS_OWNERDRAWVARIABLE);

  SortFlagArray : Array[Boolean] Of DWORD = (0, CBS_SORT);
Begin
  Inherited;

  Params.Style := ComboBoxStyleFlagArray[Style] Or SortFlagArray[Sorted] Or CBS_HASSTRINGS Or CBS_AUTOHSCROLL Or WS_VSCROLL Or WS_CHILD Or WS_CLIPSIBLINGS ;
End;


Procedure TUixAdvancedFontComboBox.DrawComboBoxItem(Const oGraphics: TGdiPlusExtendedGraphics; Const iIndex: Integer; Const aItemRectangle: TGPRect; Const aItemState: TOwnerDrawState);
Var
  aFocusRect : TGPRect;
  aTextMetric : TTEXTMETRIC;
  oHashEntry : TUixAdvancedFontComboBoxFontNameHashEntry;
  oLinearGradientBrush : TGPLinearGradientBrush;
  sFontName : String;
Const
  aPathFactorArray : Array [0..10] Of Single = (0.1, 0.1, 0.2, 0.2, 1.0, 0.9, 0.7, 0.5, 0.3, 0.3, 0.2);
  aPathPositionArray : Array [0..10] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1);
Begin
  oGraphics.Clear(VCLColourToGDIPlusColour(Color));

  If (odFocused In aItemState) And Not (odComboBoxEdit In aItemState) Or (odSelected In aItemState) Then
  Begin
    aFocusRect.X := aItemRectangle.X + 1;
    aFocusRect.Y := aItemRectangle.Y;
    aFocusRect.Width := aItemRectangle.Width - 1;
    aFocusRect.Height := aItemRectangle.Height;

    oLinearGradientBrush := TGPLinearGradientBrush.Create(aFocusRect, $FFffebb5, $FFffd66c, LinearGradientModeVertical);
    Try
      oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

      oGraphics.FillRectangle(oLinearGradientBrush, aFocusRect);
    Finally
      oLinearGradientBrush.Free;
    End;

    Pen.SetColor(argbDarkGray);
    oGraphics.DrawRoundedRectangle(Pen, aFocusRect, 3);

    Pen.SetColor(argbHalfTransparentWhite);
    oGraphics.DrawRoundedRectangle(Pen, ContractRectangle(aFocusRect, 1), 3);
  End;

  if (FSelectedFonts.Count > 0) And (iIndex = FSelectedFonts.Count) Then
  Begin
    Pen.SetColor(argbBlack);
    oGraphics.DrawLine(Pen, aItemRectangle.X, aItemRectangle.Y, aItemRectangle.X + aItemRectangle.Width, aItemRectangle.Y);
  End;

  sFontName := Items[iIndex];

  oHashEntry := RetrieveFontByName(sFontName);

  If Assigned(oHashEntry) Then
  Begin
    SelectObject(FCachedDC, oHashEntry.GdiFont.Handle);
    GetTextMetrics(FCachedDC, aTextMetric);

    SolidBrush.SetColor(argbBlack);

    If (aTextMetric.tmCharSet <> ANSI_CHARSET) Then
      oGraphics.DrawValidatedString(sFontName, FDefaultFont, FDefaultStringFormat.ProduceStringFormat, MakeRectF(aItemRectangle), SolidBrush)
    Else
      oGraphics.DrawValidatedString(sFontName, oHashEntry.GdiPlusFont, FDefaultStringFormat.ProduceStringFormat, MakeRectF(aItemRectangle), SolidBrush);
  End;
End;


Procedure TUixAdvancedFontComboBox.Cache;
Var
  iFontIndex : Integer;
  oGdiFont : TFont;
  oHashEntry : TUixAdvancedFontComboBoxFontNameHashEntry;
  sFontName : String;
Begin
  Inherited;

  FCachedDC := CreateCompatibleDC(0);

  FHashTable.Clear;
  FHashTable.PredictCapacityByExpectedCount(Items.Count);

  For iFontIndex := 0 To Items.Count - 1 Do
  Begin
    sFontName := Items[iFontIndex];
    FAllFonts.Add(sFontName);

    oGdiFont := TFont.Create;
    oGdiFont.Size := FontSize;
    oGdiFont.Name := sFontName;

    oHashEntry := TUixAdvancedFontComboBoxFontNameHashEntry.Create;
    oHashEntry.FontName := sFontName;
    oHashEntry.GdiFont := oGdiFont;
    oHashEntry.GdiPlusFont := TGPFont.Create(sFontName, FontSize);
    FHashTable.Add(oHashEntry);
  End;
End;


Function TUixAdvancedFontComboBox.RetrieveFontByName(sFontName: String): TUixAdvancedFontComboBoxFontNameHashEntry;
Begin
  FLookupHashEntry.FontName := sFontName;
  Result := TUixAdvancedFontComboBoxFontNameHashEntry(FHashTable.Get(FLookupHashEntry));
End;


procedure TUixAdvancedFontComboBox.Change;
Var
  iIndex : integer;
Begin
  Inherited;

  iIndex := FSelectedFonts.IndexByValue(Text);
  if iIndex > -1 then
    FSelectedFonts.DeleteByIndex(iIndex);
  FSelectedFonts.Insert(0, Text);
  BuildItems;
end;

procedure TUixAdvancedFontComboBox.BuildItems;
Var
  iLoop : integer;
  sName : String;
Begin
  if ItemIndex = -1 Then
    sName := 'Arial'
  Else
    sName := Items[ItemIndex];

  Items.BeginUpdate;
  Try
    Items.Clear;

    For iLoop := 0 to FSelectedFonts.Count - 1 do
      Items.Add(FSelectedFonts[iLoop]);

    For iLoop := 0 To FAllFonts.Count - 1 Do
      if not FSelectedFonts.ExistsByValue(FAllFonts[iLoop]) Then
        Items.Add(FAllFonts[iLoop]);
  Finally
    Items.EndUpdate;
  End;

  ItemIndex := Items.IndexOf(sName);
end;

{ TUixAdvancedFontComboBoxFontNameHashTable }


Function TUixAdvancedFontComboBoxFontNameHashTable.Equal(oA, oB: TFslHashEntry): Integer;
Begin
  Result := Inherited Equal(oA, oB);

  If Result = 0 Then
    Result := StringCompare(TUixAdvancedFontComboBoxFontNameHashEntry(oA).FontName, TUixAdvancedFontComboBoxFontNameHashEntry(oB).FontName);
End;


Function TUixAdvancedFontComboBoxFontNameHashTable.ItemClass: TFslHashEntryClass;
Begin
  Result := TUixAdvancedFontComboBoxFontNameHashEntry;
End;


{ TUixAdvancedFontComboBoxFontNameHashEntry }


Constructor TUixAdvancedFontComboBoxFontNameHashEntry.Create;
Begin
  Inherited;

  FGdiFont := Nil;
  FGdiPlusFont := Nil;
End;


Destructor TUixAdvancedFontComboBoxFontNameHashEntry.Destroy;
Begin
  FGdiPlusFont.Free;
  FGdiFont.Free;

  Inherited;
End;


Procedure TUixAdvancedFontComboBoxFontNameHashEntry.Generate;
Begin
  Inherited;

  Code := HashStringToCode32(FontName);
End;


Procedure TUixAdvancedFontComboBoxFontNameHashEntry.SetFontName(Const Value: String);
Begin
  FFontName := Value;

  Generate;
End;

{ TUixAdvancedColourComboBox }


Constructor TUixAdvancedColourComboBox.Create(oOwner: TComponent);
Begin
  Inherited;

  FSelectedColours := TFslIntegerList.Create;
  FLastCustom := -1;

  DoubleBuffered := True;
  Style := csOwnerDrawFixed;
  SuppressBackground := True;
  ItemHeight := 24;
  Height := 15;
  OverrideDropDownWidth := True;
  DropDownWidth := 200;

  FDefaultFont := TGPFont.Create('Tahoma', 8);

  FCustomFont := TGdiPlusFont.Create;
  FCustomFont.Italic := True;
  FCustomFont.Underline := True;
  FCustomFont.FontFamily := 'Tahoma';
  FCustomFont.Size := 8;
  FCustomFont.Prepare;

  FDefaultStringFormat := TGdiPlusStringFormat.Create;
  FDefaultStringFormat.HorizontalAlignmentLeft;
  FDefaultStringFormat.VerticalAlignmentMiddle;
End;


Destructor TUixAdvancedColourComboBox.Destroy;
Begin
  FCustomFont.Free;
  FDefaultFont.Free;
  FMeasureGraphics.Free;
  FDefaultStringFormat.Free;
  FSelectedColours.Free;
  Inherited;
End;


Procedure TUixAdvancedColourComboBox.CreateHandle;
Begin
  Inherited;

  BuildItems;
End;


Procedure TUixAdvancedColourComboBox.DrawComboBoxItem(Const oGraphics: TGdiPlusExtendedGraphics; Const iIndex: Integer; Const aItemRectangle: TGPRect; Const aItemState: TOwnerDrawState);
Var
  aFocusRect : TGPRect;
  oLinearGradientBrush : TGPLinearGradientBrush;
  aColourRect : TGPRect;
  aColourTextRect : TGPRect;
  sItemValue : String;
  iCurrentColour : TArgbColour;

  oFont : TGPFont;
Const
  aPathFactorArray : Array [0..10] Of Single = (0.1, 0.1, 0.2, 0.2, 1.0, 0.9, 0.7, 0.5, 0.3, 0.3, 0.2);
  aPathPositionArray : Array [0..10] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1);
Begin
  oGraphics.Clear(VCLColourToGDIPlusColour(Color));

  sItemValue := Items[iIndex];
  iCurrentColour := VCLColourToGDIPlusColour(ItemValueToColour(Items[iIndex]));

  If (odFocused In aItemState) And Not (odComboBoxEdit In aItemState) Or (odSelected In aItemState) Then
  Begin
    aFocusRect.X := aItemRectangle.X + 1;
    aFocusRect.Y := aItemRectangle.Y;
    aFocusRect.Width := aItemRectangle.Width - 1;
    aFocusRect.Height := aItemRectangle.Height;

    oLinearGradientBrush := TGPLinearGradientBrush.Create(aFocusRect, $FFffebb5, $FFffd66c, LinearGradientModeVertical);
    Try
      oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

      oGraphics.FillRectangle(oLinearGradientBrush, aFocusRect);
    Finally
      oLinearGradientBrush.Free;
    End;

    Pen.SetColor(argbDarkGray);
   oGraphics.DrawRoundedRectangle(Pen, aFocusRect, 3);

    Pen.SetColor(argbHalfTransparentWhite);
    oGraphics.DrawRoundedRectangle(Pen, ContractRectangle(aFocusRect, 1), 3);
  End;

  oGraphics.SetSmoothingMode(SmoothingModeAntiAlias);


  If Not (odComboBoxEdit  In aItemState) Then
  Begin
    aColourRect.X := aItemRectangle.X + 3;
    aColourRect.Y := aItemRectangle.Y + 3;
    aColourRect.Width := 16;
    aColourRect.Height := 16;

    oFont := FDefaultFont;

    If StringStartsWith(sItemValue, 'Custom') Then
    Begin
      oFont := FCustomFont.NativeFont;
      if iIndex > 0 Then
      Begin
        Pen.SetColor(argbBlack);
        oGraphics.DrawLine(Pen, aItemRectangle.X, aItemRectangle.Y, aItemRectangle.X + aItemRectangle.Width, aItemRectangle.Y);
      End;
    End;

    SolidBrush.SetColor(iCurrentColour);
    oGraphics.FillRoundedRectangle(SolidBrush, aColourRect, 3);

    Pen.SetColor(argbDarkGray);
    oGraphics.DrawRoundedRectangle(Pen, aColourRect, 3);

    aColourTextRect.X := aColourRect.X + aColourRect.Width + 3;
    aColourTextRect.Y := aItemRectangle.Y;
    aColourTextRect.Width := aItemRectangle.Width - aColourRect.Width - 3;
    aColourTextRect.Height := aItemRectangle.Height;

    SolidBrush.SetColor(argbBlack);
    oGraphics.DrawValidatedString(Items[iIndex], oFont, FDefaultStringFormat.ProduceStringFormat, MakeRectF(aColourTextRect), SolidBrush);
  End
  Else
  Begin
    SolidBrush.SetColor(iCurrentColour);
    oGraphics.FillRectangle(SolidBrush, aItemRectangle);

    Pen.SetColor(argbDarkGray);
    oGraphics.DrawValidRectangle(Pen, aItemRectangle);
  End;
End;


Procedure TUixAdvancedColourComboBox.Change;
Var
  oColourDialog : TColorDialog;
  iIndex : integer;
Begin
  Inherited;

  If ItemIndex = FSelectedColours.Count Then
  Begin
    oColourDialog := TColorDialog.Create(Self);
    oColourDialog.Options := [cdFullOpen, cdAnyColor];
    oColourDialog.Color := ItemValueToColour(Items[ItemIndex]);

    If oColourDialog.Execute Then
      Value := oColourDialog.Color;
  End;
  iIndex := FSelectedColours.IndexByValue(Value);
  if iIndex > -1 then
    FSelectedColours.DeleteByIndex(iIndex);
  if IntegerArrayIndexOf(HTML_COLOUR_VALUES, Value) > -1 Then
    FSelectedColours.Insert(0, Value);
  BuildItems;
End;


Function TUixAdvancedColourComboBox.ItemValueToColour(sText : String) : TColour;
Begin
  If StringStartsWith(sText, 'Custom: ') Then
    Delete(sText, 1, 8);

  Result := clBlack;

  If StringIsHTMLColour(sText) Then
    Result := HTMLColourStringToColour(sText);
End;


Function TUixAdvancedColourComboBox.GetValue: TColour;
Begin
  Result := ItemValueToColour(Items[ItemIndex]);
End;


Procedure TUixAdvancedColourComboBox.SetValue(Const Value : TColour);
Var
  sValue : String;
Begin
  If Value <> ItemValueToColour(Items[ItemIndex]) Then
  Begin
    sValue := ColourToHTMLColourTitleString(Value);

    If Items.IndexOf(sValue) > -1 Then
    Begin
      ItemIndex := Items.IndexOf(sValue);
    End
    Else
    Begin
      Items[FSelectedColours.Count] := 'Custom: ' + sValue;
      ItemIndex := FSelectedColours.Count;
    End;
  End;
End;



procedure TUixAdvancedColourComboBox.BuildItems;
Var
  iLoop, iIndex : integer;
  aLoop : THTMLColours;
  sColour : String;
  sCustom : String;
Begin
  if ItemIndex = -1 Then
    sColour := 'Black'
  Else
    sColour := Items[ItemIndex];
  if FLastCustom = -1 Then
    sCustom := 'Custom'
  Else
    sCustom := Items[FLastCustom];


  Items.BeginUpdate;
  Try
    Items.Clear;

    For iLoop := 0 to FSelectedColours.Count - 1 do
    Begin
      iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, FSelectedColours[iLoop]);
      Items.Add(HTML_COLOUR_TITLES[THTMLColours(iIndex)]);
    End;

    FLastCustom := Items.Count;
    Items.Add(sCustom);

    For aLoop := Low(THTMLColours) To High(THTMLColours) Do
      if not FSelectedColours.ExistsByValue(HTML_COLOUR_VALUES[aLoop]) Then
        Items.Add(HTML_COLOUR_TITLES[aLoop]);
  Finally
    Items.EndUpdate;
  End;

  ItemIndex := Items.IndexOf(sColour);
end;

Const
  MarginWidth = 5;
  DropDownHeight = 18;
  DropDownWidth = 21;


Constructor TUixAdvancedButton.Create(oOwner : TComponent);
Begin
  Inherited;

  If Assigned(oOwner) And (oOwner Is TWinControl) Then
    Parent := TWinControl(oOwner);

  Height := 30;
  Width := 80;

  TabStop := True;
  DoubleBuffered := True;
  TrackMouseLeave := False;

  FPen := TGPPen.Create(argbBlack);
  FSolidBrush := TGPSolidBrush.Create(argbBlack);

  FDefaultFont := TGdiPlusFont.Create;
  FDefaultFont.Size := 9.5;
  FDefaultFont.FontFamily := 'Tahoma';
  FDefaultFont.Prepare;

  FDefaultStringFormat := TGdiPlusStringFormat.Create;
  FDefaultStringFormat.TrimModeEllipsisCharacter;
  FDefaultStringFormat.HorizontalAlignmentLeft;
  FDefaultStringFormat.NoWrap := True;
  FDefaultStringFormat.LineLimit := True;

  FClientPath := TGPGraphicsPath.Create;
  FDropDownPopup := Nil;
  FImageAttributes := TGPImageAttributes.Create;
  FPresentationEntity := TUixAdvancedButtonPresentationEntity.Create;
End;


Destructor TUixAdvancedButton.Destroy;
Begin
  FPresentationEntity.Free;
  FImageAttributes.Free;
  FClientPath.Free;
  FDefaultStringFormat.Free;
  FDefaultFont.Free;

  FSolidBrush.Free;
  FPen.Free;

  Inherited;
End;


Procedure TUixAdvancedButton.Paint(Const oGraphics : TGdiPlusExtendedGraphics; Const pDC : HDC; Const aClipRectangle : TGPRect);
Var
  aNonChevronAreaRect : TGPRect;
  aImageRect : TGPRectF;
  aIconBoundingRect : TGPRect;
  aCaptionBoundingRectF : TGPRectF;
  aDropDownRect : TGPRect;
  aDropDownBoundingRect : TGPRect;

  aDropDownEffectRect : TGPRect;

  oLinearGradientBrush : TGPLinearGradientBrush;

  iTopColour : TArgbColour;
  iBottomColour : TArgbColour;

  bHasPopup : Boolean;
  bHasClick : Boolean;

  iIconWidth : Integer;
  iIconHeight : Integer;
Const
  aPathFactorArray : Array [0..10] Of Single = (0.9, 0.9, 0.9, 0.8, 0.2, 0.2, 0.3, 0.5, 0.5, 0.6, 0.7);
  aPathPositionArray : Array [0..10] Of Single = (0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1);
Begin
  Inherited;

  bHasPopup := HasPopupMenuItems;
  bHasClick := Assigned(OnClick);

  FClientRectangle.X := ClientRect.Left + PresentationEntity.MarginLeft;
  FClientRectangle.Y := ClientRect.Top + PresentationEntity.MarginTop;
  FClientRectangle.Width := ((ClientRect.Right - PresentationEntity.MarginRight) - (ClientRect.Left + PresentationEntity.MarginLeft)) - 1;
  FClientRectangle.Height := ((ClientRect.Bottom - PresentationEntity.MarginBottom) - (ClientRect.Top + PresentationEntity.MarginTop)) - 1;

  oGraphics.SetSmoothingMode(SmoothingModeAntiAlias);

  FClientPath.Reset;

  Case ShapeStyle Of
    UixAdvancedButtonShapeStyleSquare : FClientPath.AddRectangle(FClientRectangle);
    UixAdvancedButtonShapeStyleRounded : oGraphics.RoundedRectanglePath(FClientPath, MakeRectF(FClientRectangle), 3);
  Else
    Error('Paint', 'Unhandled shape style.');
  End;

  If Not Enabled Or Not InputEnabled Then
  Begin
    iTopColour := PresentationEntity.DisabledBackgroundColourTop;
    iBottomColour := PresentationEntity.DisabledBackgroundColourBottom;
  End
  Else If FMouseHover Then
  Begin
    iTopColour := PresentationEntity.HoverBackgroundColourTop;
    iBottomColour := PresentationEntity.HoverBackgroundColourBottom;
  End
  Else If FChecked Then
  Begin
    iTopColour := $FFFDB759;
    iBottomColour := $FFFDEB9F;
  End
  Else If Focused Then
  Begin
    iTopColour := PresentationEntity.FocusedBackgroundColourTop;
    iBottomColour := PresentationEntity.FocusedBackgroundColourBottom;
  End
  Else
  Begin
    iTopColour := PresentationEntity.BackgroundColourTop;
    iBottomColour := PresentationEntity.BackgroundColourBottom;
  End;

  oLinearGradientBrush := TGPLinearGradientBrush.Create(FClientRectangle, iTopColour, iBottomColour, LinearGradientModeVertical);
  Try
    oLinearGradientBrush.SetBlend(@aPathFactorArray, @aPathPositionArray, 11);

    oGraphics.FillPath(oLinearGradientBrush, FClientPath);
  Finally
    oLinearGradientBrush.Free;
  End;

  If Enabled And FMouseHover Then
    FPen.SetColor(PresentationEntity.HoverBorderColour)
  Else
    FPen.SetColor(PresentationEntity.BorderColour);

  oGraphics.DrawPath(FPen, FClientPath);

  If Not bHasPopup Then
  Begin
    aNonChevronAreaRect.X := FClientRectangle.X;
    aNonChevronAreaRect.Y := FClientRectangle.Y;
    aNonChevronAreaRect.Width := FClientRectangle.Width;
    aNonChevronAreaRect.Height := FClientRectangle.Height;

    aDropDownBoundingRect := CreateEmptyRectangle;
  End
  Else If bHasPopup And PresentationEntity.ShowDropDownChevronBelow Then
  Begin
    aNonChevronAreaRect.X := FClientRectangle.X;
    aNonChevronAreaRect.Y := FClientRectangle.Y;
    aNonChevronAreaRect.Width := FClientRectangle.Width;
    aNonChevronAreaRect.Height := FClientRectangle.Height - DropDownHeight;

    aDropDownBoundingRect.X := aNonChevronAreaRect.X;
    aDropDownBoundingRect.Y := aNonChevronAreaRect.Y + aNonChevronAreaRect.Height;
    aDropDownBoundingRect.Width := aNonChevronAreaRect.Width;
    aDropDownBoundingRect.Height := DropDownHeight;
  End
  Else If bHasPopup Then
  Begin
    aNonChevronAreaRect.X := FClientRectangle.X;
    aNonChevronAreaRect.Y := FClientRectangle.Y;
    aNonChevronAreaRect.Width := FClientRectangle.Width - DropDownWidth;
    aNonChevronAreaRect.Height := FClientRectangle.Height;

    aDropDownBoundingRect.X := aNonChevronAreaRect.X + aNonChevronAreaRect.Width;
    aDropDownBoundingRect.Y := aNonChevronAreaRect.Y;
    aDropDownBoundingRect.Width := DropDownWidth;
    aDropDownBoundingRect.Height := aNonChevronAreaRect.Height;
  End
  Else
  Begin
    aNonChevronAreaRect := FClientRectangle;
    aDropDownBoundingRect := CreateEmptyRectangle;
  End;

  FDropDownRectangle := aDropDownBoundingRect;

  If PresentationEntity.HasIconBitmapImage Then
  Begin
    iIconHeight := PresentationEntity.IconBitmapImage.Height;
    iIconWidth := PresentationEntity.IconBitmapImage.Width;

    aIconBoundingRect.X := aNonChevronAreaRect.X;
    aIconBoundingRect.Y := aNonChevronAreaRect.Y;
    aIconBoundingRect.Width := iIconWidth + (MarginWidth * 2);
    aIconBoundingRect.Height := aNonChevronAreaRect.Height;

    aImageRect.X := aIconBoundingRect.X + MarginWidth;
    aImageRect.Y := aIconBoundingRect.Y + (aIconBoundingRect.Height / 2) - (iIconHeight / 2);
    aImageRect.Width := iIconWidth;
    aImageRect.Height := iIconHeight;

    If Not Enabled Or Not InputEnabled Then
      FImageAttributes.SetColorMatrix(GrayScaleColorMatrixArray)
    Else
      FImageAttributes.ClearColorMatrix;

    If PresentationEntity.IconBitmapImage.IsImageFormatBMP Then
      FImageAttributes.SetColorKey(PresentationEntity.IconBitmapImage.TransparentColour, PresentationEntity.IconBitmapImage.TransparentColour)
    Else
      FImageAttributes.ClearColorKey;

    If (Checked Or (FLeftMouseDown And FMouseHover)) And Not FDropDownActivated Then
      oGraphics.TranslateTransform(2, 2);

    oGraphics.DrawImage(PresentationEntity.IconBitmapImage.NativeBitmap, aImageRect, 0, 0, aImageRect.Width, aImageRect.Height, UnitPixel, FImageAttributes, Nil, Nil);
    oGraphics.ResetTransform;
  End
  Else
  Begin
    aIconBoundingRect := CreateEmptyRectangle;
  End;

  If FMouseHover Then
  Begin
    FPen.SetColor(argbDkGray);

    Case ShapeStyle Of
      UixAdvancedButtonShapeStyleSquare : oGraphics.DrawRectangle(FPen, ContractRectangle(FClientRectangle, 2));
      UixAdvancedButtonShapeStyleRounded : oGraphics.DrawRoundedRectangle(FPen, ContractRectangle(FClientRectangle, 2), 3);
    Else
      Error('Paint', 'Unhandled shape style.');
    End;
  End;

  If Checked Or (FLeftMouseDown And FMouseHover) And Not FDropDownActivated Then
  Begin
    aDropDownEffectRect := ContractRectangle(FClientRectangle, 2);

    Case ShapeStyle Of
      UixAdvancedButtonShapeStyleSquare :
      Begin
        If FLeftMouseDown Then
        Begin
          FSolidBrush.SetColor(ColourAdjustAlpha(argbBlack, 25));
          oGraphics.FillRectangle(FSolidBrush, aDropDownEffectRect);

          FPen.SetColor(argbBlack);
        End
        Else
        Begin
          FSolidBrush.SetColor(ColourAdjustAlpha(argbDarkGray, 50));
          oGraphics.FillRectangle(FSolidBrush, aDropDownEffectRect);
        End;

        oGraphics.DrawRectangle(FPen, aDropDownEffectRect);

        oGraphics.SetClip(ContractRectangle(aDropDownEffectRect, 1));

        FPen.SetColor(argbGray);
        oGraphics.DrawRectangle(FPen, ContractRectangle(aDropDownEffectRect, 1));

        oGraphics.SetClip(ContractRectangle(aDropDownEffectRect, 2));

        FPen.SetColor(argbLtGray);
        oGraphics.DrawRectangle(FPen, ContractRectangle(aDropDownEffectRect, 2));
      End;

      UixAdvancedButtonShapeStyleRounded :
      Begin
        If FLeftMouseDown Then
        Begin
          FSolidBrush.SetColor(ColourAdjustAlpha(argbBlack, 25));
          oGraphics.FillRoundedRectangle(FSolidBrush, aDropDownEffectRect, 3);

          FPen.SetColor(argbBlack);
        End
        Else
        Begin
          FSolidBrush.SetColor(ColourAdjustAlpha(argbDarkGray, 50));
          oGraphics.FillRectangle(FSolidBrush, aDropDownEffectRect);
        End;

        oGraphics.DrawRoundedRectangle(FPen, aDropDownEffectRect, 3);

        oGraphics.SetClip(ContractRectangle(aDropDownEffectRect, 1));

        FPen.SetColor(argbGray);
        oGraphics.DrawRoundedRectangle(FPen, ContractRectangle(aDropDownEffectRect, 1), 3);

        oGraphics.SetClip(ContractRectangle(aDropDownEffectRect, 2));

        FPen.SetColor(argbLtGray);
        oGraphics.DrawRoundedRectangle(FPen, ContractRectangle(aDropDownEffectRect, 2), 3);
      End;
    Else
      Error('Paint', 'Unhandled shape style.');
    End;

    oGraphics.ResetClip;
  End;

  If PresentationEntity.ShowCaption Then
  Begin
    aCaptionBoundingRectF.X := aNonChevronAreaRect.X + aIconBoundingRect.Width;
    aCaptionBoundingRectF.Y := aNonChevronAreaRect.Y;
    aCaptionBoundingRectF.Width := aNonChevronAreaRect.Width - aIconBoundingRect.Width;
    aCaptionBoundingRectF.Height := aNonChevronAreaRect.Height;

    Case PresentationEntity.HorizontalTextAlignment Of
      UixAdvancedButtonHorizontalTextAlignmentTypeLeft : FDefaultStringFormat.HorizontalAlignmentLeft;
      UixAdvancedButtonHorizontalTextAlignmentTypeCenter : FDefaultStringFormat.HorizontalAlignmentCenter;
      UixAdvancedButtonHorizontalTextAlignmentTypeRight : FDefaultStringFormat.HorizontalAlignmentRight;
    End;

   If FLeftMouseDown And FMouseHover And Not FDropDownActivated Then
      oGraphics.TranslateTransform(2, 2);

    If Enabled And InputEnabled Then
      FSolidBrush.SetColor(PresentationEntity.TextColour)
    Else
      FSolidBrush.SetColor(argbHalfTransparentBlack);

    oGraphics.DrawValidatedString(PresentationEntity.Caption, FDefaultFont.NativeFont, FDefaultStringFormat.ProduceStringFormat, aCaptionBoundingRectF, FSolidBrush);
    oGraphics.ResetTransform;
  End
  Else
  Begin
    aCaptionBoundingRectF := CreateEmptyRectangleF;
  End;

  If bHasPopup Then
  Begin
    aDropDownRect.X := aDropDownBoundingRect.X + (aDropDownBoundingRect.Width Div 2) - 4;
    aDropDownRect.Y := aDropDownBoundingRect.Y + (aDropDownBoundingRect.Height Div 2) - 2;
    aDropDownRect.Width := 8;
    aDropDownRect.Height := 4;

    FSolidBrush.SetColor(argbBlack);

    oGraphics.FillTriangle(FSolidBrush, aDropDownRect);

    If bHasClick And FMouseHover Then
    Begin
      If FLeftMouseDown And Not FDropDownActivated Then
        FPen.SetColor(argbBlack)
      Else
        FPen.SetColor(argbDkGray);

      If Not PresentationEntity.ShowDropDownChevronBelow Then
        oGraphics.DrawLine(FPen, aDropDownBoundingRect.X, aDropDownBoundingRect.Y + 2, aDropDownBoundingRect.X, aDropDownBoundingRect.Y + aDropDownBoundingRect.Height - 2)
      Else
        oGraphics.DrawLine(FPen, aDropDownBoundingRect.X + 2, aDropDownBoundingRect.Y, aDropDownBoundingRect.X + aDropDownBoundingRect.Width - 2, aDropDownBoundingRect.Y)
    End;
  End;
End;


Procedure TUixAdvancedButton.MouseUp(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer);
Begin
  Inherited;

  FLeftMouseDown := False;

  If (aButton = mbLeft) And Assigned(FOnClick) And RectangleContainsPoint(FClientRectangle, iX, iY) Then
    FOnClick(Self);

  Invalidate;
End;


Procedure TUixAdvancedButton.MouseDown(aButton : TMouseButton; aShift : TShiftState; iX, iY : Integer);
Var
  aPopupClientPoint : TPoint;
  bHasClick : Boolean;
  bHasPopup : Boolean;
  bMenuVisible : Boolean;
  iMenuItemIndex : Integer;
  iMenuHeight : Integer;
  aClientRectOnScreen : TRect;
  aMonitorInfo : TMonitorInfo;
  aMonitorRect : TRect;
Begin
  Inherited;

  If RectangleContainsPoint(FClientRectangle, iX, iY) Then
  Begin
    bHasClick := Assigned(OnClick);
    bHasPopup := HasPopupMenuItems;

    SetFocus;

    FLeftMouseDown := (aButton = mbLeft);

    Invalidate;

    If bHasPopup Then
    Begin
      bMenuVisible := FDropDownActivated;

      If bHasClick And bHasPopup Then
        FDropDownActivated := RectangleContainsPoint(FDropDownRectangle, iX, iY)
      Else
        FDropDownActivated := RectangleContainsPoint(FClientRectangle, iX, iY);

      RedrawWindow(Handle, Nil, 0, RDW_INVALIDATE Or RDW_UPDATENOW);

      If Not bMenuVisible And FDropDownActivated Then
      Begin
        If (bHasPopup And bHasClick) Then
          aPopupClientPoint.X := (ClientRect.Left + PresentationEntity.MarginLeft) + (ClientRect.Right - PresentationEntity.MarginRight) - DropDownWidth
        Else
          aPopupClientPoint.X := ClientRect.Left + PresentationEntity.MarginLeft;

        aClientRectOnScreen := GDIPlusRectToVCLRect(GdiPlusClientToScreen(FClientRectangle));
        aMonitorInfo := MonitorInfoFromRect(aClientRectOnScreen);
        aMonitorRect := aMonitorInfo.rcWork;

        iMenuHeight := 0;

        For iMenuItemIndex := 0 To FDropDownPopup.Items.Count - 1 Do
          iMenuHeight := iMenuHeight + TUixMenuItem(FDropDownPopup.Items[iMenuItemIndex]).MeasuredHeight;

        If (aClientRectOnScreen.Bottom + iMenuHeight) > aMonitorRect.Bottom Then
          aPopupClientPoint.Y := ClientRect.Top - iMenuHeight
        Else
          aPopupClientPoint.Y := (ClientRect.Top + PresentationEntity.MarginTop) + (ClientRect.Bottom - PresentationEntity.MarginBottom);

        If PresentationEntity.FullButtonPopup Then
        Begin
          aPopupClientPoint.X := FClientRectangle.X;

          For iMenuItemIndex := 0 To FDropDownPopup.Items.Count - 1 Do
            TUixMenuItem(FDropDownPopup.Items[iMenuItemIndex]).MinimumWidth := FClientRectangle.Width - (DropDownWidth * 2);
        End;

        Windows.ClientToScreen(Handle, aPopupClientPoint);

        FDropDownPopup.OnClosePopup := PopupMenuClose;
        FDropDownPopup.Popup(aPopupClientPoint.X, aPopupClientPoint.Y);
      End;
    End;
  End;
End;


Procedure TUixAdvancedButton.MouseMove(aShift : TShiftState; iX, iY : Integer);
Var
  bWasHovering : Boolean;
Begin
  Inherited;

  bWasHovering := FMouseHover;

  FLeftMouseDown := (ssLeft In aShift);
  FMouseHover := RectangleContainsPoint(FClientRectangle, iX, iY);

  If FDropDownActivated Then
    FDropDownActivated := False;

  If bWasHovering <> FMouseHover Then
    Invalidate;
End;


Procedure TUixAdvancedButton.KeyDown(Var iKey : Word; aShift : TShiftState);
Begin
  Inherited;

End;


Procedure TUixAdvancedButton.KeyUp(Var iKey : Word; aShift : TShiftState);
Begin
  Inherited;

End;


Procedure TUixAdvancedButton.WMKillFocus(Var aMessage : TMessage);
Begin
  FDropDownActivated := False;

  Invalidate;
End;


Procedure TUixAdvancedButton.WMSetFocus(Var aMessage : TMessage);
Begin
  Invalidate;
End;


Function TUixAdvancedButton.RecommendedHeight : Integer;
Begin
  If PresentationEntity.HasIconBitmapImage Then
  Begin
    Result := IntegerMax(PresentationEntity.IconBitmapImage.Height + 10, 30);

    If PresentationEntity.ShowDropDownChevronBelow And Assigned(FDropDownPopup) Then
      Inc(Result, DropDownHeight);
  End
  Else
  Begin
    Result := 30;
  End;

  Inc(Result, PresentationEntity.MarginTop + PresentationEntity.MarginBottom);
End;


Function TUixAdvancedButton.RecommendedWidth : Integer;
Var
  oGraphics : TGdiPlusExtendedGraphics;
  aStringSize : TGPSizeF;
Begin
  If PresentationEntity.ShowCaption Then
  Begin
    oGraphics := TGdiPlusExtendedGraphics.Create(Handle, False);
    Try
      aStringSize := oGraphics.StringSize(PresentationEntity.Caption, FDefaultFont.NativeFont, FDefaultStringFormat.ProduceStringFormat);
    Finally
      oGraphics.Free;
    End;

    Result := RealCeiling(aStringSize.Width) + MarginWidth;
  End
  Else
  Begin
    Result := 0;
  End;

  If PresentationEntity.HasIconBitmapImage Then
    Result := Result + PresentationEntity.IconBitmapImage.Width + (2 * MarginWidth);

  Inc(Result, PresentationEntity.MarginLeft + PresentationEntity.MarginRight);

  If Not PresentationEntity.ShowDropDownChevronBelow And Assigned(FDropDownPopup) Then
    Result := Result + DropDownWidth;
End;


Procedure TUixAdvancedButton.CMMouseLeave(Var aMessage: TMessage);
Begin
  Inherited;

  If Enabled Or InputEnabled Then
  Begin
    FMouseHover := False;
    FDropDownActivated := False;

    Invalidate;
  End;
End;


Procedure TUixAdvancedButton.PopupMenuClose(oSender: TObject);
Begin
  FDropDownActivated := False;
  FLeftMouseDown := False;
  FMouseHover := False;

  Invalidate;
End;


Procedure TUixAdvancedButton.Prepare;
Begin
  FDefaultFont.FontFamily := PresentationEntity.FontFamily;
  FDefaultFont.Size := PresentationEntity.FontSize;

  FDefaultFont.Prepare;

  Invalidate;
End;


Procedure TUixAdvancedButton.CMHintShow(Var aMessage: TCMHintShow);
Var
  sHint : String;
Begin
  If RectangleContainsPoint(FClientRectangle, aMessage.HintInfo.CursorPos.X, aMessage.HintInfo.CursorPos.Y) Then
  Begin
    sHint := Hint;
    If sHint = '' Then
      sHint := PresentationEntity.Caption;

    aMessage.HintInfo.CursorRect := GDIPlusRectToVCLRect(FClientRectangle);
    aMessage.HintInfo.HintStr := sHint;
    aMessage.Result := 0;
  End
  Else
  Begin
    aMessage.Result := 1;
  End;
End;


Procedure TUixAdvancedButton.ShapeStyleRounded;
Begin
  FShapeStyle := UixAdvancedButtonShapeStyleRounded;
  Invalidate;
End;


Procedure TUixAdvancedButton.ShapeStyleSquare;
Begin
  FShapeStyle := UixAdvancedButtonShapeStyleSquare;
  Invalidate;
End;


Procedure TUixAdvancedButton.FireClick;
Begin
  If Assigned(FOnClick) Then
    FOnClick(Self)
End;


Function TUixAdvancedButton.HasPopupMenuItems : Boolean;
Var
  iMenuItemIndex : Integer;
Begin
  Result := Assigned(FDropDownPopup);

  If Result Then
  Begin
    Result := False;

    iMenuItemIndex := 0;
    While Not Result And (iMenuItemIndex < FDropDownPopup.Items.Count) Do
    Begin
      Result := FDropDownPopup.Items[iMenuItemIndex].Visible;

      Inc(iMenuItemIndex);
    End;
  End;
End;

Constructor TUixAdvancedButtonPresentationEntity.Create;
Begin
  Inherited;

  FShowCaption := True;

  FIconBitmapImage := Nil;

  FTextColour := argbBlack;

  FBackgroundColourTop := $FFAED1FF;
  FBackgroundColourBottom := $FFDFEDFF;

  FDisabledBackgroundColourTop := $FFCFD7EB;
  FDisabledBackgroundColourBottom := $FFF5F7FB;

  FFocusedBackgroundColourTop := $FFFFD767;
  FFocusedBackgroundColourBottom := $FFFFFEE4;

  FHoverBackgroundColourTop := $FFFFD767;
  FHoverBackgroundColourBottom := $FFFFFEE4;

  FFontSize := 9.5;
  FFontFamily := 'Tahoma';

  FHorizontalTextAlignment := UixAdvancedButtonHorizontalTextAlignmentTypeLeft;

  FBorderColour := $FF6593CF;
  FHoverBorderColour := FBorderColour;
End;


Destructor TUixAdvancedButtonPresentationEntity.Destroy;
Begin
  Inherited;

  FIconBitmapImage.Free;
End;


Function TUixAdvancedButtonPresentationEntity.Link : TUixAdvancedButtonPresentationEntity;
Begin
  Result := TUixAdvancedButtonPresentationEntity(Inherited Link);
End;


Function TUixAdvancedButtonPresentationEntity.GetHasIconBitmapImage : Boolean;
Begin
  Result := Assigned(FIconBitmapImage);
End;


Procedure TUixAdvancedButtonPresentationEntity.SetHasIconBitmapImage(Const Value : Boolean);
Begin
  If Assigned(FIconBitmapImage) And Not Value Then
  Begin
    FIconBitmapImage.Free;
    FIconBitmapImage := Nil;
  End
  Else If Not Assigned(FIconBitmapImage) And Value Then
  Begin
    FIconBitmapImage := TGdiPlusBitmapImage.Create;
  End;
End;


Function TUixAdvancedButtonPresentationEntity.GetIconBitmapImage : TGdiPlusBitmapImage;
Begin
  Assert(Invariants('GetIconBitmapImage', FIconBitmapImage, TGdiPlusBitmapImage, 'FIconBitmapImage'));

  Result := FIconBitmapImage;
End;


Procedure TUixAdvancedButtonPresentationEntity.SetIconBitmapImage(Const Value : TGdiPlusBitmapImage);
Begin
  Assert(Invariants('SetIconBitmapImage', Value, TGdiPlusBitmapImage, 'Value'));

  FIconBitmapImage.Free;
  FIconBitmapImage := Value;
End;


Function TUixAdvancedButtonPresentationEntity.GetMargin : Integer;
Begin
  Assert(CheckCondition((FMarginLeft = FMarginRight) And (FMarginRight = FMarginTop) And (FMarginTop = FMarginBottom), 'GetMargin', 'Margins are not equal and cannot be queried in this way.'));

  Result := FMarginLeft;
End;


Procedure TUixAdvancedButtonPresentationEntity.SetMargin(Const Value : Integer);
Begin
  FMarginLeft := Value;
  FMarginRight := Value;
  FMarginTop := Value;
  FMarginBottom := Value;
End;

End.

