Unit fui_vclx_forms;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Contnrs, ExtCtrls, StdCtrls, ComCtrls, Consts,
  fsl_base, fsl_utilities, fsl_collections,
  fui_vclx_base, fui_vclx_controls;


Type
  TUixFormClass = Class Of TUixForm;

  TUixFormCanAcceptDelegate = Function : Boolean Of Object;
  TUixFormCanRejectDelegate = Function : Boolean Of Object;
  TUixFormCanExecuteDelegate = Function : Boolean Of Object;
  TUixFormCanValidateDelegate = Function : Boolean Of Object;
  TUixFormCloseAcceptDelegate = Function : Boolean Of Object;
  TUixFormCloseAcceptPrepareDelegate = Procedure Of Object;
  TUixFormCloseRejectDelegate = Function : Boolean Of Object;
  TUixFormAcceptDelegate = Procedure Of Object;
  TUixFormRejectDelegate = Procedure Of Object;
  TUixFormPaintDelegate = Procedure Of Object;
  TUixFormCloseQueryDelegate = Procedure (Const bForceClose : Boolean; Var bCanClose : Boolean) Of Object;
  TUixFormDialogKeyDirectionDelegate = Procedure (Const iFactorX, iFactorY : Integer) Of Object;
  TUixFormCollectCreateParametersDelegate = Procedure (Var aParams : TCreateParams) Of Object;

  TUixForm = Class(TForm)
    pnlClient : TUixPanel;
    pnlBottom : TUixPanel;
    btnCancel : TUixButton;
    btnOk : TUixButton;

    Procedure FormShow(oSender : TObject);
    Procedure btnOkClick(oSender : TObject);
    Procedure btnCancelClick(oSender : TObject);
    Procedure FormCloseQuery(oSender : TObject; Var bCanClose : Boolean);

    Private
      FBusy : Boolean;
      FBalloon : TUixBalloon;
      FTaskTray : Boolean;
      FPrimaryControl : TWinControl;
      FPopupWindowStyle : Boolean;
      FBalloonAnchorPosition : TUixBalloonAnchorPosition;

      FAllowWindowMoveOnClientClick : Boolean;
      FImplementsPrintMessages : Boolean;

      FHasAutomaticDocking : Boolean;
      FIsChangingAutomaticDocking : Boolean;

      FHasDefaultClientBorderWidth : Boolean;

      FDefaultClientBorderWidth : Integer;

      FHasModalOKButton : Boolean;
      FHasModalCancelButton : Boolean;
      FHasDialogKeyDirection : Boolean;

      FMouseWheelHover : Boolean;

      FForceClose : Boolean;
      FFireDeactivateEventOnApplicationDeactivate : Boolean;

      FCanAcceptDelegate : TUixFormCanAcceptDelegate;
      FCanRejectDelegate : TUixFormCanRejectDelegate;
      FCanExecuteDelegate : TUixFormCanExecuteDelegate;
      FCanValidateDelegate : TUixFormCanValidateDelegate;
      FCloseQueryDelegate : TUixFormCloseQueryDelegate;
      FDialogKeyDirectionDelegate : TUixFormDialogKeyDirectionDelegate;
      FCloseAcceptDelegate : TUixFormCloseAcceptDelegate;
      FCloseAcceptPrepareDelegate : TUixFormCloseAcceptPrepareDelegate;
      FCloseRejectDelegate : TUixFormCloseRejectDelegate;
      FAcceptDelegate : TUixFormAcceptDelegate;
      FRejectDelegate : TUixFormRejectDelegate;
      FPaintDelegate : TUixFormPaintDelegate;
      FMoveDelegate : TNotifyEvent;
      FCollectCreateParametersDelegate : TUixFormCollectCreateParametersDelegate;

      Function GetAnchoredBottom: Boolean;
      Procedure SetAnchoredBottom(Const Value: Boolean);

      Function GetAnchoredLeft: Boolean;
      Procedure SetAnchoredLeft(Const Value: Boolean);

      Function GetAnchoredRight: Boolean;
      Procedure SetAnchoredRight(Const Value: Boolean);

      Function GetAnchoredTop: Boolean;
      Procedure SetAnchoredTop(Const Value: Boolean);

      Function GetBorderIconHelp : Boolean;
      Procedure SetBorderIconHelp(Const Value : Boolean);

      Function GetBorderIconMaximise : Boolean;
      Procedure SetBorderIconMaximise(Const Value : Boolean);

      Function GetBorderIconMinimise : Boolean;
      Procedure SetBorderIconMinimise(Const Value : Boolean);

      Function GetBorderIconSystem : Boolean;
      Procedure SetBorderIconSystem(Const Value : Boolean);

      Function GetHasClientPanel : Boolean;
      Procedure SetHasClientPanel(Const Value : Boolean);

      Function GetHasBottomPanel : Boolean;
      Procedure SetHasBottomPanel(Const Value : Boolean);

      Function GetHasOKButton : Boolean;
      Procedure SetHasOKButton(Const Value : Boolean);

      Function GetHasDefaultOKButton: Boolean;
      Procedure SetHasDefaultOKButton(Const Value: Boolean);

      Function GetHasCancelButton: Boolean;
      Procedure SetHasCancelButton(Const Value: Boolean);

      Function GetHasAutomaticDocking: Boolean;
      Procedure SetHasAutomaticDocking(Const Value: Boolean);

      Function GetPopupWindowStyle : Boolean;
      Procedure SetPopupWindowStyle(Const Value : Boolean);

      Function GetInternalFormState : TFormState;
      Procedure SetInternalFormState(Const Value : TFormState);

      Procedure CMDialogKey(Var aMessage : TCMDialogKey); Message CM_DIALOGKEY;
      Procedure WMNCHitTest(Var aMessage : TWMNCHitTest); Message WM_NCHITTEST;

    Protected
      Procedure PrepareInitialise; Virtual;
      Procedure PrepareFinalise; Virtual;

      Procedure Initialise; Virtual;
      Procedure Finalise; Virtual;

      Procedure WndProc(Var aMessage : TMessage); Override;

      Procedure WMEndSession(Var aMessage : TMessage); Message WM_ENDSESSION;
      Procedure WMQueryEndSession(Var aMessage : TMessage); Message WM_QUERYENDSESSION;
      Procedure WMMove(Var aMessage : TMessage); Message WM_MOVE;
      Procedure WMPrint(Var aMessage : TMessage); Message WM_PRINT;

      Procedure CreateParams(Var aParams : TCreateParams); Override;

      Procedure Error(aException : EFslExceptionClass; Const sMethod, sMessage : String); Overload;
      Procedure Error(Const sMethod, sMessage : String); Overload;
      Class Procedure ClassError(Const sMethod, sMessage : String);

      Function Condition(bCondition : Boolean; Const sLocation, sMessage : String) : Boolean; Overload;
      Function Invariant(Const sMethod, sMessage: String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oForm : TUixForm; aClass: TClass; Const sObject : String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TObject; aClass : TClass; Const sObject : String) : Boolean; Overload;
      Function Invariants(Const sLocation : String; oObject : TFslObject; aClass: TFslObjectClass; Const sObject : String): Boolean; Overload;
      Function Invariants(Const sLocation : String; aReference, aClass : TClass; Const sReference : String) : Boolean; Overload;

      Procedure DialogKeyDirection(Const iFactorX, iFactorY : Integer); Virtual;

      Function QueryEndSession : Boolean; Virtual;
      Procedure EndSession; Virtual;

      Procedure DoCloseQuery(Var bCanClose : Boolean); Virtual;
      Procedure DoMove; Virtual;

      Function CloseAccept : Boolean; Virtual;
      Function CloseReject : Boolean; Virtual;

      Function CanAccept : Boolean; Virtual;
      Function CanReject : Boolean; Virtual;
      Function CanExecute : Boolean; Virtual;
      Function CanValidate : Boolean; Virtual;

      Procedure RefreshActions; Virtual;
      Procedure RefreshBottom; Virtual;
      Procedure RefreshClient; Virtual;
      Procedure RefreshButtons; Virtual;

      Function ExecuteModal : Boolean; Virtual;

      Procedure HideBalloon;
      Procedure HideBalloons(oWinControl : TWinControl);

      Procedure SetParent(oParent : TWinControl); Override;

      Function GetBusy : Boolean; Virtual;
      Procedure SetBusy(Const Value: Boolean); Virtual;

    Public
      constructor Create(oOwner : TComponent); Override;
      destructor Destroy; Override;

      Procedure AfterConstruction; Override;
      Procedure BeforeDestruction; Override;

      Function Accepting : Boolean; Virtual;
      Function Rejecting : Boolean; Virtual;

      Procedure HideClientBalloons;

      Procedure MouseWheelHandler(Var aMessage: TMessage); Override;

      Function Invariants(Const sLocation : String; aClass : TClass) : Boolean; Overload;

      Procedure ModalOK;
      Procedure ModalCancel;
      Procedure ModalNone;

      Function IsModalNone : Boolean;
      Function IsModalOK : Boolean;
      Function IsModalCancel : Boolean;

      Procedure Invalid(oControl : TWinControl; Const sTitle, sMessage : String);

      Procedure Embed(Const oParent : TWinControl);
      Function IsEmbedded : Boolean; 

      Function Execute : Boolean; Virtual;
      Function ExecuteProgress : Boolean; Virtual;
      Function ExecuteStandard : Boolean; Virtual;

      Function AttemptAccept : Boolean; Virtual;
      Function AttemptReject : Boolean; Virtual;

      Procedure Refresh; Virtual;
      Procedure RefreshPrepare; Virtual;
      Procedure RefreshExecute; Virtual;
      Procedure RefreshTerminate; Virtual;

      Procedure RefreshInitialPrepare; Virtual;
      Procedure RefreshInitialExecute; Virtual;
      Procedure RefreshInitialTerminate; Virtual;

      Procedure Restore; Virtual;
      Procedure Commit; Virtual;
      Procedure Accept; Virtual;
      Procedure Reject; Virtual;

      Procedure Enter; Virtual;
      Procedure Leave; Virtual;

      Procedure Flash;
      Procedure Center;
      Procedure Hide; Virtual;
      Procedure Show; Virtual;

      Procedure Paint; Override;

      Procedure CloseModal;

      Procedure Okay; Virtual;
      Procedure Cancel; Virtual;

      Procedure Progress;

      Function IsAlignedHorizontal : Boolean;
      Function IsAlignedVertical : Boolean;
      Function IsAlignedTop : Boolean;
      Function IsAlignedBottom : Boolean; 
      Function IsAlignedLeft : Boolean; 
      Function IsAlignedRight : Boolean; 
      Function IsAlignedClient : Boolean; 

      Procedure AlignClient; Virtual;
      Procedure AlignLeft; Virtual;
      Procedure AlignRight; Virtual;
      Procedure AlignTop; Virtual;
      Procedure AlignBottom; Virtual;

      Procedure ShuffleLeft;
      Procedure ShuffleRight;
      Procedure ShuffleTop; 
      Procedure ShuffleBottom; 

      Procedure PositionDesigned; 
      Procedure PositionScreen;
      Procedure PositionDesktop;
      Procedure PositionMainForm;
      Procedure PositionOwner;

      Function IsPositionDesigned : Boolean;
      Function IsPositionScreen : Boolean;
      Function IsPositionDesktop : Boolean;
      Function IsPositionMainForm : Boolean;
      Function IsPositionOwner : Boolean;

      Procedure BorderStyleDialog;
      Procedure BorderStyleSingle; 
      Procedure BorderStyleNone; 
      Procedure BorderStyleSizeable; 
      Procedure BorderStyleToolWindow;
      Procedure BorderStyleToolWindowSizeable;

      Function IsBorderStyleDialog : Boolean;
      Function IsBorderStyleSingle : Boolean;
      Function IsBorderStyleNone : Boolean;
      Function IsBorderStyleSizeable : Boolean;
      Function IsBorderStyleToolWindow : Boolean;
      Function IsBorderStyleToolWindowSizeable : Boolean;

      Procedure WindowMaximise; 
      Procedure WindowMinimise; 
      Procedure WindowNormal; 

      Function IsWindowMaximise : Boolean; 
      Function IsWindowMinimise : Boolean; 
      Function IsWindowNormal : Boolean;

      Procedure DeclarePrimaryControl(oControl : TWinControl); 

      Property AnchoredRight : Boolean Read GetAnchoredRight Write SetAnchoredRight;
      Property AnchoredLeft : Boolean Read GetAnchoredLeft Write SetAnchoredLeft;
      Property AnchoredTop : Boolean Read GetAnchoredTop Write SetAnchoredTop;
      Property AnchoredBottom : Boolean Read GetAnchoredBottom Write SetAnchoredBottom;

      Property BorderIconMinimise : Boolean Read GetBorderIconMinimise Write SetBorderIconMinimise;
      Property BorderIconMaximise : Boolean Read GetBorderIconMaximise Write SetBorderIconMaximise;
      Property BorderIconHelp : Boolean Read GetBorderIconHelp Write SetBorderIconHelp;
      Property BorderIconSystem : Boolean Read GetBorderIconSystem Write SetBorderIconSystem;

      Property ClientPanel : TUixPanel Read pnlClient;
      Property BottomPanel : TUixPanel Read pnlBottom;
      Property OKButton : TUixButton Read btnOK;
      Property CancelButton : TUixButton Read btnCancel;
      Property TaskTray : Boolean Read FTaskTray Write FTaskTray;
      Property Busy : Boolean Read GetBusy Write SetBusy;
      Property AllowWindowMoveOnClientClick : Boolean Read FAllowWindowMoveOnClientClick Write FAllowWindowMoveOnClientClick;

      Property HasClientPanel : Boolean Read GetHasClientPanel Write SetHasClientPanel;
      Property HasBottomPanel : Boolean Read GetHasBottomPanel Write SetHasBottomPanel;

      Property HasDefaultClientBorderWidth : Boolean Read FHasDefaultClientBorderWidth Write FHasDefaultClientBorderWidth;
      Property DefaultClientBorderWidth : Integer Read FDefaultClientBorderWidth Write FDefaultClientBorderWidth;

      Property HasAutomaticDocking : Boolean Read GetHasAutomaticDocking Write SetHasAutomaticDocking;
      Property HasOKButton : Boolean Read GetHasOKButton Write SetHasOKButton;
      Property HasDefaultOKButton : Boolean Read GetHasDefaultOKButton Write SetHasDefaultOKButton;

      Property HasCancelButton : Boolean Read GetHasCancelButton Write SetHasCancelButton;
      Property HasModalOKButton : Boolean Read FHasModalOKButton Write FHasModalOKButton;
      Property HasModalCancelButton : Boolean Read FHasModalCancelButton Write FHasModalCancelButton;
      Property HasDialogKeyDirection : Boolean Read FHasDialogKeyDirection Write FHasDialogKeyDirection;

      Property MouseWheelHover : Boolean Read FMouseWheelHover Write FMouseWheelHover;

      Property ForceClose : Boolean Read FForceClose;
      Property FireDeactivateEventOnApplicationDeactivate : Boolean Read FFireDeactivateEventOnApplicationDeactivate Write FFireDeactivateEventOnApplicationDeactivate;

      Property MoveDelegate : TNotifyEvent Read FMoveDelegate Write FMoveDelegate;
      Property CanAcceptDelegate : TUixFormCanAcceptDelegate Read FCanAcceptDelegate Write FCanAcceptDelegate;
      Property CanRejectDelegate : TUixFormCanRejectDelegate Read FCanRejectDelegate Write FCanRejectDelegate;
      Property CanExecuteDelegate : TUixFormCanExecuteDelegate Read FCanExecuteDelegate Write FCanExecuteDelegate;
      Property CanValidateDelegate : TUixFormCanValidateDelegate Read FCanValidateDelegate Write FCanValidateDelegate;
      Property CloseAcceptDelegate : TUixFormCloseAcceptDelegate Read FCloseAcceptDelegate Write FCloseAcceptDelegate;
      Property CloseQueryDelegate : TUixFormCloseQueryDelegate Read FCloseQueryDelegate Write FCloseQueryDelegate;
      Property DialogKeyDirectionDelegate : TUixFormDialogKeyDirectionDelegate Read FDialogKeyDirectionDelegate Write FDialogKeyDirectionDelegate;
      Property CloseRejectDelegate : TUixFormCloseRejectDelegate Read FCloseRejectDelegate Write FCloseRejectDelegate;
      Property CloseAcceptPrepareDelegate : TUixFormCloseAcceptPrepareDelegate Read FCloseAcceptPrepareDelegate Write FCloseAcceptPrepareDelegate;
      Property CollectCreateParametersDelegate : TUixFormCollectCreateParametersDelegate Read FCollectCreateParametersDelegate Write FCollectCreateParametersDelegate;
      Property AcceptDelegate : TUixFormAcceptDelegate Read FAcceptDelegate Write FAcceptDelegate;
      Property RejectDelegate : TUixFormRejectDelegate Read FRejectDelegate Write FRejectDelegate;
      Property PaintDelegate : TUixFormPaintDelegate Read FPaintDelegate Write FPaintDelegate;

      Property ImplementsPrintMessages : Boolean Read FImplementsPrintMessages Write FImplementsPrintMessages;
      Property BalloonAnchorPosition : TUixBalloonAnchorPosition Read FBalloonAnchorPosition Write FBalloonAnchorPosition;
      Property PopupWindowStyle : Boolean Read GetPopupWindowStyle Write SetPopupWindowStyle;
      Property InternalFormState : TFormState Read GetInternalFormState Write SetInternalFormState;
      Property ParentColor;
  End;

  TUixFormList = Class(TFslPointerList)
    Private
      Function GetFormByIndex(Const iIndex : Integer) : TUixForm;
      Procedure SetFormByIndex(Const iIndex : Integer; Const Value : TUixForm);

      Function GetForm(Const iIndex : Integer) : TUixForm;
      Procedure SetForm(Const iIndex : Integer; Const Value : TUixForm);

    Public
      Procedure Refresh; 
      Procedure Restore; 
      Procedure Commit; 
      Procedure Accept; 
      Procedure Reject; 

      Procedure Show; 
      Procedure Hide;

      Function IndexByClass(Const aClass : TUixFormClass) : Integer;

      Property Forms[Const iIndex : Integer] : TUixForm Read GetForm Write SetForm;
      Property FormByIndex[Const iIndex : Integer] : TUixForm Read GetFormByIndex Write SetFormByIndex; Default;
  End;

  TUixFormClassList = Class(TFslClassList)
    Private
      Function GetClass(Const iIndex : Integer) : TUixFormClass;
      Procedure SetClass(Const iIndex : Integer; Const Value : TUixFormClass);

    Public
      Property ClassByIndex[Const iIndex : Integer] : TUixFormClass Read GetClass Write SetClass; Default;
  End;

  TWinControl = Controls.TWinControl;


Implementation


{$R *.DFM}


Constructor TUixForm.Create(oOwner: TComponent);
Begin
  Inherited;

  PrepareInitialise;
End;


Destructor TUixForm.Destroy;
Begin
  Try
    PrepareFinalise;
  Finally
    Inherited;
  End;
End;


Procedure TUixForm.AfterConstruction;
Begin
  Inherited;

  Initialise;
End;


Procedure TUixForm.BeforeDestruction;
Begin
  Try
    Finalise;
  Finally
    Inherited;
  End;
End;


Procedure TUixForm.CreateParams(Var aParams : TCreateParams);
Begin
  Inherited;

  If FPopupWindowStyle Then
    aParams.Style := aParams.Style Or WS_POPUP;

  If FormStyle = fsStayOnTop Then
    aParams.WndParent := GetDesktopWindow;

  If Assigned(FCollectCreateParametersDelegate) Then
    FCollectCreateParametersDelegate(aParams);
End;


Procedure TUixForm.Error(aException: EFslExceptionClass; Const sMethod, sMessage: String);
Begin
  Raise aException.Create(Self, sMethod, sMessage);
End;


Procedure TUixForm.Error(Const sMethod, sMessage: String);
Begin
  Error(EFslException, sMethod, sMessage);
End;


Class Procedure TUixForm.ClassError(Const sMethod, sMessage: String);
Begin
  Raise EFslException.Create(Nil, sMethod, sMessage);
End;


Function TUixForm.Invariant(Const sMethod, sMessage: String): Boolean;
Begin
  // Call this method as you would the Error method to raise an exception.
  // Use this when you are not sure if self is valid as it is a non-virtual method.

  Raise EFslInvariant.Create(Self, sMethod, sMessage); // Can't use Error method here as it is virtual.

  Result := True;
End;


Function TUixForm.Invariants(Const sLocation : String; oForm : TUixForm; aClass : TClass; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, TObject(oForm), aClass, sObject);

  Result := True;
End;


Function TUixForm.Invariants(Const sLocation : String; oObject : TObject; aClass: TClass; Const sObject : String) : Boolean;
Begin
  If Not Assigned(aClass) Then
    Invariant('Invariants', 'aClass was not assigned.');

  // Ensure object is assigned.
  If Not Assigned(oObject) Then
    Invariant(sLocation, sObject + ' was not assigned and was expected to have been of class ' + aClass.ClassName);

  // Ensure object is of the expected class.
  If Not oObject.InheritsFrom(aClass) Then
    Invariant(sLocation, sObject + ' was of class ' + oObject.ClassName + ' and should have been of class ' + aClass.ClassName);

  Result := True;
End;


Function TUixForm.Invariants(Const sLocation : String; oObject : TFslObject; aClass : TFslObjectClass; Const sObject : String) : Boolean;
Begin
  Invariants(sLocation, TObject(oObject), aClass, sObject);

  Result := True;
End;


Function TUixForm.Invariants(Const sLocation: String; aReference, aClass: TClass; Const sReference : String): Boolean;
Begin
  // Ensure class is assigned.
  If Not Assigned(aReference) Then
    Invariant(sLocation, sReference + ' was not assigned and was expected to have been of class type ' + aClass.ClassName);

  // Ensure class is of the expected class.
  If Not aReference.InheritsFrom(aClass) Then
    Invariant(sLocation, sReference + ' was of class type ' + aReference.ClassName + ' and should have been of class type ' + aClass.ClassName);

  Result := True;
End;


Function TUixForm.Invariants(Const sLocation: String; aClass: TClass): Boolean;
Begin
  Invariants(sLocation, Self, aClass, 'Self');

  Result := True;
End;


Function TUixForm.Condition(bCondition: Boolean; Const sLocation, sMessage: String): Boolean;
Begin
  If Not bCondition Then
    Invariant(sLocation, sMessage);
                                                           
  Result := True;
End;


Procedure TUixForm.PrepareInitialise;
Begin
End;


Procedure TUixForm.PrepareFinalise;
Begin
End;


Procedure TUixForm.Initialise;
Begin
  FIsChangingAutomaticDocking := False;

  HasClientPanel := False;
  HasBottomPanel := False;
  HasModalOKButton := True;
  HasModalCancelButton := True;

  DefaultClientBorderWidth := 8;

  OKButton.Title := 'OK';
  CancelButton.Title := 'Cancel';

  FBalloonAnchorPosition := UixBalloonAnchorPositionTopLeft;
End;


Procedure TUixForm.Finalise;
Begin
End;


Function TUixForm.Execute : Boolean;
Begin
  Result := ExecuteStandard;
End;


Function TUixForm.ExecuteProgress : Boolean;
Begin
  // Don't assert here invariants may not be valid until after Restore.

  Busy := True;
  Try
    Restore;
    Try
      Result := ExecuteModal;
    Finally
      Commit;
    End;
  Finally
    Busy := False;
  End;
End;


Function TUixForm.ExecuteStandard : Boolean;
Begin
  // Don't assert here invariants may not be valid until after Restore.

  Busy := True;
  Try
    Restore;
    Try
      RefreshInitialPrepare;
      RefreshInitialExecute;
      RefreshInitialTerminate;

      Result := CanExecute;

      If Result Then
      Begin
        Busy := False;
        Try
          Result := (ShowModal = mrOk);

          // See FormCloseQuery for Accept/Reject
        Finally
          Busy := True;
        End;
      End;
    Finally
      Commit;
    End;
  Finally
    Busy := False;
  End;
End;


Function TUixForm.ExecuteModal : Boolean;
Var
  pWindowList : Pointer;
//  iSaveFocusCount : Integer;
//  aSaveCursor : TCursor;
//  iSaveCount : Integer;
  hActiveWindow : HWnd;
Begin
  // Hacked from Forms.pas to allow refresh after Show, but before VCL loop.

  Result := False;

  Busy := False;
  Try
    CancelDrag;

    If Visible Or Not Enabled Or (fsModal In FFormState) Or (FormStyle = fsMDIChild) Then
      Raise EInvalidOperation.Create(SCannotShowModal);

    If GetCapture <> 0 Then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);

    ReleaseCapture;

    Include(FFormState, fsModal);

    hActiveWindow := GetActiveWindow;
  //  iSaveFocusCount := FocusCount;
  //  Screen.FSaveFocusedList.Insert(0, Screen.FFocusedForm);
  //  Screen.FFocusedForm := Self;
  //  aSaveCursor := Screen.Cursor;
    Screen.Cursor := crDefault;
  //  iSaveCount := Screen.FCursorCount;
    pWindowList := DisableTaskWindows(0);
    Try
      Show;
      Try
        SendMessage(Handle, CM_ACTIVATE, 0, 0);

        RefreshInitialPrepare;
        RefreshInitialExecute;
        RefreshInitialTerminate;

        Result := CanExecute;

        If Result Then
        Begin
          ModalResult := 0;

          Repeat
            Application.HandleMessage;

            If Application.Terminated Then
              ModalResult := mrCancel
            Else If ModalResult <> 0 Then
              CloseModal;
          Until ModalResult <> 0;

          Result := ModalResult = mrOK;

          SendMessage(Handle, CM_DEACTIVATE, 0, 0);

          If GetActiveWindow <> Handle Then
            hActiveWindow := 0;
        End;
      Finally
        Hide;
      End;
    Finally
  (*
      If Screen.FCursorCount = SaveCount Then
        Screen.Cursor := SaveCursor
      Else
        Screen.Cursor := crDefault;
  *)
      EnableTaskWindows(pWindowList);

  (*
      If Screen.FSaveFocusedList.Count > 0 Then
      Begin
        Screen.FFocusedForm := Screen.FSaveFocusedList.First;
        Screen.FSaveFocusedList.Remove(Screen.FFocusedForm);
      End
      Else
      Begin
        Screen.FFocusedForm := Nil;
      End;
  *)
      If hActiveWindow <> 0 Then
        SetActiveWindow(hActiveWindow);

  //    FocusCount := iSaveFocusCount;
      Exclude(FFormState, fsModal);
    End;
  Finally
    Busy := True;
  End;
End;


Procedure TUixForm.Restore;
Begin
End;


Procedure TUixForm.Commit;
Begin
End;


Procedure TUixForm.Accept;
Begin
  If Assigned(FAcceptDelegate) Then
    FAcceptDelegate;
End;


Procedure TUixForm.Reject;
Begin
  If Assigned(FRejectDelegate) Then
    FRejectDelegate;
End;


Procedure TUixForm.RefreshInitialPrepare;
Begin
End;


Procedure TUixForm.RefreshInitialTerminate;
Begin
  If Assigned(FPrimaryControl) Then
    ActiveControl := FPrimaryControl;
End;


Procedure TUixForm.RefreshInitialExecute;
Begin
  Refresh;
End;


Procedure TUixForm.Refresh;
Begin
  RefreshPrepare;
  Try
    RefreshExecute;
  Finally
    RefreshTerminate;
  End;
End;


Procedure TUixForm.RefreshPrepare;
Begin
End;


Procedure TUixForm.RefreshExecute;
Var
  iCurrentRightOffset : Integer;
Begin
  If HasBottomPanel Then
    RefreshBottom;

  If HasClientPanel Then
    RefreshClient;

  If BottomPanel.Visible Then
  Begin
    iCurrentRightOffset := BottomPanel.Width;
    If CancelButton.Visible Then
    Begin
      CancelButton.Left := BottomPanel.Width - CancelButton.Width - 8;
      CancelButton.Top := BottomPanel.Height - CancelButton.Height - 8;

      iCurrentRightOffset := CancelButton.Left;
    End;

    If OKButton.Visible Then
    Begin
      OKButton.Left := iCurrentRightOffset - OKButton.Width - 8;
      OKButton.Top := BottomPanel.Height - OKButton.Height - 8;
    End;
  End;
End;


Procedure TUixForm.RefreshTerminate;
Begin
  RefreshActions;
End;


Procedure TUixForm.RefreshButtons;
Begin
  If FHasModalOKButton Then
    OKButton.ModalOK
  Else
    OKButton.ModalNone;

  If FHasModalCancelButton Then
    CancelButton.ModalCancel
  Else
    CancelButton.ModalNone;
End;


Procedure TUixForm.RefreshClient;
Begin
  If HasDefaultClientBorderWidth Then
    ClientPanel.BorderWidth := DefaultClientBorderWidth;
End;


Procedure TUixForm.RefreshBottom;
Begin
End;


Procedure TUixForm.Okay;
Begin
  If FHasModalOKButton Then
    ModalOK;
End;


Procedure TUixForm.Cancel;
Begin
  If FHasModalCancelButton Then
    ModalCancel;
End;


Procedure TUixForm.Enter;
Begin
End;


Procedure TUixForm.Leave;
Begin
End;


Procedure TUixForm.Flash;
Begin
  FlashWindow(Handle, True);
End;


Procedure TUixForm.Center;
Begin
  SetBounds((Screen.Width - Width) Div 2, (Screen.Height - Height) Div 2, Width, Height);
End;


Procedure TUixForm.Progress;
Begin
  Application.ProcessMessages;
End;


Procedure TUixForm.Show;
Begin
  Inherited;
End;


Procedure TUixForm.Hide;
Begin
  Inherited;
End;


Procedure TUixForm.btnOkClick(oSender : TObject);
Begin
  Okay;
End;


Procedure TUixForm.btnCancelClick(oSender : TObject);
Begin
  Cancel;
End;


Procedure TUixForm.FormCloseQuery(oSender : TObject; Var bCanClose : Boolean);
Begin
  DoCloseQuery(bCanClose);
End;


Procedure TUixForm.FormShow(oSender : TObject);
Begin
  If FTaskTray Then
  Begin
    // Hide the application tray entry
//    ShowWindow(Application.Handle, SW_HIDE);

    // Show the forms tray entry
    SetWindowLong(Handle, GWL_EXSTYLE, GetWindowLong(Handle, GWL_EXSTYLE) Or WS_EX_APPWINDOW);
  End;
End;


Procedure TUixForm.WMEndSession(Var aMessage: TMessage);
Begin
  If Boolean(aMessage.wParam) Then
  Begin
    If (Cardinal(aMessage.lParam) Or ENDSESSION_LOGOFF <> 0) Then
      EndSession
    Else
      Close;
  End;
End;


Procedure TUixForm.WMQueryEndSession(Var aMessage: TMessage);
Begin
  aMessage.Result := Integer(QueryEndSession);
End;


Procedure TUixForm.WMMove(Var aMessage : TMessage);
Begin
  DoMove;
End;


Function TUixForm.QueryEndSession: Boolean;
Begin
  FForceClose := True;

  Result := CloseQuery And CallTerminateProcs;
End;


Function TUixForm.CanAccept : Boolean;
Begin
  Result := True;

  HideBalloon;

  If Assigned(FCanAcceptDelegate) Then
    Result := FCanAcceptDelegate;
End;


Function TUixForm.CanReject : Boolean;
Begin
  Result := True;

  If Assigned(FCanRejectDelegate) Then
    Result := FCanRejectDelegate;
End;


Function TUixForm.CanExecute : Boolean;
Begin
  Result := True;

  If Assigned(FCanExecuteDelegate) Then
    Result := FCanExecuteDelegate;
End;


Function TUixForm.CanValidate : Boolean;
Begin
  Result := Not Assigned(FCanValidateDelegate) Or FCanValidateDelegate;         
End;


Function TUixForm.Accepting : Boolean;
Begin
  Result := IsModalOK;
End;


Function TUixForm.Rejecting : Boolean;
Begin
  Result := Not Accepting;
End;


Procedure TUixForm.Invalid(oControl : TWinControl; Const sTitle, sMessage : String);
Var
  oParent : TWinControl;
Begin
  HideBalloons(Self);

  FBalloon := TUixBalloon.CreateNew(Self);
  FBalloon.Parent := oControl;
  FBalloon.Message := sMessage;
  FBalloon.Title := sTitle;
  FBalloon.Control := oControl;

  oParent := oControl.Parent;

  While Assigned(oParent) Do
  Begin
    If (oParent Is TTabSheet) And (oParent.Parent Is TPageControl) Then
      TPageControl(oParent.Parent).ActivePage := TTabSheet(oParent);

    oParent := oParent.Parent;
  End;

  If Assigned(oControl) And oControl.Enabled And oControl.Visible And oControl.CanFocus Then
  Begin
    Try
      GetParentForm(oControl).ActiveControl := oControl;
    Except
      // there's not a lot you can do, and there's certainly no reason to propagate this. GDG 12/04/2005
    End;
  End;

  FBalloon.TimeoutDuration := 8000;
  FBalloon.BalloonTypeError;
  FBalloon.AnchorPosition := FBalloonAnchorPosition;
  FBalloon.ControlRelativeHorizontalPositionCenter;
  FBalloon.ControlRelativeVerticalPositionBottom;
  FBalloon.ShowBalloon;
End;


Procedure TUixForm.HideBalloon;
Begin
  If Assigned(FBalloon) Then
    FBalloon.Hide;

  FBalloon.Free;
  FBalloon := Nil;
End;


Procedure TUixForm.HideClientBalloons;
Begin
  HideBalloon;
  If Assigned(ClientPanel) Then
    HideBalloons(ClientPanel);
End;


Procedure TUixForm.HideBalloons(oWinControl : TWinControl);
Var
  iLoop : Integer;
  oControl : TControl;
Begin
  For iLoop := 0 To oWinControl.ControlCount - 1 Do
  Begin
    oControl := oWinControl.Controls[iLoop];

    If oControl Is TPanel Then
      HideBalloons(TPanel(oControl))
    Else If oControl Is TUixForm Then
      TUixForm(oControl).HideClientBalloons
    Else If oControl Is TUixCodeEdit Then
      TUixCodeEdit(oControl).HideBalloon
    Else If oControl Is TUixComboBox Then
      TUixComboBox(oControl).HideBalloon;
  End;
End;


Procedure TUixForm.EndSession;
Begin
End;


Procedure TUixForm.DoMove;
Begin
  HideClientBalloons;

  If Assigned(FMoveDelegate) Then
    FMoveDelegate(Self);
End;


Procedure TUixForm.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixForm.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixForm.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixForm.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixForm.AlignTop;
Begin
  Align := alTop;
End;


Function TUixForm.GetBusy : Boolean;
Begin
  Result := FBusy;
End;


Procedure TUixForm.SetBusy(Const Value : Boolean);
Begin
  FBusy := Value;

  If FBusy Then
    Screen.Cursor := crHourGlass
  Else
    Screen.Cursor := crDefault;
End;


Procedure TUixForm.SetParent(oParent : TWinControl);
Begin
  Inherited;

  If FHasAutomaticDocking And Not FIsChangingAutomaticDocking Then
  Begin
    FIsChangingAutomaticDocking := True;
    Try
      ManualDock(oParent);

      If Assigned(oParent) Then
        Visible := True;
    Finally
      FIsChangingAutomaticDocking := False;
    End;
  End;
End;


Procedure TUixForm.BorderStyleDialog;
Begin
  BorderStyle := bsDialog;
End;


Procedure TUixForm.BorderStyleNone;
Begin
  BorderStyle := bsNone;
End;


Procedure TUixForm.BorderStyleSingle;
Begin
  BorderStyle := bsSingle;
End;


Procedure TUixForm.BorderStyleSizeable;
Begin
  BorderStyle := bsSizeable;
End;


Procedure TUixForm.BorderStyleToolWindow;
Begin
  BorderStyle := bsToolWindow;
End;


Procedure TUixForm.BorderStyleToolWindowSizeable;
Begin
  BorderStyle := bsSizeToolWin;
End;


Function TUixForm.IsBorderStyleDialog : Boolean;
Begin
  Result := BorderStyle = bsDialog;
End;


Function TUixForm.IsBorderStyleNone : Boolean;
Begin
  Result := BorderStyle = bsNone;
End;


Function TUixForm.IsBorderStyleSingle : Boolean;
Begin
  Result := BorderStyle = bsSingle;
End;


Function TUixForm.IsBorderStyleSizeable : Boolean;
Begin
  Result := BorderStyle = bsSizeable;
End;


Function TUixForm.IsBorderStyleToolWindow : Boolean;
Begin
  Result := BorderStyle = bsToolWindow;
End;


Function TUixForm.IsBorderStyleToolWindowSizeable : Boolean;
Begin
  Result := BorderStyle = bsSizeToolWin;
End;


Procedure TUixForm.PositionDesktop;
Begin
  Position := poDesktopCenter;
End;


Procedure TUixForm.PositionDesigned;
Begin
  Position := poDesigned;
End;


Procedure TUixForm.PositionMainForm;
Begin
  Position := poMainFormCenter;
End;


Procedure TUixForm.PositionOwner;
Begin
  Position := poOwnerFormCenter;
End;


Procedure TUixForm.PositionScreen;
Begin
  Position := poScreenCenter;
End;


Function TUixForm.IsPositionDesktop : Boolean;
Begin
  Result := Position = poDesktopCenter;
End;


Function TUixForm.IsPositionDesigned : Boolean;
Begin
  Result := Position = poDesigned;
End;


Function TUixForm.IsPositionMainForm : Boolean;
Begin
  Result := Position = poMainFormCenter;
End;


Function TUixForm.IsPositionOwner : Boolean;
Begin
  Result := Position = poOwnerFormCenter;
End;


Function TUixForm.IsPositionScreen : Boolean;
Begin
  Result := Position = poScreenCenter;
End;


Function TUixForm.GetAnchoredBottom : Boolean;
Begin
  Result := akBottom In Anchors;
End;


Function TUixForm.GetAnchoredLeft : Boolean;
Begin
  Result := akLeft In Anchors;
End;


Function TUixForm.GetAnchoredRight : Boolean;
Begin
  Result := akRight In Anchors;
End;


Function TUixForm.GetAnchoredTop : Boolean;
Begin
  Result := akTop In Anchors;
End;


Procedure TUixForm.SetAnchoredBottom(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akBottom]
  Else
    Anchors := Anchors - [akBottom];
End;


Procedure TUixForm.SetAnchoredLeft(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akLeft]
  Else
    Anchors := Anchors - [akLeft];
End;


Procedure TUixForm.SetAnchoredRight(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akRight]
  Else
    Anchors := Anchors - [akRight];
End;


Procedure TUixForm.SetAnchoredTop(Const Value : Boolean);
Begin
  If Value Then
    Anchors := Anchors + [akTop]
  Else
    Anchors := Anchors - [akTop];
End;


Function TUixForm.CloseAccept : Boolean;
Begin
  If Assigned(FCloseAcceptPrepareDelegate) Then
    FCloseAcceptPrepareDelegate;

  HideClientBalloons;

  Result := Not CanValidate Or (CanAccept And Not Rejecting);

  If Result Then
  Begin
    Busy := True;
    Try
      Accept;
    Finally
      Busy := False;
    End;
  End;

  If Result And Assigned(FCloseAcceptDelegate) Then
    Result := FCloseAcceptDelegate;
End;


Function TUixForm.CloseReject : Boolean;
Begin
  Result := CanReject And Not Accepting;

  If Result Then
  Begin
    Busy := True;
    Try
      Reject;
    Finally
      Busy := False;
    End;
  End;

  If Result And Assigned(FCloseRejectDelegate) Then
    Result := FCloseRejectDelegate;
End;


Procedure TUixForm.DoCloseQuery(Var bCanClose : Boolean);
Begin
  bCanClose := False;

  If Accepting Then
    bCanClose := CloseAccept;

  If Rejecting And Not bCanClose Then
    bCanClose := CloseReject;

  If Assigned(FCloseQueryDelegate) Then
    FCloseQueryDelegate(FForceClose, bCanClose);
End;


Function TUixForm.GetBorderIconHelp: Boolean;
Begin
  Result := biHelp In BorderIcons;
End;


Function TUixForm.GetBorderIconMaximise: Boolean;
Begin
  Result := biMaximize In BorderIcons;
End;


Function TUixForm.GetBorderIconMinimise: Boolean;
Begin
  Result := biMinimize In BorderIcons;
End;


Function TUixForm.GetBorderIconSystem: Boolean;
Begin
  Result := biSystemMenu In BorderIcons;
End;


Procedure TUixForm.SetBorderIconHelp(Const Value: Boolean);
Begin
  If Value Then
    BorderIcons := BorderIcons + [biHelp]
  Else
    BorderIcons := BorderIcons - [biHelp];
End;


Procedure TUixForm.SetBorderIconMaximise(Const Value: Boolean);
Begin
  If Value Then
    BorderIcons := BorderIcons + [biMaximize]
  Else
    BorderIcons := BorderIcons - [biMaximize];
End;


Procedure TUixForm.SetBorderIconMinimise(Const Value: Boolean);
Begin
  If Value Then
    BorderIcons := BorderIcons + [biMinimize]
  Else
    BorderIcons := BorderIcons - [biMinimize];
End;


Procedure TUixForm.SetBorderIconSystem(Const Value: Boolean);
Begin
  If Value Then
    BorderIcons := BorderIcons + [biSystemMenu]
  Else
    BorderIcons := BorderIcons - [biSystemMenu];
End;


Procedure TUixForm.WindowMaximise;
Begin
  WindowState := wsMaximized;
End;


Procedure TUixForm.WindowMinimise;
Begin
  WindowState := wsMinimized;
End;


Procedure TUixForm.WindowNormal;
Begin
  WindowState := wsNormal;
End;


Function TUixForm.IsWindowMaximise: Boolean;
Begin
  Result := WindowState = wsMaximized;
End;


Function TUixForm.IsWindowMinimise: Boolean;
Begin
  Result := WindowState = wsMinimized;
End;


Function TUixForm.IsWindowNormal: Boolean;
Begin
  Result := WindowState = wsNormal;
End;


Procedure TUixForm.ModalCancel;
Begin
  ModalResult := mrCancel;
End;


Procedure TUixForm.ModalOK;
Begin
  ModalResult := mrOK;
End;


Procedure TUixForm.ModalNone;
Begin
  ModalResult := mrNone;
End;


Function TUixForm.IsModalOK : Boolean;
Begin
  Result := ModalResult = mrOK;
End;


Function TUixForm.IsModalCancel : Boolean;
Begin
  Result := ModalResult = mrCancel;
End;


Procedure TUixForm.CloseModal;
Var
  aCloseAction : TCloseAction;
Begin
  Try
    aCloseAction := caNone;

    If CloseQuery Then
    Begin
      aCloseAction := caHide;

      DoClose(aCloseAction);
    End;

    Case aCloseAction Of
      caNone : ModalResult := 0;
      caFree : Release;
    End;
  Except
    ModalResult := 0;
    
    Application.HandleException(Self);
  End;
End;


Procedure TUixForm.RefreshActions;
Begin
  RefreshButtons;
End;


Function TUixForm.IsAlignedBottom : Boolean;
Begin
  Result := Align = alBottom;
End;


Function TUixForm.IsAlignedHorizontal : Boolean;
Begin
  Result := IsAlignedTop Or IsAlignedBottom Or IsAlignedClient;
End;


Function TUixForm.IsAlignedLeft : Boolean;
Begin
  Result := Align = alLeft;
End;


Function TUixForm.IsAlignedRight : Boolean;
Begin
  Result := Align = alRight;
End;


Function TUixForm.IsAlignedClient : Boolean;
Begin
  Result := Align = alClient;
End;


Function TUixForm.IsAlignedTop : Boolean;
Begin
  Result := Align = alTop;
End;


Function TUixForm.IsAlignedVertical : Boolean;
Begin
  Result := IsAlignedLeft Or IsAlignedRight Or IsAlignedClient;
End;


Function TUixForm.AttemptAccept : Boolean;
Begin
  Result := CanAccept;
End;


Function TUixForm.AttemptReject : Boolean;
Begin
  Result := CanReject;
End;


Procedure TUixForm.Embed(Const oParent : TWinControl);
Begin
  Parent := oParent;

  ManualDock(oParent);

  Visible := True;
End;


Function TUixForm.IsEmbedded : Boolean;
Begin
  Result := Assigned(Parent);
End;


Procedure TUixForm.ShuffleBottom;
Begin
  Top := MaxInt;
End;


Procedure TUixForm.ShuffleLeft;
Begin
  Left := 0;
End;


Procedure TUixForm.ShuffleRight;
Begin
  Left := MaxInt;
End;


Procedure TUixForm.ShuffleTop;
Begin
  Top := 0;
End;


Function TUixFormList.IndexByClass(Const aClass: TUixFormClass): Integer;
Begin
  Result := Count - 1;
  While (Result >= 0) And (FormByIndex[Result].ClassType <> aClass) Do
    Dec(Result);
End;


Procedure TUixFormList.Refresh;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Refresh;
End;


Procedure TUixFormList.Restore;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Restore;
End;


Procedure TUixFormList.Commit;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Commit;
End;


Procedure TUixFormList.Accept;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Accept;
End;


Procedure TUixFormList.Reject;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Reject;
End;


Function TUixFormList.GetFormByIndex(Const iIndex : Integer) : TUixForm;
Begin
  Result := TUixForm(Inherited PointerByIndex[iIndex]);
End;


Procedure TUixFormList.SetFormByIndex(Const iIndex: Integer; Const Value: TUixForm);
Begin
  Inherited PointerByIndex[iIndex] := Value;
End;


Function TUixFormClassList.GetClass(Const iIndex : Integer) : TUixFormClass;
Begin
  Result := TUixFormClass(Inherited ClassByIndex[iIndex]);
End;


Procedure TUixFormClassList.SetClass(Const iIndex : Integer; Const Value : TUixFormClass);
Begin
  Inherited ClassByIndex[iIndex] := Value;
End;


Procedure TUixForm.DeclarePrimaryControl(oControl: TWinControl);
Begin
  FPrimaryControl := oControl;
End;


Procedure TUixFormList.Hide;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Hide;
End;


Procedure TUixFormList.Show;
Var
  iLoop : Integer;
Begin
  For iLoop := 0 To Count - 1 Do
    FormByIndex[iLoop].Show;
End;


Function TUixFormList.GetForm(Const iIndex : Integer) : TUixForm;
Begin
  Result := FormByIndex[iIndex];
End;


Procedure TUixFormList.SetForm(Const iIndex : Integer; Const Value : TUixForm);
Begin
  FormByIndex[iIndex] := Value;
End;


Procedure TUixForm.CMDialogKey(Var aMessage : TCMDialogKey);
Var
  iFactorX : Integer;
  iFactorY : Integer;
Begin
  If aMessage.CharCode = vkTab Then
    HideClientBalloons;

  If HasDialogKeyDirection Then
  Begin
    iFactorX := 0;
    iFactorY := 0;

    Case aMessage.CharCode Of
      vkLeft : iFactorX := -1;
      vkRight : iFactorX := 1;
      vkDown : iFactorY := 1;
      vkUp : iFactorY := -1;
    End;

    If (iFactorX <> 0) Or (iFactorY <> 0) Then
      DialogKeyDirection(iFactorX, iFactorY);
  End
  Else
  Begin
    Inherited;
  End;
End;


Procedure TUixForm.WMNCHitTest(Var aMessage : TWMNCHitTest);
Begin
  Inherited;

  If AllowWindowMoveOnClientClick And (aMessage.Result = htClient) Then
    aMessage.Result := htCaption;
End;


Procedure TUixForm.Paint;
Begin
  Inherited;

  If Assigned(FPaintDelegate) Then
    FPaintDelegate;
End;


Function TUixForm.GetHasClientPanel: Boolean;
Begin
  Result := ClientPanel.Visible;
End;


Procedure TUixForm.SetHasClientPanel(Const Value: Boolean);
Begin
  ClientPanel.Visible := Value;
End;


Function TUixForm.GetHasBottomPanel : Boolean;
Begin
  Result := BottomPanel.Visible;
End;


Procedure TUixForm.SetHasBottomPanel(Const Value : Boolean);
Begin
  BottomPanel.Visible := Value;
End;


Function TUixForm.GetHasOKButton: Boolean;
Begin
  Result := OKButton.Visible;
End;


Procedure TUixForm.SetHasOKButton(Const Value: Boolean);
Begin
  OKButton.Visible := Value;
End;


Function TUixForm.GetHasDefaultOKButton: Boolean;
Begin
  Result := OKButton.Default;
End;


Procedure TUixForm.SetHasDefaultOKButton(Const Value: Boolean);
Begin
  OKButton.Default := Value;
End;


Function TUixForm.GetHasCancelButton: Boolean;
Begin
  Result := CancelButton.Visible;
End;


Procedure TUixForm.SetHasCancelButton(Const Value: Boolean);
Begin
  CancelButton.Visible := Value;
End;


Function TUixForm.IsModalNone: Boolean;
Begin
  Result := ModalResult = mrNone;
End;


Function TUixForm.GetHasAutomaticDocking : Boolean;
Begin
  Result := FHasAutomaticDocking;
End;


Procedure TUixForm.SetHasAutomaticDocking(Const Value : Boolean);
Begin
  If FHasAutomaticDocking <> Value Then
  Begin
    FHasAutomaticDocking := Value;

    If Assigned(Parent) Then
    Begin
      If FHasAutomaticDocking Then
      Begin
        ManualDock(Parent);

        Visible := True;
      End
      Else
      Begin
        ManualFloat(Classes.Rect(0, 0, 0, 0));

        Visible := False;
      End;
    End;
  End;
End;


Function TUixForm.GetPopupWindowStyle : Boolean;
Begin
  Result := FPopupWindowStyle;
End;


Procedure TUixForm.SetPopupWindowStyle(Const Value: Boolean);
Begin
  If Value <> FPopupWindowStyle Then
  Begin
    FPopupWindowStyle := Value;

    If HandleAllocated Then
      RecreateWnd;
  End;
End;


Procedure TUixForm.DialogKeyDirection(Const iFactorX, iFactorY : Integer);
Begin
  If Assigned(FDialogKeyDirectionDelegate) Then
    FDialogKeyDirectionDelegate(iFactorX, iFactorY);
End;


Function TUixForm.GetInternalFormState : TFormState;
Begin
  Result := FFormState;
End;


Procedure TUixForm.SetInternalFormState(Const Value : TFormState);
Begin
  FFormState := Value;
End;


Procedure TUixForm.WMPrint(Var aMessage : TMessage);
Begin
  If FImplementsPrintMessages Then
    PaintTo(HDC(aMessage.WParam), 0, 0);
End;


Procedure TUixForm.WndProc(Var aMessage : TMessage);
Begin
  Inherited;

  If FFireDeactivateEventOnApplicationDeactivate And (aMessage.Msg = WM_ACTIVATEAPP) Then
  Begin
    If Not TWMActivateApp(aMessage).Active Then
      Deactivate;
  End;
End;


Procedure TUixForm.MouseWheelHandler(Var aMessage: TMessage);
Var
  aMouseClientPosition : Tpoint;
  aMouseScreenPosition : Tpoint;
  oControl : TControl;
  oWinControl : TWinControl;
Begin
  If MouseWheelHover Then
  Begin
    aMouseScreenPosition := SmallPointToPoint(TCMMouseWheel(aMessage).Pos);
    aMouseClientPosition := ScreenToClient(aMouseScreenPosition);

    oControl := ControlAtPos(aMouseClientPosition, False, True);
    oWinControl := Nil;
    
    While Assigned(oControl) Do
    Begin
      If oControl Is TWinControl Then
      Begin
        oWinControl := TWinControl(oControl);

        aMouseClientPosition := oWinControl.ScreenToClient(aMouseScreenPosition);

        oControl := oWinControl.ControlAtPos(aMouseClientPosition, False, True);
      End
      Else
      Begin
        oControl := Nil;
      End;
    End;

    If Assigned(oWinControl) Then
      aMessage.Result := oWinControl.Perform(CM_MOUSEWHEEL, aMessage.WParam, aMessage.LParam);

    If aMessage.Result = 0 Then
      Inherited MouseWheelHandler(aMessage);
  End
  Else
  Begin
    Inherited MouseWheelHandler(aMessage);
  End;
End;


End.

