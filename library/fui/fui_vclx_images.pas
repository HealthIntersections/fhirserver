Unit fui_vclx_images;

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
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, ImgList,
  fsl_utilities,
  wp_graphics;


Type
  TUixImageIndex = TImageIndex;

  TUixImage = Class(TImage)
    Private
      FHoverEnterDelegate : TNotifyEvent;
      FHoverExitDelegate : TNotifyEvent;

      Function GetBitmap: TBitmap;
      Procedure SetBitmap(Const Value: TBitmap);

      Function GetHasBitmap: Boolean;
      Procedure SetHasBitmap(Const Value: Boolean);

      Procedure CMMouseEnter(Var aMessage : TMessage); Message CM_MOUSEENTER;
      Procedure CMMouseLeave(Var aMessage : TMessage); Message CM_MOUSELEAVE;

    Public
      constructor Create(oOwner : TComponent); Override;

      Procedure AlignClient;
      Procedure AlignLeft;
      Procedure AlignTop;
      Procedure AlignRight;
      Procedure AlignBottom;

      Procedure Clear;

      Procedure LoadBitmapFromResource(Const sResource : String);

      Property HasBitmap : Boolean Read GetHasBitmap Write SetHasBitmap;
      Property Bitmap : TBitmap Read GetBitmap Write SetBitmap;

      Property HoverEnterDelegate : TNotifyEvent Read FHoverEnterDelegate Write FHoverEnterDelegate;
      Property HoverExitDelegate : TNotifyEvent Read FHoverExitDelegate Write FHoverExitDelegate;
  End;

  TUixImages = Class(TImageList)
    Public
      Function AddBitmapGraphic(Const oBitmapGraphic : TFslBitmapGraphic) : Integer;
      Function AddJpegGraphic(Const oJpegGraphic : TFslJpegGraphic) : Integer;
      Procedure PrepareBitmapGraphic(Const iIndex : Integer; oBitmapGraphic : TFslBitmapGraphic);
      Procedure LoadBitmapFromResource(Const sResource : String; Const aMaskColour : TColour);

      Function AddIconGraphic(Const oIconGraphic : TFslIconGraphic) : Integer;
  End;


Procedure Register;


Implementation


Procedure Register;
Begin
  RegisterComponents('Uix', [TUixImage, TUixImages]);
End;


Constructor TUixImage.Create(oOwner: TComponent);
Begin
  Inherited;

  If oOwner Is TWinControl Then
    Parent := TWinControl(oOwner);
End;


Procedure TUixImage.AlignClient;
Begin
  Align := alClient;
End;


Procedure TUixImage.AlignLeft;
Begin
  Align := alLeft;
End;


Procedure TUixImage.AlignRight;
Begin
  Align := alRight;
End;


Procedure TUixImage.AlignTop;
Begin
  Align := alTop;
End;


Procedure TUixImage.AlignBottom;
Begin
  Align := alBottom;
End;


Procedure TUixImage.Clear;
Begin
  Picture := Nil;
End;


Procedure TUixImage.LoadBitmapFromResource(Const sResource: String);
Begin
  HasBitmap := True;
  Bitmap.Canvas.Lock;
  Try
    Bitmap.LoadFromResourceName(SysInit.HInstance, sResource);
  Finally
    Bitmap.Canvas.Unlock;
  End;
End;


Procedure TUixImage.CMMouseEnter(Var aMessage : TMessage);
Begin
  If Assigned(FHoverEnterDelegate) Then
    FHoverEnterDelegate(Self);
End;


Procedure TUixImage.CMMouseLeave(Var aMessage : TMessage);
Begin
  If Assigned(FHoverExitDelegate) Then
    FHoverExitDelegate(Self);
End;                                           


Function TUixImage.GetBitmap: TBitmap;
Begin
  Result := Picture.Bitmap;
End;


Procedure TUixImage.SetBitmap(Const Value: TBitmap);
Begin
  Picture.Bitmap := Value;
End;


Function TUixImage.GetHasBitmap: Boolean;
Begin
  Result := Assigned(Picture.Bitmap);
End;


Procedure TUixImage.SetHasBitmap(Const Value: Boolean);
Var
  oBitmap : TBitmap;
Begin
  If Value Then
  Begin
    oBitmap := TBitmap.Create;
    Try
      // VCL doesn't take ownership of the reference.
      Bitmap := oBitmap;
    Finally
      oBitmap.Free;
    End;
  End
  Else
  Begin
    Bitmap := Nil;
  End;
End;


Function TUixImages.AddBitmapGraphic(Const oBitmapGraphic: TFslBitmapGraphic) : Integer;
Begin
  Result := Add(oBitmapGraphic.Handle, Nil);
End;


Function TUixImages.AddJpegGraphic(Const oJpegGraphic : TFslJpegGraphic) : Integer;
Var
  oBitmapGraphic : TFslBitmapGraphic;
Begin
  oBitmapGraphic := oJpegGraphic.AsBitmap;
  Try
    Result := AddBitmapGraphic(oBitmapGraphic);
  Finally
    oBitmapGraphic.Free;
  End;
End;


Function TUixImages.AddIconGraphic(Const oIconGraphic : TFslIconGraphic) : Integer;
Begin
  Result := AddIcon(oIconGraphic.Icon);
End;


Procedure TUixImages.LoadBitmapFromResource(Const sResource: String; Const aMaskColour: TColour);
Var
  oMasterBitmap : TBitmap;
  oIndividualBitmap : TBitmap;
  iIndex : Integer;
  aTargetRect : TRect;
  aSourceRect : TRect;
Begin
  // Note: FileLoad/ResourceLoad/ResInstLoad seems to only support 16 colour bitmaps.

  oMasterBitmap := TBitmap.Create;
  Try
    oMasterBitmap.Canvas.Lock;
    Try
      oMasterBitmap.LoadFromResourceName(SysInit.HInstance, sResource);

      If (Height > 0) And (Width > 0) And (oMasterBitmap.Height = Height) And (oMasterBitmap.Width Mod Width = 0) Then
      Begin
        oIndividualBitmap := TBitmap.Create;
        Try
          oIndividualBitmap.Canvas.Lock;
          Try
            oIndividualBitmap.Width := Width;
            oIndividualBitmap.Height := Height;

            aTargetRect := wp_graphics.Rect(0, 0, oIndividualBitmap.Width, oIndividualBitmap.Height);
            aSourceRect := aTargetRect;

            For iIndex := 0 To (oMasterBitmap.Width Div Width) - 1 Do
            Begin
              oIndividualBitmap.Canvas.CopyRect(aSourceRect, oMasterBitmap.Canvas, aTargetRect);

              AddMasked(oIndividualBitmap, aMaskColour);

              Inc(aTargetRect.Left, Width);
              Inc(aTargetRect.Right, Width);
            End;
          Finally
            oIndividualBitmap.Canvas.Unlock;          
          End;
        Finally
          oIndividualBitmap.Free;
        End;
      End;
    Finally
      oMasterBitmap.Canvas.Unlock;
    End;
  Finally
    oMasterBitmap.Free;
  End;
End;


Procedure TUixImages.PrepareBitmapGraphic(Const iIndex: Integer; oBitmapGraphic: TFslBitmapGraphic);
Begin
  GetBitmap(iIndex, oBitmapGraphic.Handle);
End;


End.
