Unit RectSupport;


{! 13 !}


Interface


Uses
  Windows,
  CoordinateSupport, MathSupport;


Type
  TRect = Windows.TRect;
  TPoint = Windows.TPoint;


Function Rect(iLeft, iTop, iRight, iBottom : Integer) : TRect; Overload;
Procedure RectZero(Var aRect : TRect); Overload;
Function RectZero : TRect; Overload;
Function RectEmpty(Const aRect : TRect) : Boolean; Overload;
Function RectEqual(Const A, B : TRect) : Boolean; Overload;

Function RectOffset(Const aRect : TRect; iX, iY : Integer) : TRect; Overload;
Function RectIntersect(Const A, B : TRect) : TRect; Overload;
Function RectSubtract(Const A, B : TRect) : TRect; Overload;
Function RectUnion(Const A, B : TRect) : TRect; Overload;
Function RectHasIntersection(Const A, B : TRect) : Boolean; Overload;
Function RectInflate(Const aRect : TRect; iValue : Integer) : TRect; Overload;
Function RectInflate(Const aRect : TRect; iX, iY : Integer) : TRect; Overload;

Function RectWidth(Const aRect : TRect) : Integer; Overload;
Function RectHeight(Const aRect : TRect) : Integer; Overload;

Function RectHit(Const aRect : TRect; Const aPoint : TPoint) : Boolean; Overload;
Function RectBound(Const aRect, aBoundary : TRect) : TRect; Overload;


Implementation


Function Rect(iLeft, iTop, iRight, iBottom : Integer) : TRect;
Begin
  Result.Left := iLeft;
  Result.Top := iTop;
  Result.Right := iRight;
  Result.Bottom := iBottom;
End;


Procedure RectZero(Var aRect : TRect);
Begin
  SetRectEmpty(Windows.TRect(aRect));
End;


Function RectZero : TRect;
Begin
  RectZero(Result);
End;


Function RectEmpty(Const aRect : TRect) : Boolean;
Begin 
  Result := Windows.IsRectEmpty(Windows.TRect(aRect));
End;  


Function RectEqual(Const A, B : TRect) : Boolean;
Begin 
  Result := Windows.EqualRect(Windows.TRect(A), Windows.TRect(B));
End;  


Function RectOffset(Const aRect : TRect; iX, iY : Integer) : TRect;
Begin 
  Result := aRect;
  OffsetRect(Windows.TRect(Result), iX, iY);
End;  


Function RectIntersect(Const A, B : TRect) : TRect;
Begin
  Windows.IntersectRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;


Function RectSubtract(Const A, B : TRect) : TRect;
Begin
  Windows.SubtractRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;

Function RectUnion(Const A, B : TRect) : TRect;
Begin
  Windows.UnionRect(Windows.TRect(Result), Windows.TRect(A), Windows.TRect(B));
End;



Function RectHasIntersection(Const A, B : TRect) : Boolean;
Var
  aTemp : Windows.TRect;
Begin
  Result := Windows.IntersectRect(aTemp, Windows.TRect(A), Windows.TRect(B));
End;  


Function RectInflate(Const aRect : TRect; iValue : Integer) : TRect;
Begin 
  Result := RectInflate(aRect, iValue, iValue);
End;  


Function RectInflate(Const aRect : TRect; iX, iY : Integer) : TRect;
Begin 
  Result := aRect;
  Windows.InflateRect(Windows.TRect(Result), iX, iY);
End;  


Function RectWidth(Const aRect : TRect) : Integer;
Begin 
  Result := aRect.Right - aRect.Left;
End;  


Function RectHeight(Const aRect : TRect) : Integer;
Begin 
  Result := aRect.Bottom - aRect.Top;
End;


Function RectHit(Const aRect : TRect; Const aPoint : TPoint) : Boolean;
Begin
  Result := Windows.PtInRect(Windows.TRect(aRect), Windows.TPoint(aPoint));
End;


Function RectBound(Const aRect, aBoundary : TRect) : TRect;
Begin
  Result.Left := IntegerMax(aRect.Left, aBoundary.Left);
  Result.Top := IntegerMax(aRect.Top, aBoundary.Top);
  Result.Right := IntegerMin(aRect.Right, aBoundary.Right);
  Result.Bottom := IntegerMin(aRect.Bottom, aBoundary.Bottom);
End;


End. // RectSupport //
