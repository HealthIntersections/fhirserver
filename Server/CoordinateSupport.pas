Unit CoordinateSupport;


{! 8 !}


Interface


Uses
  Windows,
  MathSupport;


Type
  TCoordinateCartesian = Record
    X : Real;
    Y : Real;
  End;

  TCoordinatePolar = Record
    Radius : Real;
    Theta : Real;
  End;

  TPoint = Windows.TPoint;


Function CoordinateCartesianToPolar(Const Coordinate : TCoordinateCartesian) : TCoordinatePolar; Overload;
Function CoordinatePolarToCartesian(Const Coordinate : TCoordinatePolar) : TCoordinateCartesian; Overload;

Function Point(iX, iY : Integer) : TPoint; Overload;
Function PointSum(Const aA, aB : TPoint) : TPoint; Overload;
Function PointSubtract(Const aA, aB : TPoint) : TPoint; Overload;
Function PointSubtract(Const aA : TPoint; iValue : Integer) : TPoint; Overload;
Function PointMultiply(Const aPoint : TPoint; rValue : Real) : TPoint; Overload;
Function PointMultiply(Const aPoint : TPoint; iValue : Integer) : TPoint; Overload;
Function PointEquals(Const aA, aB : TPoint) : Boolean; Overload;


Implementation


Function CoordinateCartesianToPolar(Const Coordinate : TCoordinateCartesian) : TCoordinatePolar;
Begin
  Result.Radius := Sqrt(Sqr(Coordinate.X) + Sqr(Coordinate.Y));
  Result.Theta := ArcTan2(Coordinate.X, Coordinate.Y);
End;


Function CoordinatePolarToCartesian(Const Coordinate : TCoordinatePolar) : TCoordinateCartesian;
Begin
  Result.X := Coordinate.Radius * Cos(Coordinate.Theta);
  Result.Y := Coordinate.Radius * Sin(Coordinate.Theta);
End;


Function Point(iX, iY : Integer) : TPoint;
Begin
  Result.X := iX;
  Result.Y := iY;
End;


Function PointMultiply(Const aPoint : TPoint; iValue : Integer) : TPoint;
Begin
  Result.X := aPoint.X * iValue;
  Result.Y := aPoint.Y * iValue;
End;


Function PointMultiply(Const aPoint : TPoint; rValue : Real) : TPoint;
Begin
  Result.X := Round(aPoint.X * rValue);
  Result.Y := Round(aPoint.Y * rValue);
End;


Function PointSum(Const aA, aB : TPoint) : TPoint;
Begin
  Result.X := aA.X + aB.X;
  Result.Y := aA.Y + aB.Y;
End;


Function PointSubtract(Const aA, aB : TPoint) : TPoint;
Begin
  Result.X := aA.X - aB.X;
  Result.Y := aA.Y - aB.Y;
End;


Function PointSubtract(Const aA : TPoint; iValue : Integer) : TPoint;
Begin
  Result.X := aA.X - iValue;
  Result.Y := aA.Y - iValue;
End;


Function PointEquals(Const aA, aB : TPoint) : Boolean;
Begin
  Result := (aA.X = aB.X) And (aA.Y = aB.Y); 
End;


End. // CoordinateSupport //
