Unit CoordinateSupport;

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
