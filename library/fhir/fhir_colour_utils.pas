unit fhir_colour_utils;

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
  SysUtils, Math, SysConst, Graphics,
  fsl_utilities;

type
  TColour = integer;
  THTMLColours = (
        hcAliceblue, hcAntiquewhite, hcAqua, hcAquamarine, hcAzure,
        hcBeige, hcBisque, hcBlack, hcBlanchedalmond, hcBlue,
        hcBlueviolet, hcBrown, hcBurlywood, hcCadetblue, hcChartreuse,
        hcChocolate, hcCoral, hcCornflowerblue, hcCornsilk, hcCrimson,
        hcCyan, hcDarkblue, hcDarkcyan, hcDarkgoldenrod, hcDarkgray,
        hcDarkgreen, hcDarkkhaki, hcDarkmagenta, hcDarkolivegreen, hcDarkorange,
        hcDarkorchid, hcDarkred, hcDarksalmon, hcDarkseagreen, hcDarkslateblue,
        hcDarkslategray, hcDarkturquoise, hcDarkviolet, hcdeeppink, hcDeepskyblue,
        hcDimgray, hcDodgerblue, hcFirebrick, hcFloralwhite, hcForestgreen,
        hcFuchsia, hcGainsboro, hcGhostwhite, hcGold, hcGoldenrod,
        hcGray, hcGreen, hcGreenyellow, hcHoneydew, hcHotpink,
        hcIndianred, hcIndigo, hcIvory, hcKhaki, hcLavendar,
        hcLavenderblush, hcLawngreen, hcLemonchiffon, hcLightblue, hcLightcoral,
        hcLightcyan, hcLightgoldenrodyellow, hcLightgreen, hcLightgrey, hcLightpink,
        hcLightsalmon, hcLightseagreen, hcLightskyblue, hcLightslategray, hcLightsteelblue,
        hcLightyellow, hcLime, hcLimegreen, hcLinen, hcMagenta,
        hcMaroon, hcMediumauqamarine, hcMediumblue, hcMediumorchid, hcMediumpurple,
        hcMediumseagreen, hcMediumslateblue, hcMediumspringgreen, hcMediumturquoise, hcMediumvioletred,
        hcMidnightblue, hcMintcream, hcMistyrose, hcMoccasin, hcNavajowhite,
        hcNavy, hcOldlace, hcOlive, hcOlivedrab, hcOrange,
        hcOrangered, hcOrchid, hcPalegoldenrod, hcPalegreen, hcPaleturquoise,
        hcPalevioletred, hcPapayawhip, hcPeachpuff, hcPeru, hcPink,
        hcPlum, hcPowderblue, hcPurple, hcRed, hcRosybrown,
        hcRoyalblue, hcSaddlebrown, hcSalmon, hcSandybrown, hcSeagreen,
        hcSeashell, hcSienna, hcSilver, hcSkyblue, hcSlateblue,
        hcSlategray, hcSnow, hcSpringgreen, hcSteelblue, hcTan,
        hcTeal, hcThistle, hcTomato, hcTurquoise, hcViolet,
        hcWheat, hcWhite, hcWhitesmoke, hcYellow, hcYellowGreen);

Const
  CURRENCY_MINIMUM = -922337203685477.58;
  CURRENCY_MAXIMUM = 922337203685477.58;

  clTransparent = -1;

  HTML_COLOUR_VALUES : Array [THTMLColours] Of TColour = (
      $00FFF8F0, $00D7EBFA, $00FFFF00, $00D4FF7F, $00FFFFF0,
      $00DCF5F5, $00C4E4FF, $00000000, $00CDEBFF, $00FF0000,
      $00E22B8A, $002A2AA5, $0087B8DE, $00A09E5F, $0000FF7F,
      $001E69D2, $00507FFF, $00ED9564, $00DCF8FF, $003C14DC,
      $00FFFF00, $008B0000, $008B8B00, $000B86B8, $00A9A9A9,
      $00006400, $006BB7BD, $008B008B, $002F6B55, $00008CFF,
      $00CC3299, $0000008B, $007A96E9, $008FBC8F, $008B3D48,
      $004F4F2F, $00D1CE00, $00D30094, $009314FF, $00FFBF00,
      $00696969, $00FF901E, $002222B2, $00F0FAFF, $00228B22,
      $00FF00FF, $00DCDCDC, $00FFF8F8, $0000D7FF, $0020A5DA,
      $00808080, $00008000, $002FFFAD, $00F0FFF0, $00B469FF,
      $005C5CCD, $0082004B, $00F0FFFF, $008CE6F0, $00FAE6E6,
      $00F5F0FF, $0000FC7C, $00CDFAFF, $00E6D8AD, $008080F0,
      $00FFFFE0, $00D2FAFA, $0090EE90, $00D3D3D3, $00C1B6FF,
      $007AA0FF, $00AAB220, $00FACE87, $00998877, $00DEC4B0,
      $00E0FFFF, $0000FF00, $0032CD32, $00E6F0FA, $00FF00FF,
      $00000080, $00AACD66, $00CD0000, $00D355BA, $00D87093,
      $0071B33C, $00EE687B, $009AFA00, $00CCD148, $008515C7,
      $00701919, $00FAFFF5, $00E1E4FF, $00B5E4FF, $00ADDEFF,
      $00800000, $00E6F5FD, $00008080, $00238E68, $0000A5FF,
      $000045FF, $00D670DA, $00AAE8EE, $0098FB98, $00EEEEAF,
      $009370D8, $00D5EFFF, $00B9DAFF, $003F85CD, $00CBC0FF,
      $00DDA0DD, $00E6E0B0, $00800080, $000000FF, $008F8FBC,
      $00E16941, $0013458B, $007280FA, $0060A4F4, $00578B2E,
      $00EEF5FF, $002D52A0, $00C0C0C0, $00EBCE87, $00CD5A6A,
      $00908070, $00FAFAFF, $007FFF00, $00B48246, $008CB4D2,
      $00808000, $00D8BFD8, $004763FF, $00D0E040, $00EE82EE,
      $00B3DEF5, $00FFFFFF, $00F5F5F5, $0000FFFF, $0032CD9A);

  HTML_COLOUR_NAMES : Array [THTMLColours] Of String = (
      'Aliceblue', 'Antiquewhite', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanchedalmond', 'Blue',
      'Blueviolet', 'Brown', 'Burlywood', 'Cadetblue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflowerblue', 'Cornsilk', 'Crimson',
      'Cyan', 'Darkblue', 'Darkcyan', 'Darkgoldenrod', 'Darkgray',
      'Darkgreen', 'Darkkhaki', 'Darkmagenta', 'Darkolivegreen', 'Darkorange',
      'Darkorchid', 'Darkred', 'Darksalmon', 'Darkseagreen', 'Darkslateblue',
      'Darkslategray', 'Darkturquoise', 'Darkviolet', 'deeppink', 'Deepskyblue',
      'Dimgray', 'Dodgerblue', 'Firebrick', 'Floralwhite', 'Forestgreen',
      'Fuchsia', 'Gainsboro', 'Ghostwhite', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Greenyellow', 'Honeydew', 'Hotpink',
      'Indianred', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavenderblush', 'Lawngreen', 'Lemonchiffon', 'Lightblue', 'Lightcoral',
      'Lightcyan', 'Lightgoldenrodyellow', 'Lightgreen', 'Lightgrey', 'Lightpink',
      'Lightsalmon', 'Lightseagreen', 'Lightskyblue', 'Lightslategray', 'Lightsteelblue',
      'Lightyellow', 'Lime', 'Limegreen', 'Linen', 'Magenta',
      'Maroon', 'Mediumauqamarine', 'Mediumblue', 'Mediumorchid', 'Mediumpurple',
      'Mediumseagreen', 'Mediumslateblue', 'Mediumspringgreen', 'Mediumturquoise', 'Mediumvioletred',
      'Midnightblue', 'Mintcream', 'Mistyrose', 'Moccasin', 'Navajowhite',
      'Navy', 'Oldlace', 'Olive', 'Olivedrab', 'Orange',
      'Orangered', 'Orchid', 'Palegoldenrod', 'Palegreen', 'Paleturquoise',
      'Palevioletred', 'Papayawhip', 'Peachpuff', 'Peru', 'Pink',
      'Plum', 'Powderblue', 'Purple', 'Red', 'Rosybrown',
      'Royalblue', 'Saddlebrown', 'Salmon', 'Sandybrown', 'Seagreen',
      'Seashell', 'Sienna', 'Silver', 'Skyblue', 'Slateblue',
      'Slategray', 'Snow', 'Springgreen', 'Steelblue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'Whitesmoke', 'Yellow', 'YellowGreen');

  HTML_COLOUR_TITLES : Array [THTMLColours] Of String = (
      'Alice Blue', 'Antique White', 'Aqua', 'Aquamarine', 'Azure',
      'Beige', 'Bisque', 'Black', 'Blanched Almond', 'Blue',
      'Blue Violet', 'Brown', 'Burlywood', 'Cadet Blue', 'Chartreuse',
      'Chocolate', 'Coral', 'Cornflower Blue', 'Cornsilk', 'Crimson',
      'Cyan', 'Dark Blue', 'Dark Cyan', 'Dark Goldenrod', 'Dark Gray',
      'Dark Green', 'Dark Khaki', 'Dark Magenta', 'Dark Olive Green', 'Dark Orange',
      'Dark Orchid', 'Dark Red', 'Dark Salmon', 'Dark Sea Green', 'Dark Slate Blue',
      'Dark Slate Gray', 'Dark Turquoise', 'Dark Violet', 'Deep Pink', 'Deep Sky Blue',
      'Dim Gray', 'Dodger Blue', 'Firebrick', 'Floral White', 'Forest Green',
      'Fuchsia', 'Gainsboro', 'Ghost White', 'Gold', 'Goldenrod',
      'Gray', 'Green', 'Green Yellow', 'Honeydew', 'Hot Pink',
      'Indian Red', 'Indigo', 'Ivory', 'Khaki', 'Lavendar',
      'Lavender Blush', 'Lawn Green', 'Lemon Chiffon', 'Light Blue', 'Light Coral',
      'Light Cyan', 'Light Goldenrod Yellow', 'Light Green', 'Light Grey', 'Light Pink',
      'Light Salmon', 'Light Sea Green', 'Light Sky Blue', 'Light Slate Gray', 'Light Steel Blue',
      'Light Yellow', 'Lime', 'Lime Green', 'Linen', 'Magenta',
      'Maroon', 'Medium Aquamarine', 'Medium Blue', 'Medium Orchid', 'Medium Purple',
      'Medium Seagreen', 'Medium Slate Blue', 'Medium Spring Green', 'Medium Turquoise', 'Medium Violet Red',
      'Midnight Blue', 'Mint Cream', 'Misty Rose', 'Moccasin', 'Navajo White',
      'Navy', 'Old Lace', 'Olive', 'Olive Drab', 'Orange',
      'Orange Red', 'Orchid', 'Pale Goldenrod', 'Pale Green', 'Pale Turquoise',
      'Pale Violet Red', 'Papaya Whip', 'Peach Puff', 'Peru', 'Pink',
      'Plum', 'Powder Blue', 'Purple', 'Red', 'Rosy Brown',
      'Royal Blue', 'Saddle Brown', 'Salmon', 'Sandy Brown', 'Sea Green',
      'Seashell', 'Sienna', 'Silver', 'Sky Blue', 'Slate Blue',
      'Slate Gray', 'Snow', 'Spring Green', 'Steel Blue', 'Tan',
      'Teal', 'Thistle', 'Tomato', 'Turquoise', 'Violet',
      'Wheat', 'White', 'White Smoke', 'Yellow', 'Yellow Green');

Type
 TColourRatio = Record
    Blue  : Real;
    Green : Real;
    Red   : Real;
    Alpha : Real;
  End;

  TColourParts = Packed Record
    Red : Byte;
    Green : Byte;
    Blue : Byte;
    Alpha : Byte;
  End;

Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function HTMLColourStringToColour(Const sColour : String) : TColour; Overload;
Function StringIsHTMLColour(Const sColour : String) : Boolean; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour; Overload;
Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function ColourToHTMLColourString(Const iColour : TColour) : String; Overload;
Function ColourToHTMLColourTitleString(Const iColour : TColour) : String; Overload;
Function ColourToXMLColourString(Const iColour : TColour) : String; Overload;
Function XMLColourStringToColour(Const sColour : String) : TColour; Overload;
Function XMLColourStringToColourOrDefault(Const sColour : String; Const aDefault : TColour) : TColour; Overload;
Function ColourMakeGrey(iColour : TColour) : TColour; Overload;
Function ColourMultiply(iColour : TColour; Const aRatio : TColourRatio) : TColour; Overload;
Function ColourMultiply(iColour : TColour; Const iRatio : Real) : TColour; Overload;
Function ColourInverse(iColour : TColour) : TColour;
Function ColourToString(iColour : TColour) : String; Overload;


implementation

Function ColourCompose(iRed, iGreen, iBlue, iAlpha : Byte) : TColour;
Begin
  TColourParts(Result).Red := iRed;
  TColourParts(Result).Green := iGreen;
  TColourParts(Result).Blue := iBlue;
  TColourParts(Result).Alpha := iAlpha;
End;

Function ColourMultiply(iColour : TColour; Const iRatio : Real) : TColour;
Begin
  TColourParts(Result).Red := Trunc(RealMin(TColourParts(iColour).Red * iRatio, 255));
  TColourParts(Result).Green := Trunc(RealMin(TColourParts(iColour).Green * iRatio, 255));
  TColourParts(Result).Blue := Trunc(RealMin(TColourParts(iColour).Blue * iRatio, 255));
  TColourParts(Result).Alpha := Trunc(RealMin(TColourParts(iColour).Alpha * iRatio, 255));
End;


Function ColourMultiply(iColour : TColour; Const aRatio : TColourRatio) : TColour;
Begin
  TColourParts(Result).Red := Trunc(RealMin(TColourParts(iColour).Red * aRatio.Red, 255));
  TColourParts(Result).Green := Trunc(RealMin(TColourParts(iColour).Green * aRatio.Green, 255));
  TColourParts(Result).Blue := Trunc(RealMin(TColourParts(iColour).Blue * aRatio.Blue, 255));
  TColourParts(Result).Alpha := Trunc(RealMin(TColourParts(iColour).Alpha * aRatio.Alpha, 255));
End;


Function ColourInverse(iColour : TColour) : TColour;
Begin
  TColourParts(Result).Red := Abs(255 - TColourParts(iColour).Red);
  TColourParts(Result).Green := Abs(255 - TColourParts(iColour).Green);
  TColourParts(Result).Blue := Abs(255 - TColourParts(iColour).Blue);
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;

Function ColourToString(iColour : TColour) : String;
Begin
  {$IFDEF FPC}
  result := Graphics.ColorToString(iColour);
  {$ELSE}
  Result := ColorToString(iColour);
  {$ENDIF}
End;

Function ColourMakeGrey(iColour : TColour) : TColour; Overload;
var
  c : Word;
Begin
  c := Trunc((TColourParts(iColour).Red + TColourParts(iColour).Green + TColourParts(iColour).Blue) / 3);

  TColourParts(Result).Red := c;
  TColourParts(Result).Green := c;
  TColourParts(Result).Blue := c;
  TColourParts(Result).Alpha := TColourParts(iColour).Alpha;
End;

Function HTMLColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Var
  iIndex : Integer;
Begin
  iIndex := StringArrayIndexOf(HTML_COLOUR_NAMES, sColour);

  If iIndex = -1 Then
    iIndex := StringArrayIndexOf(HTML_COLOUR_TITLES, sColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_VALUES[THTMLColours(iIndex)]
  Else
    Result := HTMLEncodedColourStringToColour(sColour, aDefault);
End;


Function HTMLColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLColourStringToColour(sColour, 0);
End;


Function StringIsHTMLColour(Const sColour : String) : Boolean;
Begin
  Result := StringArrayExistsInsensitive(HTML_COLOUR_NAMES, sColour) Or StringArrayExistsInsensitive(HTML_COLOUR_TITLES, sColour) Or
     ((Length(sColour) = 7) And (sColour[1] = '#') And StringIsHexadecimal(Copy(sColour, 2, 2)) And StringIsHexadecimal(Copy(sColour, 4, 2)) And StringIsHexadecimal(Copy(sColour, 6, 2)));
End;

Function HTMLEncodedColourStringToColour(Const sColour : String; Const aDefault : TColour) : TColour;
Begin
  If (Length(sColour) < 7) Or (sColour[1] <> '#') Then
    Result := aDefault
  Else
  Begin
    TColourParts(Result).Red := DecodeHexadecimal(AnsiChar(sColour[2]), AnsiChar(sColour[3]));
    TColourParts(Result).Green := DecodeHexadecimal(AnsiChar(sColour[4]), AnsiChar(sColour[5]));
    TColourParts(Result).Blue := DecodeHexadecimal(AnsiChar(sColour[6]), AnsiChar(sColour[7]));
    TColourParts(Result).Alpha := 0;
  End;
End;


Function HTMLEncodedColourStringToColour(Const sColour : String) : TColour;
Begin
  Result := HTMLEncodedColourStringToColour(sColour, 0);
End;

Function ColourToHTMLEncodedColourString(Const iColour : TColour) : String;
Begin
  Result := '#' + string(EncodeHexadecimal(TColourParts(iColour).Red) + EncodeHexadecimal(TColourParts(iColour).Green) + EncodeHexadecimal(TColourParts(iColour).Blue));
End;


Function ColourToHTMLColourString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_NAMES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;


Function ColourToHTMLColourTitleString(Const iColour : TColour) : String;
Var
  iIndex : Integer;
Begin
  iIndex := IntegerArrayIndexOf(HTML_COLOUR_VALUES, iColour);

  If iIndex > -1 Then
    Result := HTML_COLOUR_TITLES[THTMLColours(iIndex)]
  Else
    Result := ColourToHTMLEncodedColourString(iColour);
End;

Function ColourToXMLColourString(Const iColour : TColour) : String;
Begin
  If TColourParts(iColour).Alpha > 0 Then
    Result := string(EncodeHexadecimal(TColourParts(iColour).Alpha))
  Else
    Result := '';

  Result := '#' + string(EncodeHexadecimal(TColourParts(iColour).Red) + EncodeHexadecimal(TColourParts(iColour).Green) + EncodeHexadecimal(TColourParts(iColour).Blue)) + Result;
End;


Function XMLColourStringToColour(Const sColour : String) : TColour;
Begin
  If (Length(sColour) >= 7) And (sColour[1] = '#') And StringIsHexadecimal(Copy(sColour, 2, Length(sColour))) Then
  Begin
    TColourParts(Result).Red := DecodeHexadecimal(AnsiChar(sColour[2]), AnsiChar(sColour[3]));
    TColourParts(Result).Green := DecodeHexadecimal(AnsiChar(sColour[4]), AnsiChar(sColour[5]));
    TColourParts(Result).Blue := DecodeHexadecimal(AnsiChar(sColour[6]), AnsiChar(sColour[7]));

    If (Length(sColour) >= 9) Then
      TColourParts(Result).Alpha := DecodeHexadecimal(AnsiChar(sColour[8]), AnsiChar(sColour[9]))
    Else
      TColourParts(Result).Alpha := $00;
  End
  Else
  Begin
    Result := HTMLEncodedColourStringToColour(sColour);
  End;
End;


Function XMLColourStringToColourOrDefault(Const sColour : String; Const aDefault : TColour) : TColour;
Begin
  If sColour = '' Then
    Result := aDefault
  Else
    Result := XMLColourStringToColour(sColour);
End;


end.
