unit wp_clipboard;

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

{$i fhir.inc}

interface

uses
  Windows, Classes, SysUtils, Graphics, PngImage,
  fsl_base, fsl_collections, fsl_stream, fsl_utilities,
  wp_graphics;

type
  TWPClipboardContentType = (wcctUnknown, wcctText, wcctUnicode, wcctFormattedText, wcctRTF, wcctHTML, wcctNative, wcctODT, wcctJPEG, wcctBitmap, wcctHL7);
  TWPClipboardContentTypes = Set Of TWPClipboardContentType;

  TWPClipboardFormat = Integer;

  TWPClipboard = Class(TFslObject)
    Private
      FOpened : Boolean;

      Procedure LoadContentNames(oNames : TFslStringList);
      Function PickFormat(aContentType : TWPClipboardContentType; Const aFormats : Array Of Integer) : Integer;
      Function PasteFormatForContentType(aContentType : TWPClipboardContentType) : TWPClipboardFormat;
      Function CopyFormatForContentType(aContentType : TWPClipboardContentType) : TWPClipboardFormat;

      Procedure CopyBitmap(oImage : TFslVCLGraphic);
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Open; Overload; Virtual;
      Procedure Close; Overload; Virtual;
      Procedure Clear; Overload; Virtual;

      Function ContentNames : String; Overload; Virtual;
      Function ContentTypes : TWPClipboardContentTypes;  Overload; Virtual;

      Function HasContentType(aContentType : TWPClipboardContentType) : Boolean;  Overload; Virtual;
      Function HasText : Boolean;  Overload; Virtual;
      Function HasUnicode : Boolean;  Overload; Virtual;
      Function HasFormattedText : Boolean;  Overload; Virtual;
      Function HasRTF : Boolean;  Overload; Virtual;
      Function HasODT : Boolean;  Overload; Virtual;
      Function HasHTML : Boolean;  Overload; Virtual;
      Function HasNative : Boolean;  Overload; Virtual;
      Function HasJPEG : Boolean;  Overload; Virtual;
      Function HasBitmap : Boolean;  Overload; Virtual;

      Function CanPaste : Boolean; Overload; Virtual;

      Function TypeForFormat(Const iFormat : Integer) : TWPClipboardContentType;

      Function  PasteContent(aContentType : TWPClipboardContentType) : String; Overload; Virtual;
      Procedure PasteContent(aContentType : TWPClipboardContentType; oBuffer : TFslBuffer; ascii : boolean);  Overload; Virtual;
      Procedure PasteText(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteUnicode(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteFormattedText(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteRTF(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteODT(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteHTML(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteNative(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteJPEG(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PastePNG(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure PasteBitmap(oImage : TFslVCLGraphic);  Overload; Virtual;

      Procedure CopyContent(aContentType : TWPClipboardContentType; oBuffer : TFslBuffer; ascii : boolean);  Overload; Virtual;
      Procedure CopyText(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyHL7(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyUnicode(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyFormattedText(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyRTF(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyODT(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyHTML(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyNative(oBuffer : TFslBuffer);  Overload; Virtual;
      Procedure CopyImage(oImage : TFslVCLGraphic);  Overload; Virtual; // write as many formats as possible
      Procedure CopyText(sContent : String);  Overload; Virtual;
  End; { TWPClipboard }


Const
  WPCLIPBOARDCONTENTTYPE_NAMES : Array[TWPClipboardContentType] Of String =
    ('Unknown', 'Unformatted Text', 'Unicode Text', 'Formatted Text (Plain)', 'Formatted Text (RTF)', 'Formatted Text (HTML)', 'WP Native Format', 'OpenDocument FOrmat', 'Lossy Image', 'Image', 'HL7');


Function Clipboard : TWPClipboard;

// globals

Var
  CF_FORMATTEDTEXT : TWPClipboardFormat = 0;
  CF_RTF : TWPClipboardFormat = 0;
  CF_HL7 : TWPClipboardFormat = 0;
  CF_ODT : TWPClipboardFormat = 0;
  CF_HTML : TWPClipboardFormat = 0;
  CF_NATIVE : TWPClipboardFormat = 0;

  CF_JFIF : TWPClipboardFormat = 0;
  CF_JPEG : TWPClipboardFormat = 0;
  CF_PNG : TWPClipboardFormat = 0;
  CF_GIF : TWPClipboardFormat = 0;

implementation


Var
  gClipboard : TWPClipboard = Nil;


Function Clipboard : TWPClipboard;
Begin { Function Clipboard }
  If Not Assigned(gClipboard) Then
    gClipboard := TWPClipboard.Create;

  Result := gClipboard;
End;  { Function Clipboard }


Constructor TWPClipboard.Create;
Begin
  Inherited;

  FOpened := False;
End;


Destructor TWPClipboard.Destroy;
Begin
  If FOpened Then
    Close;

  Inherited;
End;


Procedure TWPClipboard.Close;
Begin
  if FOpened and not Windows.CloseClipboard then
    {RaiseLastWin32Error};
  FOpened := False;
End;


Procedure TWPClipboard.Open;
Begin
  FOpened := True;
  if not Windows.OpenClipboard(0) then // could be application.handle - but this would bring forms into the picture, prefer not to
    RaiseLastWin32Error;
End;


Procedure TWPClipboard.Clear;
Begin
  Windows.EmptyClipboard;
End;


//-- Content Types -------------------------------------------------------------



Procedure RegisterFormats;
Begin
  CF_FORMATTEDTEXT := RegisterClipboardFormat('Formatted Text');
  CF_RTF := RegisterClipboardFormat('Rich Text Format');
  CF_ODT := RegisterClipboardFormat('OpenDocument Format');
  CF_HTML := RegisterClipboardFormat('HTML Format');
  CF_NATIVE := RegisterClipboardFormat('WP Native Format');
  CF_JFIF := RegisterClipboardFormat('JFIF');
  CF_JPEG := RegisterClipboardFormat('JPEG');
  CF_PNG := RegisterClipboardFormat('PNG');
  CF_GIF := RegisterClipboardFormat('GIF');
  CF_HL7 := RegisterClipboardFormat('HL7');
End;

Procedure TWPClipboard.LoadContentNames(oNames : TFslStringList);
Var
  iFormat : Integer;
  aStr : Array [0..1024] Of Char;
Begin
  iFormat := 0;
  Repeat
    iFormat := EnumClipboardFormats(iFormat);
    If iFormat <> 0 Then
    Begin
      Case iFormat Of
        CF_TEXT : oNames.Add('Text');
        CF_BITMAP : oNames.Add('Bitmap');
        CF_METAFILEPICT : oNames.Add('MetaFile');
        CF_SYLK : oNames.Add('SYLK');
        CF_DIF : oNames.Add('DIF');
        CF_TIFF : oNames.Add('TIFF');
        CF_OEMTEXT : oNames.Add('OEM Text');
        CF_DIB : oNames.Add('Device Independent Bitmap');
        CF_PALETTE : oNames.Add('Palette');
        CF_PENDATA : oNames.Add('Pen Data');
        CF_RIFF : oNames.Add('RIFF');
        CF_WAVE : oNames.Add('Wave');
        CF_UNICODETEXT : oNames.Add('Unicode Text');
        CF_ENHMETAFILE : oNames.Add('Enhanced Metafile');
        CF_HDROP : oNames.Add('H Drop');
        CF_LOCALE : oNames.Add('Locale');
      Else
        If GetClipboardFormatName(iFormat, aStr, 1024) <> 0 Then
          oNames.Add(aStr)
        Else
          oNames.Add(IntegerToString(iFormat));
      End;
    End;
  Until iFormat = 0;
End;

Function TWPClipboard.ContentNames : String;
Var
  oNames : TFslStringList;
Begin
  oNames := TFslStringList.Create;
  Try
    LoadContentNames(oNames);
    oNames.Sorted;
    Result := oNames.AsCSV;
  Finally
    oNames.Free;
  End;
End;


Function TWPClipboard.ContentTypes : TWPClipboardContentTypes;
Var
  iFormat : Integer;
  bLocalOpen : Boolean;
Begin
  bLocalOpen := Not FOpened;
  If bLocalOpen Then
    Open;
  Try
    iFormat := 0;
    Result := [];
    Repeat
      iFormat := EnumClipboardFormats(iFormat);
      // would be nice to have a case statement, but not all are consts
      // would be nice to have a map, but it's a goofy relationship
      If iFormat = CF_TEXT Then
        Include(Result, wcctText)
      Else If iFormat = CF_UNICODETEXT Then
        Include(Result, wcctUnicode)
      Else If iFormat = CF_BITMAP Then
        Include(Result, wcctBitmap)
  //    Else If iFormat = CF_METAFILEPICT Then
  //      Include(result, wcctMetafile)
      Else If iFormat = CF_TIFF Then
        Include(Result, wcctBitmap)
      Else If iFormat = CF_OEMTEXT Then
        Include(Result, wcctText)
      Else If iFormat = CF_DIB Then
        Include(Result, wcctBitmap)
  //    Else If iFormat = CF_ENHMETAFILE Then
  //      Include(result, wcctMetafile)
      Else If iFormat = CF_FORMATTEDTEXT Then
        Include(Result, wcctFormattedText)
      Else If iFormat = CF_RTF Then
        Include(Result, wcctRTF)
      Else If iFormat = CF_ODT Then
        Include(Result, wcctODT)
      Else If iFormat = CF_HTML Then
        Include(Result, wcctHTML)
      Else If iFormat = CF_NATIVE Then
        Include(Result, wcctNative)
      Else If iFormat = CF_JFIF Then
        Include(Result, wcctJPEG)
      Else If iFormat = CF_JPEG Then
        Include(Result, wcctJPEG)
      Else If iFormat = CF_PNG Then
        Include(Result, wcctBitmap)
      Else If iFormat = CF_GIF Then
        Include(Result, wcctBitmap)
      Else If iFormat = CF_HL7 Then
        Include(Result, wcctHL7)
      Else If iFormat <> 0 Then
        Include(Result, wcctUnknown);
    Until iFormat = 0;
  Finally
    If bLocalOpen Then
      Close;
  End;
End;


Function TWPClipboard.HasContentType(aContentType : TWPClipboardContentType) : Boolean;
Begin
  Result := aContentType In ContentTypes
End;

Function TWPClipboard.HasText : Boolean;
Begin
  Result := HasContentType(wcctText);
End;


Function TWPClipboard.HasUnicode : Boolean;
Begin
  Result := HasContentType(wcctUnicode);
End;


Function TWPClipboard.HasFormattedText : Boolean;
Begin
  Result := HasContentType(wcctFormattedText);
End;


Function TWPClipboard.HasRTF : Boolean;
Begin
  Result := HasContentType(wcctRTF);
End;


Function TWPClipboard.HasODT : Boolean;
Begin
  Result := HasContentType(wcctRTF);
End;


Function TWPClipboard.HasHTML : Boolean;
Begin
  Result := HasContentType(wcctHTML);
End;


Function TWPClipboard.HasNative : Boolean;
Begin
  Result := HasContentType(wcctNative);
End;


Function TWPClipboard.HasJPEG : Boolean;
Begin
  Result := HasContentType(wcctJPEG);
End;


Function TWPClipboard.HasBitmap : Boolean;
Begin
  Result := HasContentType(wcctBitmap);
End;


Function TWPClipboard.PickFormat(aContentType : TWPClipboardContentType; Const aFormats : Array Of Integer) : Integer;
Var
  iLoop : Integer;
  iFormat : Integer;
  bFound : Boolean;
Begin
  Result := 0;
  bFound := False;
  iLoop := Low(aFormats);
  While Not bFound And (iLoop <= High(aFormats)) Do
  Begin
    iFormat := 0;
    // double loop, we will try to find the formats in preferred order
    Repeat
      iFormat := EnumClipboardFormats(iFormat);
      bFound := iFormat = aFormats[iLoop];
    Until bFound Or (iFormat = 0);
    If bFound Then
      Result := iFormat;
    Inc(iLoop);
  End;
  If Not bFound Then
    RaiseError('PickFormat', 'Unable to Find a format for '+WPCLIPBOARDCONTENTTYPE_NAMES[aContentType]+' in '+ContentNames);
End;


Function TWPClipboard.PasteFormatForContentType(aContentType : TWPClipboardContentType) : TWPClipboardFormat;
Begin
  Result := 0;
  Case aContentType Of
    wcctUnknown : RaiseError('FetchFormatForContentType', 'Cannot Paste with Unknown Content Type');
    wcctText : Result := PickFormat(wcctText, [CF_TEXT, CF_OEMTEXT, CF_UNICODETEXT]);
    wcctUnicode : Result := PickFormat(wcctUnicode, [CF_UNICODETEXT, CF_TEXT, CF_OEMTEXT]);
    wcctFormattedText : Result := PickFormat(wcctFormattedText, [CF_FORMATTEDTEXT]);
    wcctRTF : Result := PickFormat(wcctRTF, [CF_RTF]);
    wcctODT : Result := PickFormat(wcctODT, [CF_ODT]);
    wcctHTML : Result := PickFormat(wcctHTML, [CF_HTML]);
    wcctNative : Result := PickFormat(wcctNative, [CF_NATIVE]);
    wcctJPEG : Result := PickFormat(wcctJPEG, [CF_JFIF, CF_JPEG]);
    wcctBitmap : Result := PickFormat(wcctBitmap, [CF_BITMAP, CF_PNG, CF_DIB, CF_TIFF, CF_GIF]);
    wcctHL7 : Result := PickFormat(wcctHL7, [CF_HL7]);
  Else
    RaiseError('FetchFormatForContentType', 'Content Type not valid');
  End;
End;


Function TWPClipboard.CopyFormatForContentType(aContentType : TWPClipboardContentType) : TWPClipboardFormat;
Begin
  Result := 0;
  Case aContentType Of
    wcctUnknown : RaiseError('PutFormatForContentType', 'Cannot Copy with Unknown Content Type');
    wcctText : Result := CF_TEXT;
    wcctUnicode : Result := CF_UNICODETEXT;
    wcctFormattedText : Result := CF_FORMATTEDTEXT;
    wcctRTF : Result := CF_RTF;
    wcctODT : Result := CF_ODT;
    wcctHTML : Result := CF_HTML;
    wcctNative : Result := CF_NATIVE;
    wcctJPEG : Result := CF_JPEG; // or should it be JFIF?
    wcctBitmap : Result := CF_BITMAP;
    wcctHL7 : Result := CF_HL7;
  Else
    RaiseError('PutFormatForContentType', 'Content Type not valid');
  End;
End;


//-- Fetch ---------------------------------------------------------------------

Function TWPClipboard.CanPaste : Boolean;
Var
  aContentTypes : TWPClipboardContentTypes;
Begin
  aContentTypes := ContentTypes;
  Result := (aContentTypes <> []) And (aContentTypes <> [wcctUnknown]);
End;


Procedure TWPClipboard.PasteContent(aContentType : TWPClipboardContentType; oBuffer : TFslBuffer; ascii : boolean);
Var
  hData : HGLOBAL;
  ws : WideString;
  s : String;
Begin { Procedure TWPClipboard.PasteAsText }
  hData := GetClipboardData(PasteFormatForContentType(aContentType));

  If hData = 0 Then
    oBuffer.Clear
  Else
  Begin
    Try
    {$IFDEF VER130}
      if ascii then
        oBuffer.AsText := PAnsiChar(GlobalLock(hData))
      else
      begin
        ws := PWideChar(GlobalLock(hData));
        s := ws;
        oBuffer.AsText := s;
      end;
    {$ELSE}
      if ascii then
        oBuffer.AsAscii := PAnsiChar(GlobalLock(hData))
      else
        oBuffer.AsText := PChar(GlobalLock(hData));
    {$ENDIF}
    Finally
      GlobalUnlock(hData);
    End;
  End;
End;


Procedure TWPClipboard.PasteText(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctText, oBuffer, true);
End;


Procedure TWPClipboard.PasteUnicode(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctUnicode, oBuffer, false);
End;


Procedure TWPClipboard.PasteFormattedText(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctFormattedText, oBuffer, true);
End;


Procedure TWPClipboard.PasteRTF(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctRTF, oBuffer, true);
End;


Procedure TWPClipboard.PasteODT(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctODT, oBuffer, true);
End;


Procedure TWPClipboard.PasteHTML(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctHTML, oBuffer, true);
End;


Procedure TWPClipboard.PasteNative(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctNative, oBuffer, true);
End;


Procedure TWPClipboard.PasteJPEG(oBuffer : TFslBuffer);
Begin
  PasteContent(wcctJPEG, oBuffer, true);
End;


Procedure TWPClipboard.PasteBitmap(oImage : TFslVCLGraphic);
Var
  Data: THandle;
  Palette: HPALETTE;
Begin
  Assert(CheckCondition(oImage.Handle Is TBitmap, 'PasteBitmap', 'Image is not a bitmap'));

  Data := GetClipboardData(CF_BITMAP);
  Palette := GetClipboardData(CF_PALETTE);
  oImage.Handle.LoadFromClipboardFormat(CF_BITMAP, Data, Palette);
End;


Procedure TWPClipboard.CopyContent(aContentType : TWPClipboardContentType; oBuffer : TFslBuffer; ascii : boolean);
Var
  hData : HGLOBAL;
  pData : Pointer;
  iSize : Integer;
Begin
{$IFDEF VER130}
  iSize := oBuffer.Capacity + 1;

  hData := GlobalAlloc(GMEM_MOVEABLE Or GMEM_DDESHARE, iSize);
  Try
    pData := GlobalLock(hData);
    Try
      ZeroMemory(pData, iSize);
      Move(oBuffer.Data^, pData^, oBuffer.Capacity);
    Finally
      GlobalUnlock(hData);
    End;

    if SetClipboardData(CopyFormatForContentType(aContentType), hData) = 0 then
      RaiseLastWin32Error;
  Except
    GlobalFree(hData);

    Raise;
  End;
{$ELSE}
  if ascii then
    iSize := Length(oBuffer.AsAscii) + 1
  else
    iSize := Length(oBuffer.AsText) * 2 + 2;


  hData := GlobalAlloc(GMEM_MOVEABLE Or GMEM_DDESHARE, iSize);
  Try
    pData := GlobalLock(hData);
    Try
      if ascii then
        Move(PAnsiChar(oBuffer.AsAscii)^, pData^, iSize)
      else
        Move(PChar(oBuffer.AsText)^, pData^, iSize);
    Finally
      GlobalUnlock(hData);
    End;

    SetClipboardData(CopyFormatForContentType(aContentType), hData);
  Except
    GlobalFree(hData);

    Raise;
  End;
{$ENDIF}
End;


Procedure TWPClipboard.CopyText(oBuffer : TFslBuffer);
Begin
  {$IFDEF VER130}
  CopyContent(wcctText, oBuffer, true);
  {$ELSE}
  CopyContent(wcctUnicode, oBuffer, false);
  {$ENDIF}
End;

Procedure TWPClipboard.CopyHL7(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctHL7, oBuffer, true);
End;


Procedure TWPClipboard.CopyUnicode(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctUnicode, oBuffer, false);
End;


Procedure TWPClipboard.CopyFormattedText(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctFormattedText, oBuffer, true);
End;


Procedure TWPClipboard.CopyRTF(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctRTF, oBuffer, true);
End;


Procedure TWPClipboard.CopyODT(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctODT, oBuffer, true);
End;


Procedure TWPClipboard.CopyHTML(oBuffer : TFslBuffer);
Var
  sTemp, sValue, s: AnsiString;
  iStartHtml, iEndHtml, iStartFrag, iEndFrag: Integer;
  iMaxLen, iOffset: Integer;
Begin
  // insert start/end fragment
  sValue := oBuffer.AsAscii;
  iStartFrag := StringFindInsensitive(sValue, '<body>');
  iEndFrag := StringFindInsensitive(sValue, '</body>');
  sTemp := StringCopy(sValue, 1, iStartFrag + 5)
                + #13#10 + '<!--StartFragment-->' + #13#10
                + StringCopy(sValue, iStartFrag + 6, iEndFrag - iStartFrag - 6)
                + #13#10 + '<!--EndFragment-->' + #13#10
                + StringCopy(sValue, iEndFrag, Length(sValue) - iEndFrag + 1);

  // create HTML description header
  iStartFrag := StringFindInsensitive(sTemp, '<!--StartFragment-->');
  iEndFrag := StringFindInsensitive(sTemp, '<!--EndFragment-->');
  iStartHtml := 1;
  iEndHtml := Length(sTemp);
  iMaxLen := Length(IntegerToString(iEndHtml)) + 1;
  // a sample value with no position information
  s := 'Version:0.9' + #13#10
        + 'StartHTML:' + #13#10
        + 'EndHTML:' + #13#10
        + 'StartFragment:' + #13#10
        + 'EndFragment:' + #13#10;
  // the final offset position
  iOffset := iMaxLen * 4 + Length(s);
  sValue := 'Version:0.9' + #13#10
        + 'StartHTML:' + StringPadLeft(IntegerToString(iStartHtml + iOffset), '0', iMaxLen)
        + #13#10 + 'EndHTML:' + StringPadLeft(IntegerToString(iEndHtml + iOffset), '0', iMaxLen)
        + #13#10 + 'StartFragment:' + StringPadLeft(IntegerToString(iStartFrag + iOffset), '0', iMaxLen)
        + #13#10 + 'EndFragment:' + StringPadLeft(IntegerToString(iEndFrag + iOffset), '0', iMaxLen)
        + #13#10 + sTemp;

  oBuffer.AsAscii := sValue;
  CopyContent(wcctHTML, oBuffer, true);
End;

Procedure TWPClipboard.CopyNative(oBuffer : TFslBuffer);
Begin
  CopyContent(wcctNative, oBuffer, true);
End;


Procedure TWPClipboard.CopyBitmap(oImage : TFslVCLGraphic);
Var
  Data: THandle;
  Format: Word;
  Palette: HPALETTE;
Begin
  Open;
  Try
    Palette := 0;
    oImage.Handle.SaveToClipboardFormat(Format, Data, Palette);
    Windows.SetClipboardData(Format, Data);
    If Palette <> 0 Then
      SetClipboardData(CF_PALETTE, Palette);
  Finally
    Close;
  End;
End;

Procedure TWPClipboard.CopyImage(oImage : TFslVCLGraphic);
Begin
  CopyBitmap(oImage);
  // todo - other formats like JPEG if possible
End;



Function TWPClipboard.TypeForFormat(Const iFormat: Integer): TWPClipboardContentType;
Begin
  // would be nice to have a case statement, but not all are consts
  // would be nice to have a map, but it's a goofy relationship
  If iFormat = CF_TEXT Then
    Result := wcctText
  Else If iFormat = CF_UNICODETEXT Then
    Result := wcctUnicode
  Else If iFormat = CF_BITMAP Then
    Result := wcctBitmap
//    Else If iFormat = CF_METAFILEPICT Then
//      result := wcctMetafile
  Else If iFormat = CF_TIFF Then
    Result := wcctBitmap
  Else If iFormat = CF_OEMTEXT Then
    Result := wcctText
  Else If iFormat = CF_DIB Then
    Result := wcctBitmap
//    Else If iFormat = CF_UNICODETEXT Then
//      result := wcctUnicode
//    Else If iFormat = CF_ENHMETAFILE Then
//      result := wcctMetafile
  Else If iFormat = CF_FORMATTEDTEXT Then
    Result := wcctFormattedText
  Else If iFormat = CF_RTF Then
    Result := wcctRTF
  Else If iFormat = CF_ODT Then
    Result := wcctODT
  Else If iFormat = CF_HTML Then
    Result := wcctHTML
  Else If iFormat = CF_NATIVE Then
    Result := wcctNative
  Else If iFormat = CF_JFIF Then
    Result := wcctJPEG
  Else If iFormat = CF_JPEG Then
    Result := wcctJPEG
  Else If iFormat = CF_PNG Then
    Result := wcctBitmap
  Else If iFormat = CF_GIF Then
    Result := wcctBitmap
  Else
    Result := wcctUnknown;
End;


Function TWPClipboard.PasteContent(aContentType: TWPClipboardContentType): String;
Var
  oBuffer : TFslBuffer;
Begin
  oBuffer := TFslBuffer.Create;
  Try
    PasteContent(aContentType, oBuffer, aContentType <> wcctUnicode);
{$IFDEF VER130}
    Result := oBuffer.AsAscii;
{$ELSE}
    Result := oBuffer.AsText;
{$ENDIF}
  Finally
    oBuffer.Free;
  End;
End;


procedure TWPClipboard.CopyText(sContent: String);
var
  oBuffer : TFslBuffer;
begin
  oBuffer := TFslBuffer.Create;
  Try
    oBuffer.AsText := sContent;
    CopyText(oBuffer);
  Finally
    oBuffer.Free;
  End;
end;

procedure TWPClipboard.PastePNG(oBuffer: TFslBuffer);
var
  oImage : TFslBitmapGraphic;
  oPNG : TPngObject;
  oAdaptor : TVCLStream;
  oMem : TFslMemoryStream;
begin
  oImage := TFslBitmapGraphic.create;
  try
    PasteBitmap(oImage);
    oPNG := TPngObject.Create;
    Try
      oPNG.Assign(oImage.Handle);

      oMem := TFslMemoryStream.create;
      try
        oMem.Buffer := oBuffer.Link;
        oAdaptor := TVCLStream.Create;
        Try
          oAdaptor.Stream := oMem.Link;

          oPNG.SaveToStream(oAdaptor);
        Finally
         oAdaptor.Free;
        End;
      finally
        oMem.Free;
      end;
    Finally
      oPng.Free;
    End;

  finally
    oImage.free;
  end;
end;


Initialization
  RegisterFormats;
Finalization
  gClipboard.Free;
  gClipboard := Nil;
end.

