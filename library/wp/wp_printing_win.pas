Unit wp_printing_win;

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
  Windows, WinSpool, SysUtils, Graphics, Consts,
  Winapi.GdipObj, Winapi.GdipApi,
  fsl_base, fsl_utilities, fsl_collections, fsl_threads,
  wp_graphics, wp_printing_base, wp_gdiplus;

Type
  TFslPrinterDefinition = Class(TFslName)
    Private
      FPort : String;
      FDriver : String;
      FIsDefault : Boolean;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Assign(oObject : TFslObject); Override;

      Function Link : TFslPrinterDefinition;
      Function Clone : TFslPrinterDefinition;

      Property Driver : String Read FDriver Write FDriver;
      Property Port : String Read FPort Write FPort;
      Property IsDefault : Boolean Read FIsDefault Write FIsDefault;
  End;

  TFslPrinterDefinitionList = Class(TFslNameList)
    Private
      Function GetDefinition(iIndex: Integer): TFslPrinterDefinition;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function Link : TFslPrinterDefinitionList;

      Function GetByName(Const sName : String) : TFslPrinterDefinition; Reintroduce;

      Property Definitions[iIndex : Integer] : TFslPrinterDefinition Read GetDefinition; Default;
  End;



Type
  TFslFontType = (aftUnknown, aftTrueType, aftRaster, aftDevice);
  TFslFontTypeSet = Set Of TFslFontType;

  TFslPrinterFont = Class(TFslName)
    Private
      FPitch : TFslFontPitch;
      FTypes : TFslFontTypeSet;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Procedure Assign(oObject : TFslObject); Override;

      Property Pitch : TFslFontPitch Read FPitch Write FPitch;
      Property Types : TFslFontTypeSet Read FTypes Write FTypes;
  End;

  TFslPrinterFontList = Class (TFslNameList)
    Private
      Function GetFont(iIndex: Integer): TFslPrinterFont;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Procedure Add(Const sName : String; aPitch : TFslFontPitch; aTypes : TFslFontTypeSet); Overload;

      Function GetByName(Const sName : String) : TFslPrinterFont; Reintroduce;

      Function AsCSV : String;

      Procedure Enumerate(aHandle : HDC);

      Property Font[iIndex : Integer] : TFslPrinterFont Read GetFont; Default;
  End;


Const
  TADVFONTTYPE_NAMES : Array [TFslFontType] Of String = ('Unknown', 'TrueType', 'Raster', 'Device');


Function FontTypesToString(aTypes : TFslFontTypeSet) : String; Overload;



Type
  TFslSynchronisedPrinterAPI = Class(TFslObject)
    Protected
      Class Function SynchroniseLock : TFslLock;

    Public
      // From Windows.pas
      Class Function DeleteIC(aHDC : HDC) : Boolean;
      Class Function CreateIC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC;
      Class Function EnumFonts(DC: HDC; lpszFace: PChar; fntenmprc: TFNFontEnumProc; lpszData: PChar): Integer;
      Class Function StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHegiht, SrcX, SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; Var BitsInfo: TBitmapInfo; Usage: UINT; Rop: DWORD): Integer;
      Class Function CreateDC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC;

      // From WinSpool.pas
      Class Function GetPrinter(hPrinter: THandle; Level: DWORD; pPrinter: Pointer; cbBuf: DWORD; pcbNeeded: PDWORD): BOOL;
      Class Function GetPrinterDriver(hPrinter: THandle; pEnvironment: PChar; Level: DWORD; pDriverInfo: Pointer; cbBuf: DWORD; var pcbNeeded: DWORD): BOOL;

      Class Function OpenPrinter(pPrinterName: PChar; Var phPrinter: THandle; pDefault: PPrinterDefaults): BOOL;
      Class Function ClosePrinter(hPrinter: THandle): BOOL;
      {$IFNDEF VER130}
      Class Function DocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; fMode: DWORD): LongInt; Overload;
      {$ENDIF}
      Class Function DocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; Const pDevModeOutput: TDeviceMode; Var pDevModeInput: TDeviceMode; fMode: DWORD): LongInt; Overload;
      Class Function AdvancedDocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; pDevModeOutput, pDevModeInput: PDeviceMode): LongInt;
      Class Function DeviceCapabilities(pDevice, pPort: PChar; fwCapability: Word; pOutput: PChar; DevMode: PDeviceMode): Integer; Overload;
      Class Function EnumJobs(hPrinter: THandle; FirstJob, NoJobs, Level: DWORD; pJob: Pointer; cbBuf: DWORD; Var pcbNeeded, pcReturned: DWORD): BOOL;

      // From Graphics.pas
      Class Procedure GetDIBSizes(Bitmap: HBITMAP; Var InfoHeaderSize: DWORD; Var ImageSize: DWORD);
      Class Function GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; Var BitmapInfo; Var Bits): Boolean;
  End;


Type
  TFslPaperSize = (psUnknown, psCustom,
          psA4, psA5, psA3, psA6,
          psLETTER, psLEGAL,
          psB4, psB5,
          psA3_ROTATED, psA4_ROTATED, psA4SMALL, psA5_ROTATED, psA6_ROTATED,
          psLETTER_ROTATED, psLETTERSMALL,

          ps10X14, ps11X17, ps12X11,
          psCSHEET, psDSHEET, psESHEET,

          psENV_9, psENV_10, psENV_11, psENV_12, psENV_14,
          psENV_C5, psENV_C3, psENV_C4, psENV_C6, psENV_C65, psENV_B4, psENV_B5, psENV_B6,
          psENV_DL, psENV_ITALY, psENV_MONARCH, psENV_PERSONAL,
          psEXECUTIVE, psFOLIO, psLEDGER, psNOTE, psQUARTO, psSTATEMENT, psTABLOID,
          psFANFOLD_US, psFANFOLD_STD_GERMAN, psFANFOLD_LGL_GERMAN,


          psB4_JIS_ROTATED, psB5_JIS_ROTATED, psB6_JIS, psB6_JIS_ROTATED,
          psDBL_JAPANESE_POSTCARD, psDBL_JAPANESE_POSTCARD_ROTATED, psJAPANESE_POSTCARD_ROTATED,
          psJENV_CHOU3, psJENV_CHOU3_ROTATED, psJENV_CHOU4, psJENV_CHOU4_ROTATED, psJENV_KAKU2, psJENV_KAKU2_ROTATED, psJENV_KAKU3, psJENV_KAKU3_ROTATED, psJENV_YOU4, psJENV_YOU4_ROTATED,

          psP16K, psP16K_ROTATED, psP32K, psP32K_ROTATED, psP32KBIG, psP32KBIG_ROTATED,
          psPENV_1, psPENV_1_ROTATED, psPENV_2, psPENV_2_ROTATED, psPENV_3, psPENV_3_ROTATED, psPENV_4, psPENV_4_ROTATED,
          psPENV_5, psPENV_5_ROTATED, psPENV_6, psPENV_6_ROTATED, psPENV_7, psPENV_7_ROTATED, psPENV_8, psPENV_8_ROTATED, psPENV_9, psPENV_9_ROTATED, psPENV_10, psPENV_10_ROTATED);


Const
  ADVPAPERSIZE_CONSTS : Array [TFslPaperSize] Of SmallInt = (
          0, 0,
          DMPAPER_A4, DMPAPER_A5, DMPAPER_A3, DMPAPER_A6,
          DMPAPER_LETTER, DMPAPER_LEGAL,
          DMPAPER_B4, DMPAPER_B5,
          DMPAPER_A3_ROTATED, DMPAPER_A4_ROTATED, DMPAPER_A4SMALL, DMPAPER_A5_ROTATED, DMPAPER_A6_ROTATED,
          DMPAPER_LETTER_ROTATED, DMPAPER_LETTERSMALL,
          DMPAPER_10X14, DMPAPER_11X17, DMPAPER_12X11,
          DMPAPER_CSHEET, DMPAPER_DSHEET, DMPAPER_ESHEET,
          DMPAPER_ENV_9, DMPAPER_ENV_10, DMPAPER_ENV_11, DMPAPER_ENV_12, DMPAPER_ENV_14,
          DMPAPER_ENV_C5, DMPAPER_ENV_C3, DMPAPER_ENV_C4, DMPAPER_ENV_C6, DMPAPER_ENV_C65, DMPAPER_ENV_B4, DMPAPER_ENV_B5, DMPAPER_ENV_B6,
          DMPAPER_ENV_DL, DMPAPER_ENV_ITALY, DMPAPER_ENV_MONARCH, DMPAPER_ENV_PERSONAL,
          DMPAPER_EXECUTIVE, DMPAPER_FOLIO, DMPAPER_LEDGER, DMPAPER_NOTE, DMPAPER_QUARTO, DMPAPER_STATEMENT, DMPAPER_TABLOID,
          DMPAPER_FANFOLD_US, DMPAPER_FANFOLD_STD_GERMAN, DMPAPER_FANFOLD_LGL_GERMAN,
          DMPAPER_B4_JIS_ROTATED, DMPAPER_B5_JIS_ROTATED, DMPAPER_B6_JIS, DMPAPER_B6_JIS_ROTATED,
          DMPAPER_DBL_JAPANESE_POSTCARD, DMPAPER_DBL_JAPANESE_POSTCARD_ROTATED, DMPAPER_JAPANESE_POSTCARD_ROTATED,
          DMPAPER_JENV_CHOU3, DMPAPER_JENV_CHOU3_ROTATED, DMPAPER_JENV_CHOU4, DMPAPER_JENV_CHOU4_ROTATED, DMPAPER_JENV_KAKU2, DMPAPER_JENV_KAKU2_ROTATED, DMPAPER_JENV_KAKU3, DMPAPER_JENV_KAKU3_ROTATED, DMPAPER_JENV_YOU4, DMPAPER_JENV_YOU4_ROTATED,
          DMPAPER_P16K, DMPAPER_P16K_ROTATED, DMPAPER_P32K, DMPAPER_P32K_ROTATED, DMPAPER_P32KBIG, DMPAPER_P32KBIG_ROTATED,
          DMPAPER_PENV_1, DMPAPER_PENV_1_ROTATED, DMPAPER_PENV_2, DMPAPER_PENV_2_ROTATED, DMPAPER_PENV_3, DMPAPER_PENV_3_ROTATED, DMPAPER_PENV_4, DMPAPER_PENV_4_ROTATED, DMPAPER_PENV_5, DMPAPER_PENV_5_ROTATED,
          DMPAPER_PENV_6, DMPAPER_PENV_6_ROTATED, DMPAPER_PENV_7, DMPAPER_PENV_7_ROTATED, DMPAPER_PENV_8, DMPAPER_PENV_8_ROTATED, DMPAPER_PENV_9, DMPAPER_PENV_9_ROTATED, DMPAPER_PENV_10, DMPAPER_PENV_10_ROTATED);

  ADVPAPERSIZE_CODES : Array [TFslPaperSize] Of String = (
    'Unknown',
    'Custom',
    'A4',
    'A5',
    'A3',
    'A6',
    'Letter',
    'Legal',
    'B4',
    'B5',
    'A3R',
    'A4R',
    'A4S',
    'A5R',
    'A6R',
    'LetterR',
    'LetterS',
    '10x14',
    '11x17',
    '12x11',
    'C',
    'D',
    'E',
    'ENV_9',
    'ENV_10',
    'ENV_11',
    'ENV_12',
    'ENV_14',
    'ENV_C5',
    'ENV_C3',
    'ENV_C4',
    'ENV_C6',
    'ENV_C65',
    'ENV_B4',
    'ENV_B5',
    'ENV_B6',
    'ENV_DL',
    'ENV_Italy',
    'Env_Monarch',
    'Env_6_3qr',
    'Executive',
    'Folio',
    'Ledger',
    'Note',
    'Quarto',
    'Statement',
    'Tabloid',
    'US_Fanfold',
    'German_Fanfold',
    'German_Legal_Fanfold',
    'JIS_B4',
    'JIS_B5',
    'JIS_B6',
    'JIS_B6',
    'Double_Japanese_Postcard',
    'Double_Japanese_Postcard_Rotated',
    'Japanese_Postcard_Rotated',
    'Env_Jap_Chou_3',
    'Env_Jap_Chou_3R',
    'Env_Jap_Chou_4',
    'Env_Jap_Chou_4R',
    'Env_Jap_Kaku_2',
    'Env_Jap_Kaku_2R',
    'Env_Jap_Kaku_3',
    'Env_Jap_Kaku_3R',
    'Env_Jap_You_4',
    'Env_Jap_You_4R',
    'PRC_16K',
    'PRC_16KR',
    'PRC_32K',
    'PRC_32KR',
    'PRC_32K_Big',
    'PRC_32K_Big_R',
    'PRC_Env_1',
    'PRC_Env_1R',
    'PRC_Env_2',
    'PRC_Env_2R',
    'PRC_Env_3',
    'PRC_Env_3R',
    'PRC_Env_4',
    'PRC_Env_4R',
    'PRC_Env_5',
    'PRC_Env_5R',
    'PRC_Env_6',
    'PRC_Env_6R',
    'PRC_Env_7',
    'PRC_Env_7R',
    'PRC_Env_8',
    'PRC_Env_8R',
    'PRC_Env_9',
    'PRC_Env_9R',
    'PRC_Env_10',
    'PRC_Env_10R');

  ADVPAPERSIZE_NAMES : Array [TFslPaperSize] Of String = (
    'Unknown',
    'Custom',
    'A4',
    'A5',
    'A3',
    'A6',
    'Letter',
    'Legal',
    'B4',
    'B5',
    'A3 Rotated',
    'A4 Rotated',
    'A4 Small',
    'A5 Rotated',
    'A6 Rotated',
    'Letter Rotated',
    'Letter Small',
    '10 x 14',
    '11 x 17',
    '12 x 11',
    'C Sheet',
    'D Sheet',
    'E Sheet',
    'Envelope #9',
    'Envelope #10',
    'Envelope #11',
    'Envelope #12',
    'Envelope #14',
    'Envelope C5',
    'Envelope C3',
    'Envelope C4',
    'Envelope C6',
    'Envelope C65',
    'Envelope B4',
    'Envelope B5',
    'Envelope B6',
    'Envelope DL',
    'Envelope Italy',
    'Envelope Monarch',
    '6 3/4 Envelope',
    'Executive',
    'Folio',
    'Ledger',
    'Note',
    'Quarto',
    'Statement',
    'Tabloid',
    'US Std Fanfold',
    'German Std Fanfold',
    'German Legal Fanfold',
    'B4 (JIS)',
    'B5 (JIS)',
    'B6 (JIS)',
    'B6 (JIS) Rotated',
    'Japanese Double Postcard',
    'Japanese Double Postcard Roated',
    'Japanese Postcard Rotated',
    'Japanese Envelope Chou #3',
    'Japanese Envelope Chou #3 Rotated',
    'Japan Envelope Chou #4',
    'Japan Envelope Chou #4 Rotated',
    'Japanese Envelope Kaku #2',
    'Japanese Envelope Kaku #2 Rotated',
    'Japanese Envelope Kaku #3',
    'Japanese Envelope Kaku #3 Rotated',
    'Japanese Envelope You #4',
    'Japanese Envelope You #4 Roated',
    'PRC 16K',
    'PRC 16K R',
    'PRC 32K',
    'PRC 32K R',
    'PRC 32K(Big)',
    'PRC 32K(Big) R',
    'PRC Envelope #1',
    'PRC Envelope #1 R',
    'PRC Envelope #2',
    'PRC Envelope #2 R',
    'PRC Envelope #3',
    'PRC Envelope #3 R',
    'PRC Envelope #4',
    'PRC Envelope #4 R',
    'PRC Envelope #5',
    'PRC Envelope #5 R',
    'PRC Envelope #6',
    'PRC Envelope #6 R',
    'PRC Envelope #7',
    'PRC Envelope #8',
    'PRC Envelope #7 R',
    'PRC Envelope #8 R',
    'PRC Envelope #9',
    'PRC Envelope #9 R',
    'PRC Envelope #10',
    'PRC Envelope #10 R');


  ADVPAPERSIZE_TITLES : Array [TFslPaperSize] Of String = (
    'Unknown / Not specified',
    'Custom Size',
    'A4 sheet, 210- by 297-millimeters',
    'A5 sheet, 148- by 210-millimeters',
    'A3 sheet, 297- by 420-millimeters',
    'A6 sheet, 105- by 148-millimeters',
    'Letter, 8 1/2- by 11-inches',
    'Legal, 8 1/2- by 14-inches',
    'B4 sheet, 250- by 354-millimeters',
    'B5 sheet, 182- by 257-millimeter paper',
    'A3 rotated sheet, 420- by 297-millimeters',
    'A4 rotated sheet, 297- by 210-millimeters',
    'A4 small sheet, 210- by 297-millimeters',
    'A5 rotated sheet, 210- by 148-millimeters',
    'A6 rotated sheet, 148- by 105-millimeters',
    'Letter Rotated 11 by 8 1/2 11 inches',
    'Letter Small, 8 1/2- by 11-inches',
    '10- by 14-inch sheet',
    '11- by 17-inch sheet',
    '12- by 11-inch sheet',
    'C Sheet, 17- by 22-inches',
    'D Sheet, 22- by 34-inches',
    'E Sheet, 34- by 44-inches',
    '#9 Envelope, 3 7/8- by 8 7/8-inches',
    '#10 Envelope, 4 1/8- by 9 1/2-inches',
    '#11 Envelope, 4 1/2- by 10 3/8-inches',
    '#12 Envelope, 4 3/4- by 11-inches',
    '#14 Envelope, 5- by 11 1/2-inches',
    'C5 Envelope, 162- by 229-millimeters',
    'C3 Envelope, 324- by 458-millimeters',
    'C4 Envelope, 229- by 324-millimeters',
    'C6 Envelope, 114- by 162-millimeters',
    'C65 Envelope, 114- by 229-millimeters',
    'B4 Envelope, 250- by 353-millimeters',
    'B5 Envelope, 176- by 250-millimeters',
    'B6 Envelope, 176- by 125-millimeters',
    'DL Envelope, 110- by 220-millimeters',
    'Italy Envelope, 110- by 230-millimeters',
    'Monarch Envelope, 3 7/8- by 7 1/2-inches',
    '6 3/4 Envelope, 3 5/8- by 6 1/2-inches',
    'Executive, 7 1/4- by 10 1/2-inches',
    'Folio, 8 1/2- by 13-inch paper',
    'Ledger, 17- by 11-inches',
    'Note, 8 1/2- by 11-inches',
    'Quarto, 215- by 275-millimeter paper',
    'Statement, 5 1/2- by 8 1/2-inches',
    'Tabloid',
    'US Std Fanfold, 14 7/8- by 11-inches',
    'German Std Fanfold, 8 1/2- by 12-inches',
    'German Legal Fanfold, 8 - by 13-inches',
    'B4 (JIS) rotated sheet, 364- by 257-millimeters',
    'B5 (JIS) rotated sheet, 257- by 182-millimeters',
    'B6 (JIS) sheet, 128- by 182-millimeters',
    'B6 (JIS) rotated sheet, 182- by 128-millimeters',
    'Double Japanese Postcard, 200- by 148-millimeters',
    'Double Japanese Postcard Rotated, 148- by 200-millimeters',
    'Japanese Postcard Rotated, 148- by 100-millimeters',
    'Japanese Envelope Chou #3',
    'Japanese Envelope Chou #3 Rotated',
    'Japanese Envelope Chou #4',
    'Japanese Envelope Chou #4 Rotated',
    'Japanese Envelope Kaku #2',
    'Japanese Envelope Kaku #2 Rotated',
    'Japanese Envelope Kaku #3',
    'Japanese Envelope Kaku #3 Rotated',
    'Japanese Envelope You #4',
    'Japanese Envelope You #4 Rotated',
    'PRC 16K, 146- by 215-millimeters',
    'PRC 16K Rotated, 215- by 146-millimeters',
    'PRC 32K, 97- by 151-millimeters',
    'PRC 32K Rotated, 151- by 97-millimeters',
    'PRC 32K(Big) 97- by 151-millimeters',
    'PRC 32K(Big) Rotated, 151- by 97-millimeters',
    'PRC Envelope #1, 102- by 165-millimeters',
    'PRC Envelope #1 Rotated, 165- by 102-millimeters',
    'PRC Envelope #2, 102- by 176-millimeters',
    'PRC Envelope #2 Rotated, 176- by 102-millimeters',
    'PRC Envelope #3, 125- by 176-millimeters',
    'PRC Envelope #3 Rotated, 176- by 125-millimeters',
    'PRC Envelope #4, 110- by 208-millimeters',
    'PRC Envelope #4 Rotated, 208- by 110-millimeters',
    'PRC Envelope #5, 110- by 220-millimeters',
    'PRC Envelope #5 Rotated, 220- by 110-millimeters',
    'PRC Envelope #6, 120- by 230-millimeters',
    'PRC Envelope #6 Rotated, 230- by 120-millimeters',
    'PRC Envelope #7, 160- by 230-millimeters',
    'PRC Envelope #7 Rotated, 230- by 160-millimeters',
    'PRC Envelope #8, 120- by 309-millimeters',
    'PRC Envelope #8 Rotated, 309- by 120-millimeters',
    'PRC Envelope #9, 229- by 324-millimeters',
    'PRC Envelope #9 Rotated, 324- by 229-millimeters',
    'PRC Envelope #10, 324- by 458-millimeters',
    'PRC Envelope #10 Rotated, 458- by 324-millimeters');

  ADVPAPERSIZE_WIDTHS : Array [TFslPaperSize] Of TFslGraphicMetre = (
    0,
    0,
    2100, // A4
    1480, // A5
    2970, // A3
    1050, // A6
    2159, // Letter
    2159, // Legal
    2500, // B4
    1820, // B5
    4200, // A3R
    2970, // A4R
    2100, // A4S
    2100, // A5R
    1480, // A6R
    2794, // LetterR
    2159, // LetterS
    2540, // 10x14
    2794, // 11x17
    3048, // 12x11
    4318, // C
    5588, // D
    8636, // E
    984, // ENV_9
    1048, // ENV_10
    1143, // ENV_11
    1207, // ENV_12
    1270, // ENV_14
    1620, // ENV_C5
    3240, // ENV_C3
    2290, // ENV_C4
    1140, // ENV_C6
    1140, // ENV_C65
    2500, // ENV_B4
    1760, // ENV_B5
    1760, // ENV_B6
    1100, // ENV_DL
    1100, // ENV_Italy
    984, // Env_Monarch
    921, // Env_6_3qr
    1842, // Executive
    2159, // Folio
    4318, // Ledger
    2159, // Note
    2150, // Quarto
    1397, // Statement
    2794, // Tabloid
    3778, // US_Fanfold
    2159, // German_Fanfold
    2032, // German_Legal_Fanfold
    3640, // JIS_B4
    2570, // JIS_B5
    1280, // JIS_B6
    1820, // JIS_B6
    2000, // Double_Japanese_Postcard
    1480, // Double_Japanese_Postcard_Rotated
    1480, // Japanese_Postcard_Rotated
    0, // Env_Jap_Chou_3
    0, // Env_Jap_Chou_3R
    0, // Env_Jap_Chou_4
    0, // Env_Jap_Chou_4R
    0, // Env_Jap_Kaku_2
    0, // Env_Jap_Kaku_2R
    0, // Env_Jap_Kaku_3
    0, // Env_Jap_Kaku_3R
    0, // Env_Jap_You_4
    0, // Env_Jap_You_4R
    1460, // PRC_16K
    2150, // PRC_16KR
    970, // PRC_32K
    1510, // PRC_32KR
    970, // PRC_32K_Big
    1510, // PRC_32K_Big_R
    1020, // PRC_Env_1
    1650, // PRC_Env_1R
    1020, // PRC_Env_2
    1760, // PRC_Env_2R
    1250, // PRC_Env_3
    1760, // PRC_Env_3R
    1100, // PRC_Env_4
    2080, // PRC_Env_4R
    1100, // PRC_Env_5
    2200, // PRC_Env_5R
    1200, // PRC_Env_6
    2300, // PRC_Env_6R
    1600, // PRC_Env_7
    2300, // PRC_Env_7R
    1200, // PRC_Env_8
    3090, // PRC_Env_8R
    2290, // PRC_Env_9
    3240, // PRC_Env_9R
    3240, // PRC_Env_10
    4580); // PRC_Env_10R

  ADVPAPERSIZE_HEIGHTS : Array [TFslPaperSize] Of TFslGraphicMetre = (
    0,
    0,
    2970, // A4
    2100, // A5
    4200, // A3
    1480, // A6
    2794, // Letter
    3556, // Legal
    3450, // B4
    2570, // B5
    2970, // A3R
    2100, // A4R
    2970, // A4S
    1480, // A5R
    1050, // A6R
    2159, // LetterR
    2794, // LetterS
    3556, // 10x14
    4318, // 11x17
    2794, // 12x11
    5588, // C
    8636, // D
    11176, // E
    2254, // ENV_9
    2413, // ENV_10
    2635, // ENV_11
    2794, // ENV_12
    2921, // ENV_14
    2290, // ENV_C5
    4580, // ENV_C3
    3240, // ENV_C4
    1620, // ENV_C6
    2290, // ENV_C65
    3530, // ENV_B4
    2500, // ENV_B5
    1250, // ENV_B6
    2200, // ENV_DL
    2300, // ENV_Italy
    1905, // Env_Monarch
    1651, // Env_6_3qr
    2667, // Executive
    3302, // Folio
    2794, // Ledger
    2794, // Note
    2750, // Quarto
    2159, // Statement
    4318, // Tabloid
    2794, // US_Fanfold
    3048, // German_Fanfold
    3302, // German_Legal_Fanfold
    2570, // JIS_B4
    1820, // JIS_B5
    1280, // JIS_B6
    1820, // JIS_B6
    1480, // Double_Japanese_Postcard
    2000, // Double_Japanese_Postcard_Rotated
    1000, // Japanese_Postcard_Rotated
    0, // Env_Jap_Chou_3
    0, // Env_Jap_Chou_3R
    0, // Env_Jap_Chou_4
    0, // Env_Jap_Chou_4R
    0, // Env_Jap_Kaku_2
    0, // Env_Jap_Kaku_2R
    0, // Env_Jap_Kaku_3
    0, // Env_Jap_Kaku_3R
    0, // Env_Jap_You_4
    0, // Env_Jap_You_4R
    2150, // PRC_16K
    1460, // PRC_16KR
    1510, // PRC_32K
    970, // PRC_32KR
    1510, // PRC_32K_Big
    970, // PRC_32K_Big_R
    1650, // PRC_Env_1
    1020, // PRC_Env_1R
    1760, // PRC_Env_2
    1020, // PRC_Env_2R
    1760, // PRC_Env_3
    1250, // PRC_Env_3R
    2080, // PRC_Env_4
    1100, // PRC_Env_4R
    2200, // PRC_Env_5
    1100, // PRC_Env_5R
    2300, // PRC_Env_6
    1200, // PRC_Env_6R
    2300, // PRC_Env_7
    1600, // PRC_Env_7R
    3090, // PRC_Env_8
    1200, // PRC_Env_8R
    3240, // PRC_Env_9
    2290, // PRC_Env_9R
    4580, // PRC_Env_10
    3240); // PRC_Env_10R


Type
  TFslPrinterPaperSize = Class (TFslName)
    Private
      FSize : TFslPaperSize;
      FHeight : TFslGraphicMetre;
      FWidth : TFslGraphicMetre;

    Protected
      Function GetDimUnit(Var sDim: String; Const rUnit: Real): Real; Overload; Virtual;

      Procedure BuildFromPrinterText(Const sText : String); Overload; Virtual;

      Function AsText : String; Overload; Virtual;

    Public
      Procedure Assign(oObject : TFslObject); Override;

      Property Size : TFslPaperSize Read FSize Write FSize;
      Property Height : TFslGraphicMetre Read FHeight Write FHeight;
      Property Width : TFslGraphicMetre Read FWidth Write FWidth;
  End;

  TFslPrinterPaperSizeList = Class(TFslNameList)
    Private
      Function GetPageSize(iIndex: Integer): TFslPrinterPaperSize;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Function AddFromText(Const sText : String) : Integer; Overload; Virtual;

      Function AsCSV : String; Overload; Virtual;

      Property PageSize[iIndex : Integer] : TFslPrinterPaperSize Read GetPageSize; Default;
  End;



Type

  TFslPrinterCapability = (pcCopies, pcOrientation, pcCollation, pcSizes, pcCustomSize, pcBins, pcQuality, pcColor, pcDuplex);
  TFslPrinterCapabilitySet = Set Of TFslPrinterCapability;

Const
  ADVPRINTERCAPABILITY_NAMES : Array [TFslPrinterCapability] Of String = ('Copies', 'Orientation', 'Collation',
                       'Paper Sizes', 'Custom Paper', 'Bin Selection', 'Print Quality', 'Color', 'Duplexing');

  ADVPRINTERCAPABILITY_VALUES : Array [TFslPrinterCapability] Of DWord = (
                       DM_COPIES, DM_ORIENTATION, DM_COLLATE, DM_PAPERSIZE,
                       DM_PAPERLENGTH Or DM_PAPERWIDTH,
                       DM_DEFAULTSOURCE, DM_PRINTQUALITY, DM_COLOR, DM_DUPLEX);

Function PrinterCapabilitiesToString(aCap : TFslPrinterCapabilitySet) : String; Overload;


Type
  TFslPrinterTray = Class(TFslObject)
    Private
      FID : Integer;
      FName : String;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslPrinterTray;

      Procedure Assign(oObject : TFslObject); Override;

      Function AsText : String;

      Property Name : String Read FName Write FName;
      Property ID : Integer Read FID Write FID;
  End;

  TFslPrinterTrayList = Class(TFslObjectList)
    Private
      Function GetTray(iIndex: Integer): TFslPrinterTray;

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer;
      Function CompareByID(pA, pB : Pointer) : Integer;

    Public
      Procedure Add(Const sName: String; Const iID : Integer); Overload;

      Function ExistsByName(Const sName : String) : Boolean;
      Function IndexByName(Const sName : String) : Integer;
      Function GetByName(Const sName : String) : TFslPrinterTray;

      Function ExistsByID(iID : Integer) : Boolean;
      Function IndexByID(iID : Integer) : Integer;
      Function GetByID(iID : Integer) : TFslPrinterTray;

      Function AsCSV : String;

      Property Trays[iIndex : Integer] : TFslPrinterTray Read GetTray; Default;
  End;


Const
  ADVPRINTERBINTYPE_NAMES : Array [1..15] Of String = (
     'Only One', 'Lower', 'Middle', 'Manual', 'Envelope',
     'Manual Envelope', 'Auto', 'Tractor', 'Small Format', 'Large Format',
     'Large Capacity', 'Unknown', 'Unknown', 'Cassette', 'Form Source');

Type
  TFslPrinterOrientation = (poNotSpecified, poPortrait, poLandscape);


Const
  ADVPRINTERORIENTATION_NAMES : Array [TFslPrinterOrientation] Of String = ('Not Specified', 'Portrait', 'Landscape');


Type
  TFslJobSettingsClass = Class Of TFslJobSettings;

  TFslJobSettings = Class (TFslObject)
    Private
      FOpened : Boolean;

    Protected
      Function GetOrientation : TFslPrinterOrientation; Virtual;
      Procedure SetOrientation(Const aValue: TFslPrinterOrientation); Virtual;

      Function GetPageHeight : TFslGraphicMetre; Virtual;
      Procedure SetPageHeight(Const iValue: TFslGraphicMetre); Virtual;

      Function GetPageWidth : TFslGraphicMetre; Virtual;
      Procedure SetPageWidth(Const iValue: TFslGraphicMetre); Virtual;

      Function GetPageSize : TFslPaperSize; Virtual;
      Procedure SetPageSize(Const aValue: TFslPaperSize); Virtual;

      Function GetUseColour : Boolean; Virtual;
      Procedure SetUseColour(Const bValue: Boolean); Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      Function Link : TFslJobSettings;
      Function Clone : TFslJobSettings;

      Procedure Open; Virtual;
      Procedure Close; Virtual;

      Function ActualWidth : TFslGraphicMetre; Virtual;
      Function ActualHeight : TFslGraphicMetre; Virtual;

      Property Opened : Boolean Read FOpened;
      Property Orientation: TFslPrinterOrientation Read GetOrientation Write SetOrientation;
      Property PageHeight : TFslGraphicMetre Read GetPageHeight Write SetPageHeight;
      Property PageSize : TFslPaperSize Read GetPageSize Write SetPageSize; // setting this will reset PageHeight and PageWidth unless PageSize = psCustom
      Property PageWidth : TFslGraphicMetre Read GetPageWidth Write SetPageWidth;
      Property UseColour : Boolean Read GetUseColour Write SetUseColour;
  End;

Type
  TFslPrintRange = (aprAll, aprSelection, aprPages);

Const
  TADVPRINTRANGE_NAMES : Array [TFslPrintRange] Of String = ('All', 'Selection', 'Pages');

Type
  TFslPrinterCanvas = Class;
  TFslPrinterCanvasClass = Class Of TFslPrinterCanvas;

  TFslPrinterDelphiCanvas = Class(TCanvas)
    Private
      FOwner : TFslPrinterCanvas;
      FAdjustFont : Boolean;
      FOrigMapMode : Integer;
      FOrigWin : TSize;
      FOrigView : TSize;
      FOrigOrg : TPoint;
      FOrigAlign : Cardinal;
      FNeedReset : Boolean;

      Function GetPixelsPerInchY: Integer;
      Function GetPixelsPerInchX: Integer;
      Function GetHorizontalResolution: Integer;
      Function GetVerticalResolution: Integer;

      Function GetOwnerCanvas: TFslPrinterCanvas;
      Procedure SetOwnerCanvas(Const Value: TFslPrinterCanvas);

      Procedure UpdateFont;

    Protected
      Procedure CreateHandle; Override;
      Procedure Changing; Override;

    Public
      Procedure ResetMetrics;
      Procedure SetMapMode(iMode : Integer);
      Procedure SetWindowExtEx(i1, i2 : Integer);
      Procedure SetViewPortExtEx(i1,i2 : Integer);
      Procedure SetWindowOrgEx(i1,i2 : Integer);
      Procedure SelectClipRgn(iHnd : THandle);
      Function SetTextAlign(iMode : Cardinal) : Cardinal;

      Property OwnerCanvas : TFslPrinterCanvas Read GetOwnerCanvas Write SetOwnerCanvas;
      Property PixelsPerInchY : Integer Read GetPixelsPerInchY;
      Property PixelsPerInchX : Integer Read GetPixelsPerInchX;
      Property Width : Integer Read GetHorizontalResolution;
      Property Height : Integer Read GetVerticalResolution;
      Property AdjustFont : Boolean Read FAdjustFont Write FAdjustFont; // specially for HTMLViewer
  End;

  TFslPrinterCanvas = Class(TFslGraphicCapability)
    Private
      FCanvas : TFslPrinterDelphiCanvas;
      FPen : TFslPen;
      FBrush : TFslBrush;
      FFont : TFslFont;
      FHeight : TFslGraphicMetre;
      FWidth : TFslGraphicMetre;
      FPixelsPerGraphicMetreX : Real;
      FPixelsPerGraphicMetreY : Real;
      FOffsetPixelsX : Integer;
      FOffsetPixelsY : Integer;
      FOffsetX : TFslGraphicMetre;
      FOffsetY : TFslGraphicMetre;
      FClip : THandle;

      Procedure SetPen(oPen : TFslPen);
      Procedure SetBrush(oBrush : TFslBrush);
      Procedure SetFont(oFont : TFslFont);

      Function SelectPen : THandle;
      Function SelectBrush : THandle;
      Function SelectFont : THandle;

      Procedure RestoreHandle(aOld : THandle);

      Procedure LastWindowsError(Const sMethod: String);

      Procedure PrintBitmap(oBitmap: TBitmap; iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle: TFslGraphicAngle);
      Procedure PrintBitmapPixels(oBitmap: TBitmap; iX, iY, iWidth, iHeight: Integer);
      function GetHandle: TFslGraphicHandle;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslPrinterCanvas;

      Procedure Refresh;

      Procedure AttachToPrinter(aHandle : TFslGraphicHandle);
      Procedure AttachToPreview(aHandle : TFslGraphicHandle; iPixelWidth, iPixelHeight : Integer; iLogicalWidth, iLogicalHeight : TFslGraphicMetre);

      Procedure Clip(iLeft, iTop, iRight, iBottom : Integer);
      Procedure UnClip;

      // printing routines
      Procedure Line(iX, iY, iLen: TFslGraphicMetre; iAngle : TFslGraphicAngle = 0);
      Procedure LinePixels(iX, iY, iLen: Integer; iAngle : TFslGraphicAngle = 0);
      Procedure LinePixelsXY(iX1, iY1, iX2, iY2: Integer);

      Procedure BoxRectangle(iLeft, iTop, iRight, iBottom : TFslGraphicMetre; iAngle : TFslGraphicAngle = 0);
      Procedure Box(iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle : TFslGraphicAngle = 0);
      Procedure BoxPixels(iX, iY, iWidth, iHeight : TFslGraphicMetre);
      Procedure BoxRounded(iX, iY, iWidth, iHeight, iRadius: TFslGraphicMetre);
      Procedure BoxRoundedPixels(iX, iY, iWidth, iHeight, iRadius: TFslGraphicMetre);

      Procedure Polygon(Const aPoints: Array Of TPoint);
      Procedure Polyline(Const aPoints: Array Of TPoint);
      Procedure PolygonOffset(aPoints: Array Of TPoint);
      Procedure PolylineOffset(aPoints: Array Of TPoint);

      Procedure TextOut(iX, iY: TFslGraphicMetre; Const sText : String; iAngle : TFslGraphicAngle = 0);
      Procedure TextOutPixels(iX, iY: Integer; Const sText : String); Overload;
      Procedure TextOutPixels(aBackground : TColour; iX, iY : Integer; Const sText : String); Overload;
      Procedure TextRect(Const aRect : TRect; iX, iY : TFslGraphicMetre; Const sText : String; iAngle : TFslGraphicAngle = 0);

      Function TextWidth(Const sText : String; iAngle : TFslGraphicAngle = 0) : TFslGraphicMetre;
      Function TextWidthPixels(Const sText : String; iAngle : TFslGraphicAngle = 0) : Integer;
      Function TextHeight(Const sText : String; iAngle : TFslGraphicAngle = 0) : TFslGraphicMetre;
      Function TextHeightPixels(Const sText : String; iAngle : TFslGraphicAngle = 0) : Integer;
      Function TextExtentPixels(Const sText : String; iAngle: TFslGraphicAngle) : TSize;
      Function GetTextMetrics : TTextMetric;
      Procedure GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PFslPrinterIntegers);

      Procedure PrintImage(oImage: TFslGraphic; iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle : TFslGraphicAngle = 0);
      Procedure PrintImagePixels(oImage: TFslGraphic; iX, iY, iWidth, iHeight: Integer);
      Procedure PrintImageWithMask(oImage, oMask: TFslBitmapGraphic; aCopyMode : TCopyMode; iX, iY, iWidth, iHeight: TFslGraphicMetre);

      Function ToPixelX(iValue : TFslGraphicMetre) : Integer;
      Function ToPixelY(iValue : TFslGraphicMetre) : Integer;
      Function ToPixelWidth(iValue : TFslGraphicMetre) : Integer;
      Function ToPixelHeight(iValue : TFslGraphicMetre) : Integer;
      Function DoesColour: Boolean;

      Property VCLCanvas : TFslPrinterDelphiCanvas Read FCanvas; // a handle to a clunky delphi type canvas
      Property Pen : TFslPen Read FPen Write SetPen;
      Property Brush : TFslBrush Read FBrush Write SetBrush;
      Property Font : TFslFont Read FFont Write SetFont;
      Property Height : TFslGraphicMetre Read FHeight Write FHeight; // official, may not actually be correct
      Property Width : TFslGraphicMetre Read FWidth Write FWidth; // official, may not actually be correct
      Property PixelsPerGraphicMetreX : Real Read FPixelsPerGraphicMetreX Write FPixelsPerGraphicMetreX;
      Property PixelsPerGraphicMetreY : Real Read FPixelsPerGraphicMetreY Write FPixelsPerGraphicMetreY;
      Property OffsetX : TFslGraphicMetre Read FOffsetX Write FOffsetX;
      Property OffsetY : TFslGraphicMetre Read FOffsetY Write FOffsetY;
      Property OffsetPixelsX : TFslGraphicMetre Read FOffsetPixelsX Write FOffsetPixelsX;
      Property OffsetPixelsY : TFslGraphicMetre Read FOffsetPixelsY Write FOffsetPixelsY;
      Property DCHandle : THandle read GetHandle;      // for debugging only
  End;


Type
  TFslPrintQuality = (pqUnknown, pqHigh, pqMedium, pqLow, pqDraft);
  TFslPrintDuplex = (pdSimplex, pdHorizontal, pdVertical);

  TFslPrinterSettings = Class (TFslJobSettings)
    Private
      // Setup
      FDefinition : TFslPrinterDefinition;
      FDeviceSize : Cardinal;
      FDevice : PDeviceMode;
      FDeviceHandle : THandle;
      FPrinterHandle : THandle;
      FReadOnly : Boolean;
      FJobNumber : Int64;

      Function GetCopies: SmallInt;
      Procedure SetCopies(Const iValue: SmallInt);
      Function GetDuplex : TFslPrintDuplex;
      Procedure SetDuplex(Const aValue: TFslPrintDuplex);
      Function GetQuality : TFslPrintQuality;
      Procedure SetQuality(Const aValue: TFslPrintQuality);
      Function GetTray : Integer;
      Procedure SetTray(Const iValue: Integer);
      Function GetLimitLeft: TFslGraphicMetre;
      Function GetLimitTop: TFslGraphicMetre;
      Function GetLimitRight: TFslGraphicMetre;
      Function GetLimitBottom: TFslGraphicMetre;
      Function GetOpened : Boolean;

      Function GetDefinition: TFslPrinterDefinition;
      Procedure SetDefinition(oValue : TFslPrinterDefinition);

    Protected
      Function GetOrientation : TFslPrinterOrientation; Override;
      Procedure SetOrientation(Const aValue: TFslPrinterOrientation); Override;
      Function GetPageHeight : TFslGraphicMetre; Override;
      Procedure SetPageHeight(Const iValue: TFslGraphicMetre); Override;
      Function GetPageWidth : TFslGraphicMetre; Override;
      Procedure SetPageWidth(Const iValue: TFslGraphicMetre); Override;
      Function GetPageSize : TFslPaperSize; Override;
      Procedure SetPageSize(Const aValue: TFslPaperSize); Override;
      Function GetUseColour : Boolean; Override;
      Procedure SetUseColour(Const bValue: Boolean); Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslPrinterSettings;
      Function Clone : TFslPrinterSettings;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Open; Override;
      Procedure Close; Override;

      Function PropertiesDialog(aHandle : THandle) : Boolean; Overload; Virtual;

      // you must assign a definition and then Open before using anything else
      Property Definition : TFslPrinterDefinition Read GetDefinition Write SetDefinition;

      // readonly
      Property Device: PDeviceMode Read FDevice;
      Property Handle : THandle Read FPrinterHandle;
      Property LimitBottom : TFslGraphicMetre Read GetLimitBottom;
      Property LimitLeft : TFslGraphicMetre Read GetLimitLeft;
      Property LimitRight : TFslGraphicMetre Read GetLimitRight;
      Property LimitTop : TFslGraphicMetre Read GetLimitTop; // limits of printer's ability to print
      Property Opened : Boolean Read GetOpened;

      // writeable
      Property ReadOnly : Boolean Read FReadOnly Write FReadOnly; // settings are set to readonly if they are part of a job that is in progress
      Property Copies : SmallInt Read GetCopies Write SetCopies ;
      Property Duplex : TFslPrintDuplex Read GetDuplex Write SetDuplex;
      Property Quality : TFslPrintQuality Read GetQuality Write SetQuality;
      Property Tray : Integer Read GetTray Write SetTray;
      Property UseColour : Boolean Read GetUseColour Write SetUseColour;
      Property JobNumber : Int64 Read FJobNumber Write FJobNumber;
  End;


Const
  NAMES_PRINT_QAULITY : Array [TFslPrintQuality] of String = ('Unknown', 'High', 'Medium', 'Low', 'Draft');
  NAMES_PRINT_DUPLEX : Array [TFslPrintDuplex] of String = ('Simplex', 'Horizontal', 'Vertical');



Type
  TFslRenderJob = Class(TFslObject)
    Private
      FCanvas : TFslPrinterCanvas;
      FCapabilities : TFslPrinterCapabilitySet;

      // Setup
      FSettings : TFslJobSettings;
      FTitle : String;

      // operations
      FStarted : Boolean;
      FPageNumber: Word;
      FDestinationFileName: String;

      Function GetOpened : Boolean;

      Procedure SetDestinationFileName(Const Value: String);

      Function GetCanvas: TFslPrinterCanvas;

    Protected
      Function SettingsClass : TFslJobSettingsClass; Virtual;

      Procedure SetTitle(Const sValue : String); Virtual;

      // descendent must implement these as appropriate
      Procedure InternalStart; Virtual;
      Procedure InternalNewPage; Virtual;
      Procedure InternalAbort; Virtual;
      Procedure InternalFinish; Virtual;

      Procedure RecreateCanvas; Virtual;

      Function CanvasClass : TFslPrinterCanvasClass; Overload; Virtual;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslRenderJob;

      Procedure Open; Virtual;
      Procedure OpenSettings(oSettings : TFslJobSettings); Virtual;
      Procedure Close; Virtual;
      Procedure Clear; Virtual;

      Procedure Start; Virtual; // once setup
      Procedure NewPage; Virtual;
      Procedure Finish; Virtual;
      Procedure Abort; Virtual;

      Property Canvas : TFslPrinterCanvas Read GetCanvas; // can only access after Start is called
      Property Opened : Boolean Read GetOpened;
      Property Settings : TFslJobSettings Read FSettings;
      Property Started : Boolean Read FStarted;
      Property PageNumber : Word Read FPageNumber; // Current page
      Property Title : String Read FTitle Write SetTitle;
      Property DestinationFileName : String Read FDestinationFileName Write SetDestinationFileName;
      Property Capabilities : TFslPrinterCapabilitySet read FCapabilities write FCapabilities;
  End;

  TFslRenderJobs = Class(TFslObjectList);


Type
  TFslRenderDeviceJob = Class(TFslRenderJob)
    Private
      FDC : THandle;

    Protected
      Procedure InternalStart; Override;
      Procedure InternalNewPage; Override;
      Procedure InternalAbort; Override;
      Procedure InternalFinish; Override;

      Procedure InitialiseDC; Virtual;
      Procedure InitialiseCanvas; Virtual;

    Public
      Property DC : THandle Read FDC Write FDC;
  End;

  TFslRenderDeviceJobs = Class(TFslRenderJobs);


Type
  TFslMetafile = Class(TFslVCLGraphic)
    Private
      Function GetHandle: TMetafile;
      Procedure SetHandle(Const Value: TMetafile);

    Protected
      Function HandleClass : TGraphicClass; Overload; Override;
      Function HandleNew : TGraphic; Overload; Override;

    Public
      Property Handle : TMetafile Read GetHandle Write SetHandle;
  End;

  TFslMetafileList = Class(TFslVCLGraphicList)
    Private
      Function GetGraphic(iIndex: Integer): TFslMetafile;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Graphics[iIndex : Integer] : TFslMetafile Read GetGraphic; Default;
  End;

Type
  TFslPrinterPreviewPage = Class(TFslMetaFile)
  End;

  TFslPrinterPreviewPageList = Class(TFslMetafileList)
    Private
      Function GetPage(iIndex: Integer): TFslPrinterPreviewPage;

    Protected
      Function ItemClass : TFslObjectClass; Override;

    Public
      Property Page[iIndex : Integer] : TFslPrinterPreviewPage Read GetPage; Default;
  End;

  TFslPrinterJob = Class(TFslRenderDeviceJob)
    Private
      Function GetSettings: TFslPrinterSettings;

    Protected
      Procedure InitialiseDC; Override;
      Procedure InitialiseCanvas; Override;

      Function SettingsClass : TFslJobSettingsClass; Override;

      Procedure SetTitle(Const sValue : String); Override;

    Public
      Procedure OpenDefinition(oDefinition : TFslPrinterDefinition);

      Property Settings : TFslPrinterSettings Read GetSettings;
  End;


  TFslPrinterPreviewHandle = THandle;

  TFslPrinterPreviewJob = Class(TFslRenderJob)
    Private
      FHandle : TFslPrinterPreviewHandle;
      FMeta : TMetafileCanvas;
      FPages : TFslPrinterPreviewPageList;
      FActivePage : TFslPrinterPreviewPage;
      FOffsetX : TFslGraphicMetre;
      FOffsetY : TFslGraphicMetre;

      Function GetActivePage: TFslPrinterPreviewPage;
      Procedure SetActivePage(Const Value: TFslPrinterPreviewPage);
      Function GetSettings: TFslPrinterSettings;

    Protected
      Function SettingsClass : TFslJobSettingsClass; Override;

      Procedure SetTitle(Const sValue : String); Override;

      Procedure InternalStart; Override;
      Procedure InternalNewPage; Override;
      Procedure InternalAbort; Override;
      Procedure InternalFinish; Override;

      Property ActivePage : TFslPrinterPreviewPage Read GetActivePage Write SetActivePage;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslPrinterPreviewJob;

      Property Pages : TFslPrinterPreviewPageList Read FPages;
      Property OffsetX : TFslGraphicMetre Read FOffsetX Write FOffsetX;
      Property OffsetY : TFslGraphicMetre Read FOffsetY Write FOffsetY;
      Property Settings : TFslPrinterSettings Read GetSettings;
  End;



Type
  TFslPrinterAttribute = (paDefault, paDirect, paSpoolFirst, paBiDirectional, paQueryDevice, paFax, paKeepPrintedJobs,
                          paQueued, paShared, paOffline, paPublished, paNetwork, paHidden, paLocal, paRawOnly);
  TFslPrinterAttributeSet = Set Of TFslPrinterAttribute;

  TFslPrinterStatus = (psBusy, psDoorOpen, psError, psInitializing, psWorking, psManualFeed, psNoToner, psNotAvailable, psOffline,
                        psOutOfMemory, psOutputBinFull, psPagePunt, psPaperJam, psPaperOut, psPaperProblem, psPaused, psPendingDeletion,
                        psPowerSave,psPrinting, sProcessing, psServerUnknown, psTonerLow, psUserIntervention, psWaiting, psWarmingUp);
  TFslPrinterStatusSet = Set Of TFslPrinterStatus;

  TFslPrinter = Class(TFslObject)
    Private
      FOpened : Boolean;
      FDefinition : TFslPrinterDefinition;
      FFontList : TFslPrinterFontList;
      FPaperSizeList : TFslPrinterPaperSizeList;
      FCapabilitySet : TFslPrinterCapabilitySet;
      FTrayList : TFslPrinterTrayList;
      FSettings : TFslPrinterSettings;

      Function GetDefinition: TFslPrinterDefinition;
      Procedure SetDefinition(Const oValue: TFslPrinterDefinition);

      Function GetSettings: TFslPrinterSettings;
      Procedure SetSettings(Const Value: TFslPrinterSettings);

      Function GetCapabilitySet : TFslPrinterCapabilitySet;
      Function GetFontList : TFslPrinterFontList;
      Function GetPaperSizeList : TFslPrinterPaperSizeList;
      Function GetTrayList : TFslPrinterTrayList;

    Protected
      Procedure ProduceInfo2(Out aInfoPointer : PPrinterInfo2; Out iInfoLength : Integer);
      Procedure ConsumeInfo2(Var aInfoPointer : PPrinterInfo2; Const iInfoLength : Integer);
      Procedure ProduceDriverInfo2(Out aInfoPointer : PDriverInfo2; Out iInfoLength : Cardinal);
      Procedure ConsumeDriverInfo2(Var aInfoPointer : PDriverInfo2; Const iInfoLength : Cardinal);

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Function Link : TFslPrinter;
      Function Clone : TFslPrinter;

      Procedure Assign(oObject : TFslObject); Override;

      Procedure Open;
      Procedure Close;
      Property Opened : Boolean read FOpened;

      Function PropertiesDialog(aHandle : THandle) : Boolean;
      Function Status : TFslPrinterStatusSet;
      Function StatusDescription : String;
      Function Server : String;  // '' for local printer
      Function Description : String;  // Comment and location
      Function AttributeSet : TFslPrinterAttributeSet;
      Function QueueCount : Integer;

      Function ProduceJob : TFslPrinterJob;
      Function ProducePreviewJob : TFslPrinterPreviewJob;

      Procedure ConsumeJob(oJob: TFslPrinterJob);
      Procedure ConsumePreviewJob(oJob: TFslPrinterPreviewJob);

      Function HasDefinition : Boolean;
    
      // readonly
      Property CapabilitySet : TFslPrinterCapabilitySet Read GetCapabilitySet;
      Property FontList : TFslPrinterFontList Read GetFontList; // font's supported by printer - includes all truetypes because of GDI
      Property PaperSizeList : TFslPrinterPaperSizeList Read GetPaperSizeList;
      Property TrayList : TFslPrinterTrayList Read GetTrayList;

      // writeable
      Property Definition : TFslPrinterDefinition Read GetDefinition Write SetDefinition;
      Property Settings : TFslPrinterSettings Read GetSettings Write SetSettings;
  End;

  TFslPrinterList = Class(TFslObjectList)
    Private
      Function GetPrinter(iIndex: Integer): TFslPrinter;
      Procedure SetPrinter(iIndex: Integer; Const Value: TFslPrinter);

    Protected
      Function ItemClass : TFslObjectClass; Override;

      Function CompareByName(pA, pB : Pointer) : Integer; 
      Function CompareByPort(pA, pB : Pointer) : Integer;
      Function CompareByDefault(pA, pB : Pointer) : Integer;

      Function Get(Const aValue : Integer) : TFslPrinter; Reintroduce; 

    Public
      Function Link : TFslPrinterList; 

      Function IndexByDefault : Integer;
      Function GetByDefault : TFslPrinter;

      Function IndexByName(Const aValue : String) : Integer;
      Function IndexByPort(Const aValue : String) : Integer;

      Function GetByName(Const aValue : String) : TFslPrinter;

      Function ExistsByName(Const aValue : String) : Boolean;
      Function ExistsByPort(Const aValue : String) : Boolean;

      Procedure SortedByName;
      Function IsSortedByName : Boolean;

      Procedure SortedByDefault;
      Function IsSortedByDefault : Boolean;

      Property Printer[iIndex : Integer] : TFslPrinter Read GetPrinter Write SetPrinter; Default;
  End;


Const

  ADVPRINTERATTRIBUTE_NAMES : Array [TFslPrinterAttribute] Of String = ('Default', 'Direct', 'SpoolFirst', 'BiDirectional', 'QueryDevice', 'Fax', 'KeepPrintedJobs',
                          'Queued', 'Shared', 'Offline', 'Published', 'Network', 'Hidden', 'Local', 'RawOnly');

  ADVPRINTERATTRIBUTE_VALUES : Array [TFslPrinterAttribute] Of DWord = (
          PRINTER_ATTRIBUTE_DEFAULT, PRINTER_ATTRIBUTE_DIRECT, PRINTER_ATTRIBUTE_DO_COMPLETE_FIRST, PRINTER_ATTRIBUTE_ENABLE_BIDI, PRINTER_ATTRIBUTE_ENABLE_DEVQ,
          {PRINTER_ATTRIBUTE_FAX} 0, PRINTER_ATTRIBUTE_KEEPPRINTEDJOBS, PRINTER_ATTRIBUTE_QUEUED, PRINTER_ATTRIBUTE_SHARED, PRINTER_ATTRIBUTE_WORK_OFFLINE,
          {PRINTER_ATTRIBUTE_PUBLISHED} 0, PRINTER_ATTRIBUTE_NETWORK, PRINTER_ATTRIBUTE_HIDDEN, PRINTER_ATTRIBUTE_LOCAL, PRINTER_ATTRIBUTE_RAW_ONLY);


  ADVPRINTERSTATUS_NAMES : Array [TFslPrinterStatus] Of String = (
                        'Busy', 'DoorOpen', 'Error', 'Initializing', 'Working', 'Manual Feed', 'No Toner', 'Not Available', 'Offline',
                        'Out Of Memory', 'Output Bin Full', 'Page Punt', 'Paper Jam', 'Paper Out', 'Paper Problem', 'Paused', 'Pending Deletion',
                        'Power Save', 'Printing', 'Processing', 'Server Unknown', 'Toner Low', 'User Intervention Required', 'Waiting', 'Warming Up');
  ADVPRINTERSTATUS_VALUES : Array [TFslPrinterStatus] Of DWord = (
          PRINTER_STATUS_BUSY, PRINTER_STATUS_DOOR_OPEN, PRINTER_STATUS_ERROR, PRINTER_STATUS_INITIALIZING, PRINTER_STATUS_IO_ACTIVE,
          PRINTER_STATUS_MANUAL_FEED, PRINTER_STATUS_NO_TONER, PRINTER_STATUS_NOT_AVAILABLE, PRINTER_STATUS_OFFLINE, PRINTER_STATUS_OUT_OF_MEMORY,
          PRINTER_STATUS_OUTPUT_BIN_FULL, PRINTER_STATUS_PAGE_PUNT, PRINTER_STATUS_PAPER_JAM, PRINTER_STATUS_PAPER_OUT, PRINTER_STATUS_PAPER_PROBLEM,
          PRINTER_STATUS_PAUSED, PRINTER_STATUS_PENDING_DELETION, PRINTER_STATUS_POWER_SAVE, PRINTER_STATUS_PRINTING, PRINTER_STATUS_PROCESSING,
          PRINTER_STATUS_SERVER_UNKNOWN, PRINTER_STATUS_TONER_LOW, PRINTER_STATUS_USER_INTERVENTION, PRINTER_STATUS_WAITING, PRINTER_STATUS_WARMING_UP);


Function PrinterAttributesToString(aCap : TFslPrinterAttributeSet) : String; Overload;
Function PrinterStatusesToString(aCap : TFslPrinterStatusSet) : String; Overload;


Type
  TFslPrinterManager = Class(TFslObject)
    Private
      FDefinitionList : TFslPrinterDefinitionList;
      FDefaultDefinition : TFslPrinterDefinition;
      FActive : Boolean;

      Function ExtractNextString(Var Str: PChar): PChar;

      Function GetDefaultDefinition: TFslPrinterDefinition;

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Open;
      Procedure Close;

      Procedure CompilePrinterList(oPrinterList : TFslPrinterList);

      Function HasDefaultDefinition : Boolean;

      Property DefinitionList : TFslPrinterDefinitionList Read FDefinitionList;
      Property DefaultDefinition : TFslPrinterDefinition Read GetDefaultDefinition;
  End;



Implementation



Function PrinterAttributesToString(aCap : TFslPrinterAttributeSet) : String;
Var
  aLoop : TFslPrinterAttribute;
Begin
  Result := '';

  For aLoop := Low(TFslPrinterAttribute) To High(TFslPrinterAttribute) Do
  Begin
    If aLoop In aCap Then
      StringAppend(Result, ADVPRINTERATTRIBUTE_NAMES[aLoop], ',');
  End;

  Result := '[' + Result + ']';
End;


Function PrinterStatusesToString(aCap : TFslPrinterStatusSet) : String;
Var
  aLoop : TFslPrinterStatus;
Begin
  Result := '';

  For aLoop := Low(TFslPrinterStatus) To High(TFslPrinterStatus) Do
  Begin
    If aLoop In aCap Then
      StringAppend(Result, ADVPRINTERSTATUS_NAMES[aLoop], ',');
  End;

  Result := '[' + Result + ']';
End;


Constructor TFslPrinter.Create;
Begin
  Inherited;

  FFontList := TFslPrinterFontList.Create;
  FPaperSizeList := TFslPrinterPaperSizeList.Create;
  FTrayList := TFslPrinterTrayList.Create;
  FSettings := TFslPrinterSettings.Create;
  FDefinition := Nil;
  FCapabilitySet := [];
  FOpened := False;
End;


Destructor TFslPrinter.Destroy;
Begin
  FFontList.Free;
  FPaperSizeList.Free;
  FDefinition.Free;
  FSettings.Free;
  FTrayList.Free;

  Inherited;
End;


Function TFslPrinter.Link: TFslPrinter;
Begin
  Result := TFslPrinter(Inherited Link);
End;


Function TFslPrinter.Clone : TFslPrinter;
Begin
  Result := TFslPrinter(Inherited Clone);
End;


Procedure TFslPrinter.Assign(oObject: TFslObject);
Begin
  Inherited;

  Definition := TFslPrinter(oObject).Definition.Clone;
  
  // These are calculated from opening the printer (using the definition provided).
//FFontList.Assign(TFslPrinter(oObject).FFontList);
//FPaperSizeList.Assign(TFslPrinter(oObject).FPaperSizeList);
//FTrayList.Assign(TFslPrinter(oObject).FTrayList);
//FSettings.Assign(TFslPrinter(oObject).FSettings);
//FCapabilitySet := TFslPrinter(oObject).FCapabilitySet;
End;


Procedure TFslPrinter.Open;
Type
  TFslPrinterBinName = Array [0..23] Of Char;
  TFslPrinterBinNameArray = Array [1..1000] Of TFslPrinterBinName;
  PFslPrinterBinNameArray = ^TFslPrinterBinNameArray;
  TFslPrinterBinID = Word;
  TFslPrinterBinIDArray = Array [1..1000] Of TFslPrinterBinID;
  PFslPrinterBinIDArray = ^TFslPrinterBinIDArray;
  TFslPrinterPaperName = Array [0..63] Of Char;
  TFslPrinterPaperNameArray = Array [1..1000] Of TFslPrinterPaperName;
  PFslPrinterPaperNameArray = ^TFslPrinterPaperNameArray;
Var
  aDC : HDC;
  pPaperFormatArray : PFslPrinterPaperNameArray;
  iPaperSizeCount : Integer;
  iPaperSizeIndex : Integer;
  iTrayIDCount : Integer;
  iTrayNameCount : Integer;
  iTrayIndex : Integer;
  pBinNameArray : PFslPrinterBinNameArray;
  pBinIDArray : PFslPrinterBinIDArray;
  aCapabilityLoop : TFslPrinterCapability;
  pDefinitionPort : PChar;
  pDefinitionName : PChar;
Begin
  If Not FOpened Then
  Begin
    If Not HasDefinition Then
      RaiseError('Open', 'Cannot connect to the the printer because no definition has been provided.');

    pDefinitionName := PChar(Definition.Name);
    pDefinitionPort := PChar(Definition.Port);

    aDC := TFslSynchronisedPrinterAPI.CreateIC(PChar(Definition.Driver), pDefinitionName, pDefinitionPort, Nil);

    If aDC = 0 Then
      RaiseError('Open', StringFormat('Unable to connect to the printer %s: [%s].', [Definition.Name, ErrorAsString]));

    FFontList.Enumerate(aDC);

    TFslSynchronisedPrinterAPI.DeleteIC(aDC);

    // Paper Sizes.
    iPaperSizeCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_PAPERNAMES, Nil, Nil);

    If iPaperSizeCount > 0 Then
    Begin
      MemoryCreate(pPaperFormatArray, iPaperSizeCount * SizeOf(TFslPrinterPaperName));
      Try
        iPaperSizeCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_PAPERNAMES, pChar(pPaperFormatArray), Nil);

        For iPaperSizeIndex := 1 To iPaperSizeCount Do
          FPaperSizeList.AddFromText(pPaperFormatArray^[iPaperSizeIndex]);
      Finally
        MemoryDestroy(pPaperFormatArray, iPaperSizeCount * SizeOf(TFslPrinterPaperName));
      End;
    End;

    // Trays.
    iTrayNameCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_BINNAMES, Nil, Nil);

    If iTrayNameCount <> 0 Then
    Begin
      iTrayIDCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_BINS, Nil, Nil);

      MemoryCreate(pBinNameArray, iTrayNameCount * SizeOf(TFslPrinterBinName));
      Try
        MemoryCreate(pBinIDArray, iTrayIDCount * SizeOf(TFslPrinterBinID));
        Try
          iTrayNameCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_BINNAMES, PChar(pBinNameArray), Nil);
          iTrayIDCount := TFslSynchronisedPrinterAPI.DeviceCapabilities(pDefinitionName, pDefinitionPort, DC_BINS, PChar(pBinIDArray), Nil);

          For iTrayIndex := 1 To IntegerMin(iTrayNameCount, iTrayIDCount) Do
            FTrayList.Add(pBinNameArray^[iTrayIndex], pBinIDArray^[iTrayIndex]);
        Finally
          MemoryDestroy(pBinIDArray, iTrayIDCount * SizeOf(TFslPrinterBinID));
        End;
      Finally
        MemoryDestroy(pBinNameArray, iTrayNameCount * SizeOf(TFslPrinterBinName));
      End;
    End;

    FSettings.Definition := FDefinition.Link;
    FSettings.Open;

    // Capabilities.
    FCapabilitySet := [];
    For aCapabilityLoop := Low(TFslPrinterCapability) To High(TFslPrinterCapability) Do
    Begin
      If (ADVPRINTERCAPABILITY_VALUES[aCapabilityLoop] And FSettings.Device^.dmFields) > 0 Then
        Include(FCapabilitySet, aCapabilityLoop);
    End;

    FOpened := True;
  End;
End;


Procedure TFslPrinter.Close;
Begin
  FOpened := False;

  FSettings.Close;

  FFontList.Clear;
  FTrayList.Clear;
  FPaperSizeList.Clear;

  FCapabilitySet := [];
End;


Procedure TFslPrinter.ProduceInfo2(Out aInfoPointer : PPrinterInfo2; Out iInfoLength : Integer);
Begin
  iInfoLength := 0;
  TFslSynchronisedPrinterAPI.GetPrinter(Settings.Handle, 2, Nil, 0, @iInfoLength);

  MemoryCreate(aInfoPointer, iInfoLength);
  Try
    If Not TFslSynchronisedPrinterAPI.GetPrinter(Settings.handle, 2, aInfoPointer, iInfoLength, @iInfoLength) Then
      RaiseError('ProduceInfo2', ErrorAsString);
  Except
    MemoryDestroy(aInfoPointer, iInfoLength);

    Raise;
  End;
End;


Procedure TFslPrinter.ConsumeInfo2(Var aInfoPointer : PPrinterInfo2; Const iInfoLength : Integer);
Begin
  MemoryDestroy(aInfoPointer, iInfoLength);
End;


Procedure TFslPrinter.ProduceDriverInfo2(Out aInfoPointer : PDriverInfo2; Out iInfoLength : Cardinal);
Begin
  iInfoLength := 0;
  TFslSynchronisedPrinterAPI.GetPrinterDriver(Settings.Handle, Nil, 2, Nil, 0, iInfoLength);

  MemoryCreate(aInfoPointer, iInfoLength);
  Try
    If Not TFslSynchronisedPrinterAPI.GetPrinterDriver(Settings.handle, Nil, 2, aInfoPointer, iInfoLength, iInfoLength) Then
      RaiseError('ProduceDriverInfo2', ErrorAsString);
  Except
    MemoryDestroy(aInfoPointer, iInfoLength);

    Raise;
  End;
End;


Procedure TFslPrinter.ConsumeDriverInfo2(Var aInfoPointer : PDriverInfo2; Const iInfoLength : Cardinal);
Begin
  MemoryDestroy(aInfoPointer, iInfoLength);
End;



Function TFslPrinter.ProduceJob: TFslPrinterJob;
Begin
  Result := TFslPrinterJob.Create;
  Try
    Result.OpenSettings(Settings);
    Result.Capabilities := CapabilitySet;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TFslPrinter.ConsumeJob(oJob : TFslPrinterJob);
Begin
  Assert(Invariants('ConsumeJob', oJob, TFslPrinterJob, 'oJob'));

  Try
    oJob.Close;
  Finally
    oJob.Free;
  End;
End;


Function TFslPrinter.ProducePreviewJob : TFslPrinterPreviewJob;
Begin
  Result := TFslPrinterPreviewJob.Create;
  Try
    Result.OpenSettings(Settings);
    Result.Capabilities := CapabilitySet;

    Result.Link;
  Finally
    Result.Free;
  End;
End;


Procedure TFslPrinter.ConsumePreviewJob(oJob : TFslPrinterPreviewJob);
Begin
  Assert(Invariants('ConsumeJob', oJob, TFslPrinterPreviewJob, 'oJob'));

  Try
//  oJob.Close; // Don't close because preview jobs are interactive.
  Finally
    oJob.Free;
  End;
End;


Function TFslPrinter.PropertiesDialog(aHandle : THandle) : Boolean;
Begin
  Result := Settings.PropertiesDialog(aHandle);
End;


Function TFslPrinter.GetDefinition: TFslPrinterDefinition;
Begin
  Assert(Invariants('GetDefinition', FDefinition, TFslPrinterDefinition, 'FDefinition'));

  Result := FDefinition;
End;


Procedure TFslPrinter.SetDefinition(Const oValue: TFslPrinterDefinition);
Begin
  Assert(Not Assigned(oValue) Or Invariants('SetDefinition', oValue, TFslPrinterDefinition, 'oValue'));
  Assert(CheckCondition(Not FOpened, 'SetDefinition', 'Cannot change the printer definition while it is open.'));

  FDefinition.Free;
  FDefinition := oValue;
End;


Function TFslPrinter.HasDefinition: Boolean;
Begin
  Result := Assigned(FDefinition);
End;


Function TFslPrinter.GetCapabilitySet : TFslPrinterCapabilitySet;
Begin
  Assert(CheckCondition(FOpened, 'GetCapabilitySet', 'Cannot access printer capabilities until it is open.'));

  Result := FCapabilitySet;
End;


Function TFslPrinter.GetFontList : TFslPrinterFontList;
Begin
  Assert(CheckCondition(FOpened, 'GetFontList', 'Cannot access printer fonts until it is open.'));

  Result := FFontList;
End;


Function TFslPrinter.GetPaperSizeList : TFslPrinterPaperSizeList;
Begin
  Assert(CheckCondition(FOpened, 'GetPaperSizeList', 'Cannot access printer paper sizes until it is open.'));

  Result := FPaperSizeList;
End;


Function TFslPrinter.GetSettings: TFslPrinterSettings;
Begin
  Assert(CheckCondition(FOpened, 'GetSettings', 'Cannot access printer settings until it is open.'));
  Assert(Invariants('GetSettings', FSettings, TFslPrinterSettings, 'FSettings'));

  Result := FSettings;
End;


Procedure TFslPrinter.SetSettings(Const Value: TFslPrinterSettings);
Begin
  Assert(Invariants('SetSettings', Value, TFslPrinterSettings, 'Value'));

  If (Value.Definition.Name <> Definition.Name) Then
    RaiseError('SetSettings', StringFormat('Attempted to attach settings for %s to printer %s.', [Value.Definition.Name, Definition.Name]));

  FSettings.Free;
  FSettings := Value;
End;


Function TFslPrinter.GetTrayList : TFslPrinterTrayList;
Begin
  Assert(CheckCondition(FOpened, 'GetTrayList', 'Cannot access printer trays until it is open.'));

  Result := FTrayList;
End;


Function TFslPrinter.QueueCount : Integer;
Var
  aInfoPointer : PPrinterInfo2;
  iInfoLength : Integer;
Begin
  Assert(CheckCondition(FOpened, 'QueueCount', 'Cannot get the printer queue count until the printer is open.'));

  ProduceInfo2(aInfoPointer, iInfoLength);
  Try
    Result := aInfoPointer^.cJobs;
  Finally
    ConsumeInfo2(aInfoPointer, iInfoLength)
  End;
End;


Function TFslPrinter.Description : String;
Var
  aInfoPointer : PPrinterInfo2;
  iInfoLength : Integer;
  sCnt : String;
Begin
  Assert(CheckCondition(FOpened, 'Description', 'Cannot get the printer description until the printer is open.'));

  ProduceInfo2(aInfoPointer, iInfoLength);
  Try
    Result := aInfoPointer^.pComment;

    sCnt := aInfoPointer^.pLocation;

    If sCnt <> '' Then
      StringAppend(Result, 'Location: ' + sCnt, '; ');

    sCnt := aInfoPointer^.pServerName;

    If sCnt <> '' Then
      StringAppend(Result, 'Server: ' + sCnt, '; ');
  Finally
    ConsumeInfo2(aInfoPointer, iInfoLength);
  End;
End;


Function TFslPrinter.Server : String;
Var
  aInfoPointer : PPrinterInfo2;
  iInfoLength : Integer;
Begin
  Assert(CheckCondition(FOpened, 'Server', 'Cannot get the printer server until the printer is open.'));

  ProduceInfo2(aInfoPointer, iInfoLength);
  Try
    Result := aInfoPointer^.pServerName;
  Finally
    ConsumeInfo2(aInfoPointer, iInfoLength)
  End;
End;


Function TFslPrinter.AttributeSet : TFslPrinterAttributeSet;
Var
  aInfoPointer : PPrinterInfo2;
  iInfoLength : Integer;
  aLoop : TFslPrinterAttribute;
Begin
  Assert(CheckCondition(FOpened, 'Server', 'Cannot get the printer attributes until the printer is open.'));

  ProduceInfo2(aInfoPointer, iInfoLength);
  Try
    Result := [];
    For aLoop := Low(TFslPrinterAttribute) To High(TFslPrinterAttribute) Do
    Begin
      If ADVPRINTERATTRIBUTE_VALUES[aLoop] And aInfoPointer^.Attributes > 0 Then
        Result := Result + [aLoop];
    End;
  Finally
    ConsumeInfo2(aInfoPointer, iInfoLength)
  End;
End;


Function TFslPrinter.Status : TFslPrinterStatusSet;
Var
  aInfoPointer : PPrinterInfo2;
  iInfoLength : Integer;
  aLoop : TFslPrinterStatus;
Begin
  Assert(CheckCondition(FOpened, 'Server', 'Cannot get the printer status until the printer is open.'));

  ProduceInfo2(aInfoPointer, iInfoLength);
  Try
    Result := [];
    For aLoop := Low(TFslPrinterStatus) To High(TFslPrinterStatus) Do
    Begin
      If ADVPRINTERSTATUS_VALUES[aloop] And aInfoPointer^.Status > 0 Then
        Result := Result + [aLoop];
    End;
  Finally
    ConsumeInfo2(aInfoPointer, iInfoLength)
  End;
End;


Function TFslPrinter.StatusDescription : String;
Var
  aStatus : TFslPrinterStatusSet;
Begin
  aStatus := Status;

  If aStatus = [] Then
    Result := 'Ready'
  Else
    Result := PrinterStatusesToString(Status);
End;


function TFslPrinter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
  inc(result, FFontList.sizeInBytes);
  inc(result, FPaperSizeList.sizeInBytes);
  inc(result, FTrayList.sizeInBytes);
  inc(result, FSettings.sizeInBytes);
end;

Function TFslPrinterList.GetPrinter(iIndex: Integer): TFslPrinter;
Begin
  Result := TFslPrinter(ObjectByIndex[iIndex]);
End;


Function TFslPrinterList.ItemClass: TFslObjectClass;
Begin
  Result := TFslPrinter;
End;


Function TFslPrinterList.Link: TFslPrinterList;
Begin
  Result := TFslPrinterList(Inherited Link);
End;


Procedure TFslPrinterList.SetPrinter(iIndex: Integer; Const Value: TFslPrinter);
Begin
  ObjectByIndex[iIndex] := Value;
End;


Function TFslPrinterList.CompareByName(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslPrinter(pA).Definition.Name, TFslPrinter(pB).Definition.Name);
End;


Function TFslPrinterList.CompareByPort(pA, pB : Pointer) : Integer;
Begin
  Result := StringCompare(TFslPrinter(pA).Definition.Port, TFslPrinter(pB).Definition.Port);
End;


Function TFslPrinterList.CompareByDefault(pA, pB : Pointer) : Integer;
Begin
  Result := BooleanCompare(TFslPrinter(pA).Definition.IsDefault, TFslPrinter(pB).Definition.IsDefault);
End;


Function TFslPrinterList.IndexByDefault: Integer;
Var
  oPrinter : TFslPrinter;
  oDefinition : TFslPrinterDefinition;
Begin
  oDefinition := TFslPrinterDefinition.Create;
  Try
    oPrinter := TFslPrinter(ItemNew);
    Try
      oDefinition.IsDefault := True;
      oPrinter.Definition := oDefinition.Link;

      If Not Find(oPrinter, Result, CompareByDefault) Then
        Result := -1;
    Finally
      oPrinter.Free;
    End;
  Finally
    oDefinition.Free;
  End;
End;


Function TFslPrinterList.IndexByName(Const aValue : String) : Integer;
Var
  oPrinter : TFslPrinter;
  oDefinition : TFslPrinterDefinition;
Begin
  oDefinition := TFslPrinterDefinition.Create;
  Try
    oPrinter := TFslPrinter(ItemNew);
    Try
      oDefinition.Name := aValue;
      oPrinter.Definition := oDefinition.Link;

      If Not Find(oPrinter, Result, CompareByName) Then
        Result := -1;
    Finally
      oPrinter.Free;
    End;
  Finally
    oDefinition.Free;
  End;
End;


Function TFslPrinterList.IndexByPort(Const aValue : String) : Integer;
Var
  oPrinter : TFslPrinter;
  oDefinition : TFslPrinterDefinition;
Begin
  oDefinition := TFslPrinterDefinition.Create;
  Try
    oPrinter := TFslPrinter(ItemNew);
    Try
      oDefinition.Port := aValue;
      oPrinter.Definition := oDefinition.Link;

      If Not Find(oPrinter, Result, CompareByPort) Then
        Result := -1;
    Finally
      oPrinter.Free;
    End;
  Finally
    oDefinition.Free;
  End;
End;


Function TFslPrinterList.Get(Const aValue : Integer) : TFslPrinter;
Begin
  Result := TFslPrinter(Inherited Get(aValue));
End;


Function TFslPrinterList.GetByName(Const aValue : String) : TFslPrinter;
Begin
  Result := Get(IndexByName(aValue));
End;


Function TFslPrinterList.ExistsByName(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByName(aValue));
End;


Function TFslPrinterList.ExistsByPort(Const aValue : String) : Boolean;
Begin
  Result := ExistsByIndex(IndexByPort(aValue));
End;


Procedure TFslPrinterList.SortedByName;
Begin
  SortedBy(CompareByName);
End;


Function TFslPrinterList.IsSortedByName : Boolean;
Begin
  Result := IsSortedBy(CompareByName);
End;


Procedure TFslPrinterList.SortedByDefault;
Begin
  SortedBy(CompareByDefault);
End;


Function TFslPrinterList.IsSortedByDefault : Boolean;
Begin
  Result := IsSortedBy(CompareByDefault);
End;


Function TFslPrinterList.GetByDefault: TFslPrinter;
Begin
  Result := TFslPrinter(Get(IndexByDefault));
End;

(*
How to get a flag directly from the driver about whether
it supports color or not.

2 problems:
- no reason to think this would be more accurate than
  using the CapabilitySet
- doesn't work on 64 bit systems

Type
  TDrvDeviceCapabilities = function (hPrinter : THANDLE; pDeviceName : PWideChar; iDevCap : WORD; pvOutput : Pointer; pDevMode : PDeviceMode) : DWord; stdcall;

function TFslPrinter.DeviceIsColor: Boolean;
Var
  aInfoPointer : PDriverInfo2;
  iInfoLength : Cardinal;
  hMod : HMODULE;
  DrvDeviceCapabilities : TDrvDeviceCapabilities;
  n : WideString;
  r : DWord;
Begin
  Assert(CheckCondition(FOpened, 'QueueCount', 'Cannot get the printer device color flag until the printer is open.'));

  ProduceDriverInfo2(aInfoPointer, iInfoLength);
  Try
    hMod := LoadLibrary(aInfoPointer^.pConfigFile);
    if (hMod <> 0) Then
    Begin
      Try
        @DrvDeviceCapabilities := GetProcAddress(hMod, 'DrvDeviceCapabilities');
        n := Definition.Name;
        r := DrvDeviceCapabilities(Settings.Handle, pWideChar(n), DC_COLORDEVICE, nil, Settings.Device);
        result := r = 1;
      Finally
        FreeLibrary(hMod);
      End;
    End;
  Finally
    ConsumeDriverInfo2(aInfoPointer, iInfoLength)
  End;
end;
*)


Procedure TFslPrinterDefinition.Assign(oObject: TFslObject);
Begin
  Inherited;

  Port := TFslPrinterDefinition(oObject).Port;
  Driver := TFslPrinterDefinition(oObject).Driver;
  IsDefault := TFslPrinterDefinition(oObject).IsDefault;
End;


Function TFslPrinterDefinition.Link: TFslPrinterDefinition;
Begin
  Result := TFslPrinterDefinition(Inherited Link);
End;


Function TFslPrinterDefinition.Clone: TFslPrinterDefinition;
Begin
  Result := TFslPrinterDefinition(Inherited Clone);
End;


function TFslPrinterDefinition.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FPort.length * sizeof(char)) + 12);
  inc(result, (FDriver.length * sizeof(char)) + 12);
end;

Function TFslPrinterDefinitionList.GetByName(Const sName: String): TFslPrinterDefinition;
Begin
  Result := TFslPrinterDefinition(Inherited GetByName(sName));
End;


Function TFslPrinterDefinitionList.GetDefinition(iIndex: Integer): TFslPrinterDefinition;
Begin
  Result := TFslPrinterDefinition(ObjectByIndex[iIndex]);
End;


Function TFslPrinterDefinitionList.ItemClass: TFslObjectClass;
Begin
  Result := TFslPrinterDefinition;
End;


Function TFslPrinterDefinitionList.Link: TFslPrinterDefinitionList;
Begin
  Result := TFslPrinterDefinitionList(Inherited Link);
End;

Procedure TFslPrinterFont.Assign(oObject: TFslObject);
Begin
  Inherited;

  Pitch := TFslPrinterFont(oObject).Pitch;
  Types := TFslPrinterFont(oObject).Types;
End;


Function FontTypesToString(aTypes : TFslFontTypeSet) : String; Overload;
Var
  aLoop : TFslFontType;
Begin
  Result := '';

  For aLoop := Low(TFslFontType) To High(TFslFontType) Do
  Begin
    If aLoop In aTypes Then
      StringAppend(Result, TADVFONTTYPE_NAMES[aLoop], ',');
  End;

  Result := '[' + Result + ']';
End;


Function EnumFontsProc(Var LogFont: TLogFont; Var TextMetric: TTextMetric; FontType: Integer; Data: Pointer): Integer; Stdcall;

  Function GetFontTypes(iType : Integer) : TFslFontTypeSet;
  Begin
    Result := [];
    If iType And DEVICE_FONTTYPE > 0 Then
      Include(Result, aftDevice);

    If iType And RASTER_FONTTYPE > 0 Then
      Include(Result, aftRaster);

    If iType And TRUETYPE_FONTTYPE > 0 Then
      Include(Result, aftTrueType);
  End;

Begin
  If LogFont.lfPitchAndFamily And FIXED_PITCH > 0 Then
    TFslPrinterFontList(Data).Add(LogFont.lfFaceName, afpFixed, GetFontTypes(FontType))
  Else If LogFont.lfPitchAndFamily And VARIABLE_PITCH > 0 Then
    TFslPrinterFontList(Data).Add(LogFont.lfFaceName, afpVariable, GetFontTypes(FontType))
  Else
    TFslPrinterFontList(Data).Add(LogFont.lfFaceName, afpDontCare, GetFontTypes(FontType));

  Result := 1;
End;


function TFslPrinterFont.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslPrinterFontList.Add(Const sName : String; aPitch : TFslFontPitch; aTypes : TFslFontTypeSet);
Var
  oFont : TFslPrinterFont;
Begin
  oFont := TFslPrinterFont(ItemNew);
  Try
    oFont.Name := sName;
    oFont.Pitch := aPitch;
    oFont.Types := aTypes;

    Add(oFont.Link);
  Finally
    oFont.Free;
  End;
End;


Function TFslPrinterFontList.AsCSV : String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Font[iLoop].Name, ',');
End;


Procedure TFslPrinterFontList.Enumerate(aHandle: HDC);
Begin
  Clear;
  Unsorted;
  TFslSynchronisedPrinterAPI.EnumFonts(aHandle, Nil, @EnumFontsProc, pointer(Self));
  SortedByName;
End;


Function TFslPrinterFontList.GetByName(Const sName : String): TFslPrinterFont;
Begin
  Result := TFslPrinterFont(Inherited GetByName(sName));
End;


Function TFslPrinterFontList.GetFont(iIndex : Integer): TFslPrinterFont;
Begin
  Result := TFslPrinterFont(ObjectByIndex[iIndex]);
End;


Function TFslPrinterFontList.ItemClass : TFslObjectClass;
Begin
  Result := TFslPrinterFont;
End;



Procedure TFslPrinterPaperSize.Assign(oObject: TFslObject);
Begin
  Inherited;

  Size := TFslPrinterPaperSize(oObject).Size;
  Width := TFslPrinterPaperSize(oObject).Width;
  Height := TFslPrinterPaperSize(oObject).Height;
End;


Function TFslPrinterPaperSize.GetDimUnit(Var sDim : String; Const rUnit : Real) : Real;
Begin
  Result := rUnit;

  If Pos('mm', sDim) > 0 Then
  Begin
    Result := 10;
    Delete(sDim, Pos('mm', sDim), 2);
  End
  Else If Pos('''''', sDim) > 0 Then
  Begin
    Result := 254;
    Delete(sDim, Pos('''''', sDim), 2);
  End
  Else If Pos('"', sDim) > 0 Then
  Begin
    Result := 254;
    Delete(sDim, Pos('"', sDim), 1);
  End
  Else If Pos('cm', sDim) > 0 Then
  Begin
    Result := 100;
    Delete(sDim, Pos('cm', sDim), 1);
  End
End;


Function TFslPrinterPaperSize.AsText : String;
Begin
  If FSize = psUnknown Then
    Result := Name
  Else If FSize = psCustom Then
    Result := StringFormat('%s[%dx%d]', [Name, FHeight, FWidth])
  Else
    Result := ADVPAPERSIZE_CODES[FSize]
End;


Procedure TFslPrinterPaperSize.BuildFromPrinterText(Const sText : String);
Var
  sDim : String;
  sHeight : String;
  sWidth : String;
  rUnit : Real;
  sWorking : String;
Begin
  sWorking := sText;

  If (Pos('(', sWorking) > 0) And (Pos('x', sWorking) > Pos('(', sWorking)) And (Pos(')', sWorking) > Pos('(', sWorking)) Then
  Begin
    StringSplit(sWorking, '(', sWorking, sDim);
    sWorking := StringTrimWhitespace(sWorking);
    StringSplit(sDim, ')', sDim, sHeight); // drop last )
  End;

  If StringArrayExistsInsensitive(ADVPAPERSIZE_CODES, sWorking) Then
  Begin
    Name := sWorking;
    FSize := TFslPaperSize(StringArrayIndexOfInsensitive(ADVPAPERSIZE_CODES, sWorking));
    FHeight := ADVPAPERSIZE_HEIGHTS[FSize];
    FWidth := ADVPAPERSIZE_WIDTHS[FSize];
  End
  Else If StringArrayExistsInsensitive(ADVPAPERSIZE_NAMES, sWorking) Then
  Begin
    Name := sWorking;
    FSize := TFslPaperSize(StringArrayIndexOfInsensitive(ADVPAPERSIZE_NAMES, sWorking));
    FHeight := ADVPAPERSIZE_HEIGHTS[FSize];
    FWidth := ADVPAPERSIZE_WIDTHS[FSize];
  End
  Else If StringArrayExistsInsensitive(ADVPAPERSIZE_TITLES, sWorking) Then
  Begin
    Name := sWorking;
    FSize := TFslPaperSize(StringArrayIndexOfInsensitive(ADVPAPERSIZE_TITLES, sWorking));
    FHeight := ADVPAPERSIZE_HEIGHTS[FSize];
    FWidth := ADVPAPERSIZE_WIDTHS[FSize];
  End
  Else If sDim <> '' Then
  Begin
    Name := sWorking;
    FSize := psCustom;

    StringSplit(sDim, 'x', sHeight, sWidth);

    rUnit := GetDimUnit(sWidth, 1);

    If IsNumericString(sWidth) Then
      FWidth := Round(rUnit * StringToReal(sWidth))
    Else
      FWidth := 0;

    rUnit := GetDimUnit(sHeight, rUnit);

    If IsNumericString(sHeight) Then
      FHeight := Round(rUnit * StringToReal(sHeight))
    Else
      FHeight := 0;
  End
  Else
  Begin
    Name := sWorking;
    FSize := psUnknown;
  End;
End;

Function TFslPrinterPaperSizeList.AddFromText(Const sText : String) : Integer;
Var
  oSize : TFslPrinterPaperSize;
Begin
  oSize := TFslPrinterPaperSize(ItemNew);
  Try
    oSize.BuildFromPrinterText(sText);

    Result := Add(oSize.Link);
  Finally
    oSize.Free;
  End;
End;


Function TFslPrinterPaperSizeList.AsCSV: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count -1 Do
    StringAppend(Result, PageSize[iloop].AsText, ', ');
End;


Function TFslPrinterPaperSizeList.GetPageSize(iIndex: Integer): TFslPrinterPaperSize;
Begin
  Result := TFslPrinterPaperSize(ObjectByIndex[iIndex]);
End;


Function TFslPrinterPaperSizeList.ItemClass: TFslObjectClass;
Begin
  Result := TFslPrinterPaperSize;
End;


Procedure TFslPrinterTray.Assign(oObject: TFslObject);
Begin
  Inherited;

  Name := TFslPrinterTray(oObject).Name;
  ID := TFslPrinterTray(oObject).ID;
End;


Function TFslPrinterTray.AsText: String;
Begin
  If FID In [1..15] Then
    Result := FName + ':['+ ADVPRINTERBINTYPE_NAMES[FID] + ']'
  Else
    Result := FName + ':'+ IntegerToString(FID);
End;


Function TFslPrinterTray.Link: TFslPrinterTray;
Begin
  Result := TFslPrinterTray(Inherited Link);
End;


function TFslPrinterTray.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
end;

Function TFslPrinterTrayList.CompareByID(pA, pB: Pointer): Integer;
Begin
  Result := TFslPrinterTray(pA).FID - TFslPrinterTray(pB).FID;
End;


Function TFslPrinterTrayList.CompareByName(pA, pB: Pointer): Integer;
Begin
  Result := StringCompare(TFslPrinterTray(pA).FName, TFslPrinterTray(pB).FName);
End;


Function TFslPrinterTrayList.ExistsByID(iID: Integer): Boolean;
Begin
  Result := ExistsByIndex(IndexByID(iID));
End;


Function TFslPrinterTrayList.ExistsByName(Const sName: String): Boolean;
Begin
  Result := ExistsByIndex(IndexByName(sName));
End;


Function TFslPrinterTrayList.GetByID(iID: Integer): TFslPrinterTray;
Begin
  Result := TFslPrinterTray(Get(IndexByID(iID)));
End;


Function TFslPrinterTrayList.GetByName(Const sName: String): TFslPrinterTray;
Begin
  Result := TFslPrinterTray(Get(IndexByName(sName)));
End;


Function TFslPrinterTrayList.IndexByID(iID: Integer): Integer;
Var
  oPrinterTray : TFslPrinterTray;
Begin
  oPrinterTray := TFslPrinterTray(ItemNew);
  Try
    oPrinterTray.FID := iID;

    If Not Find(oPrinterTray, Result, CompareByID) Then
      Result := -1;
  Finally
    oPrinterTray.Free;
  End;
End;


Function TFslPrinterTrayList.IndexByName(Const sName: String): Integer;
Var
  oPrinterTray : TFslPrinterTray;
Begin
  oPrinterTray := TFslPrinterTray(ItemNew);
  Try
    oPrinterTray.Name := sName;

    If Not Find(oPrinterTray, Result, CompareByName) Then
      Result := -1;
  Finally
    oPrinterTray.Free;
  End;
End;


Function TFslPrinterTrayList.GetTray(iIndex: Integer): TFslPrinterTray;
Begin
  Result := TFslPrinterTray(ObjectByIndex[iIndex]);
End;


Function TFslPrinterTrayList.ItemClass: TFslObjectClass;
Begin
  Result := TFslPrinterTray;
End;


Procedure TFslPrinterTrayList.Add(Const sName: String; Const iID: Integer);
Var
  oTray : TFslPrinterTray;
Begin
  oTray := TFslPrinterTray(ItemNew);
  Try
    oTray.Name := sName;
    oTray.ID := iID;

    Add(oTray.Link);
  Finally
    oTray.Free;
  End;
End;


Function TFslPrinterTrayList.AsCSV: String;
Var
  iLoop : Integer;
Begin
  Result := '';
  For iLoop := 0 To Count - 1 Do
    StringAppend(Result, Trays[iLoop].AsText, ', ');
End;



Constructor TFslPrinterSettings.Create;
Begin
  Inherited;

  FDefinition := Nil;
End;


Destructor TFslPrinterSettings.Destroy;
Begin
  Close;
  FDefinition.Free;

  Inherited;
End;


Function TFslPrinterSettings.GetOpened: Boolean;
Begin
  Result := Assigned(FDevice);
End;


Procedure TFslPrinterSettings.Open;
{$IFDEF VER130}
Var
  aStubDevMode: TDeviceMode;
{$ENDIF}
Begin
  Inherited;

  If Not TFslSynchronisedPrinterAPI.OpenPrinter(PChar(Definition.Name), FPrinterHandle, Nil) Then
    RaiseError('PrinterOpen', ErrorAsString);

  {$IFNDEF VER130}
  FDeviceSize := TFslSynchronisedPrinterAPI.DocumentProperties(0, FPrinterHandle, PChar(Definition.Name), 0);
  {$ELSE}
  FDeviceSize := TFslSynchronisedPrinterAPI.DocumentProperties(0, FPrinterHandle, PChar(Definition.Name), aStubDevMode, aStubDevMode, 0);
  {$ENDIF}
  FDeviceHandle := GlobalAlloc(GHND, FDeviceSize);
  If FDeviceHandle <> 0 Then
    FDevice := GlobalLock(FDeviceHandle);

  If TFslSynchronisedPrinterAPI.DocumentProperties(0, FPrinterHandle, PChar(Definition.Name), FDevice^, FDevice^, DM_OUT_BUFFER) < 0 Then
    RaiseError('PrinterOpen', StringFormat('Unable to Open Printer %s: %s', [Definition.Name, ErrorAsString]));
End;


Procedure TFslPrinterSettings.Close;
Begin
  Inherited;

  If FDeviceHandle <> 0 Then
  Begin
    FDevice := Nil;
    GlobalUnlock(FDeviceHandle);
    GlobalFree(FDeviceHandle);
    FDeviceHandle := 0;
  End;

  If FPrinterHandle <> 0 Then
  Begin
    TFslSynchronisedPrinterAPI.ClosePrinter(FPrinterHandle);
    FPrinterHandle := 0;
  End;
End;


Function TFslPrinterSettings.Clone: TFslPrinterSettings;
Begin
  Result := TFslPrinterSettings(Inherited Clone);
End;


Function TFslPrinterSettings.Link: TFslPrinterSettings;
Begin
  Result := TFslPrinterSettings(Inherited Link);
End;


Procedure TFslPrinterSettings.Assign(oObject: TFslObject);
Begin
  Inherited;

  // note that order is important; Definition first, and readOnly last
  Definition := TFslPrinterSettings(oObject).Definition.Clone;

  Open;

  // we could copy the properties, but this will miss the other stuff
  // we haven't wrapped that is accessible through document properties
  If FDeviceSize = TFslPrinterSettings(oObject).FDeviceSize Then
    Move(TFslPrinterSettings(oObject).FDevice^, FDevice^, FDeviceSize)
  Else
  Begin
    Copies := TFslPrinterSettings(oObject).Copies;
    Duplex := TFslPrinterSettings(oObject).Duplex;
    Orientation := TFslPrinterSettings(oObject).Orientation;
    PageHeight := TFslPrinterSettings(oObject).PageHeight;
    PageSize := TFslPrinterSettings(oObject).PageSize;
    PageWidth := TFslPrinterSettings(oObject).PageWidth;
    Quality := TFslPrinterSettings(oObject).Quality;
    Tray := TFslPrinterSettings(oObject).Tray;
    UseColour := TFslPrinterSettings(oObject).UseColour;
  End;

  ReadOnly := TFslPrinterSettings(oObject).ReadOnly;
End;



Function TFslPrinterSettings.GetCopies: SmallInt;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  Result := FDevice^.dmCopies;
End;


Procedure TFslPrinterSettings.SetCopies(Const iValue: SmallInt);
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot change the set up of the print job because the printer has not been opened');

  If ReadOnly Then
    RaiseError('SetCopies', 'Cannot change the set up of a print job while printing is in progress');

  FDevice^.dmCopies := iValue;
End;


Function TFslPrinterSettings.GetOrientation: TFslPrinterOrientation;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  If FDevice^.dmOrientation = DMORIENT_PORTRAIT Then
    Result := poPortrait
  Else
    Result := poLandscape;
End;


Procedure TFslPrinterSettings.SetOrientation(Const aValue: TFslPrinterOrientation);
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot change the set up of the print job because the printer has not been opened');

  If ReadOnly Then
    RaiseError('SetCopies', 'Cannot change the set up of a print job while printing is in progress');

  If aValue = poPortrait Then
    FDevice^.dmOrientation := DMORIENT_PORTRAIT
  Else If aValue = poLandscape Then
    FDevice^.dmOrientation := DMORIENT_LANDSCAPE
End;


Function TFslPrinterSettings.GetUseColour: Boolean;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  Result := FDevice^.dmColor = DMCOLOR_COLOR;
End;


Procedure TFslPrinterSettings.SetUseColour(Const bValue: Boolean);
Begin
  If ReadOnly Then
    RaiseError('SetUseColour', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetUseColour', 'Cannot change the set up of the print job because the printer has not been opened');

  If bValue Then
    FDevice^.dmColor := DMCOLOR_COLOR
  Else
    FDevice^.dmColor := DMCOLOR_MONOCHROME
End;


Function TFslPrinterSettings.GetDuplex : TFslPrintDuplex;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  Case FDevice^.dmDuplex Of
    DMDUP_SIMPLEX : Result := pdSimplex;
    DMDUP_HORIZONTAL : Result := pdHorizontal;
    DMDUP_VERTICAL : Result := pdVertical;
  Else
   Result := pdSimplex;
  End;
End;


Procedure TFslPrinterSettings.SetDuplex(Const aValue: TFslPrintDuplex);
Begin
  If ReadOnly Then
    RaiseError('SetDuplex', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetDuplex', 'Cannot change the set up of the print job because the printer has not been opened');

  Case aValue Of
    pdSimplex : FDevice^.dmDuplex := DMDUP_SIMPLEX;
    pdHorizontal : FDevice^.dmDuplex := DMDUP_HORIZONTAL;
    pdVertical : FDevice^.dmDuplex := DMDUP_VERTICAL;
  Else
    FDevice^.dmDuplex := DMDUP_SIMPLEX;
  End;
End;


Function TFslPrinterSettings.GetQuality : TFslPrintQuality;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  Case LongWord(FDevice^.dmDuplex) Of
    DMRES_HIGH : Result := pqHigh;
    DMRES_MEDIUM : Result := pqMedium;
    DMRES_LOW : Result := pqLow;
    DMRES_DRAFT : Result := pqDraft;
  Else
    Result := pqUnknown;
  End;
End;


Procedure TFslPrinterSettings.SetQuality(Const aValue: TFslPrintQuality);
Begin
  If ReadOnly Then
    RaiseError('SetQuality', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetQuality', 'Cannot change the set up of the print job because the printer has not been opened');

  Case aValue Of
    pqHigh : FDevice^.dmDuplex := Short(DMRES_HIGH);
    pqMedium : FDevice^.dmDuplex := Short(DMRES_MEDIUM);
    pqLow : FDevice^.dmDuplex := Short(DMRES_LOW);
    pqDraft : FDevice^.dmDuplex := Short(DMRES_DRAFT);
  End;
End;


Function TFslPrinterSettings.GetTray: Integer;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');
  Result := FDevice^.dmDefaultSource;
End;


Procedure TFslPrinterSettings.SetTray(Const iValue: Integer);
Begin
  If ReadOnly Then
    RaiseError('SetTray', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetTray', 'Cannot change the set up of the print job because the printer has not been opened');

  FDevice^.dmDefaultSource := iValue;
End;


Function TFslPrinterSettings.GetPageHeight : TFslGraphicMetre;
Begin
  If Not Opened Then
    RaiseError('GetPageHeight', 'Cannot read the set up of the print job because the printer has not been opened');

  Result := FDevice^.dmPaperLength;
End;


Procedure TFslPrinterSettings.SetPageHeight(Const iValue: TFslGraphicMetre);
Begin
  If ReadOnly Then
    RaiseError('SetPageHeight', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetPageHeight', 'Cannot change the set up of the print job because the printer has not been opened');

  FDevice^.dmPaperLength := iValue;
End;


Function TFslPrinterSettings.GetPageWidth : TFslGraphicMetre;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  Result := FDevice^.dmPaperWidth;
End;


Procedure TFslPrinterSettings.SetPageWidth(Const iValue: TFslGraphicMetre);
Begin
  If ReadOnly Then
    RaiseError('SetPageWidth', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetPageWidth', 'Cannot change the set up of the print job because the printer has not been opened');

  FDevice^.dmPaperWidth := iValue;
End;


Function IndexOfSmallInt(Const aValues : Array Of SmallInt; Const iValue : SmallInt): Integer;
Begin
  Result := High(aValues);
  While (Result >= Low(aValues)) And (iValue <> aValues[Result]) Do
    Dec(Result);
End;


Function TFslPrinterSettings.GetPageSize : TFslPaperSize;
Var
  iIndex : Integer;
Begin
  If Not Opened Then
    RaiseError('SetCopies', 'Cannot read the set up of the print job because the printer has not been opened');

  iIndex := IndexOfSmallInt(ADVPAPERSIZE_CONSTS, FDevice^.dmPaperSize);

  If iIndex = -1 Then
    Result := psCustom
  Else
    Result := TFslPaperSize(iIndex);
End;


Procedure TFslPrinterSettings.SetPageSize(Const aValue: TFslPaperSize);
Begin
  If ReadOnly Then
    RaiseError('SetPageSize', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetPageSize', 'Cannot change the set up of the print job because the printer has not been opened');

  FDevice^.dmPaperSize := ADVPAPERSIZE_CONSTS[aValue];
End;


Function TFslPrinterSettings.GetLimitLeft: TFslGraphicMetre;
Var
  iValue : DWord;
Begin
  iValue := TFslSynchronisedPrinterAPI.DeviceCapabilities(PChar(Definition.Name), PChar(Definition.Port), DC_MINEXTENT, Nil, FDevice);

  Result := iValue And $FFFF;
End;


Function TFslPrinterSettings.GetLimitTop: TFslGraphicMetre;
Var
  iValue : DWord;
Begin
  iValue := TFslSynchronisedPrinterAPI.DeviceCapabilities(PChar(Definition.Name), PChar(Definition.Port), DC_MINEXTENT, Nil, FDevice);
  Result := iValue Shr 16;
End;


Function TFslPrinterSettings.GetLimitBottom: TFslGraphicMetre;
Var
  iValue : DWord;
Begin
  iValue := TFslSynchronisedPrinterAPI.DeviceCapabilities(PChar(Definition.Name), PChar(Definition.Port), DC_MAXEXTENT, Nil, FDevice);
  Result := ActualHeight - Integer(iValue Shr 16);
End;


Function TFslPrinterSettings.GetLimitRight: TFslGraphicMetre;
Var
  iValue : DWord;
Begin
  iValue := TFslSynchronisedPrinterAPI.DeviceCapabilities(PChar(Definition.Name), PChar(Definition.Port), DC_MAXEXTENT, Nil, FDevice);
  Result := ActualWidth - Integer(iValue And $FFFF);
End;


Function TFslPrinterSettings.PropertiesDialog(aHandle: THandle) : Boolean;
Begin
  Result := TFslSynchronisedPrinterAPI.AdvancedDocumentProperties(aHandle, FPrinterHandle, PChar(Definition.Name), FDevice, FDevice) = IDOK;
End;


Function TFslPrinterSettings.GetDefinition: TFslPrinterDefinition;
Begin
  Assert(Invariants('GetDefinition', FDefinition, TFslPrinterDefinition, 'FDefinition'));

  Result := FDefinition;
End;


Procedure TFslPrinterSettings.SetDefinition(oValue : TFslPrinterDefinition);
Begin
  Assert(Not Assigned(oValue) Or Invariants('SetDefinition', oValue, TFslPrinterDefinition, 'oValue'));

  FDefinition.Free;
  FDefinition := oValue;
End;


function TFslPrinterSettings.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinition.sizeInBytes);
end;

Procedure TFslJobSettings.Open;
Begin
  FOpened := True;
End;


Procedure TFslJobSettings.Close;
Begin
  FOpened := False;
End;


Function TFslJobSettings.Clone: TFslJobSettings;
Begin
  Result := TFslJobSettings(Inherited Clone);
End;


Function TFslJobSettings.Link: TFslJobSettings;
Begin
  Result := TFslJobSettings(Inherited Link);
End;


Function TFslJobSettings.GetOrientation: TFslPrinterOrientation;
Begin
  Result := poNotSpecified;
End;


Function TFslJobSettings.GetPageSize: TFslPaperSize;
Begin
  Result := psUnknown;
End;


Function TFslJobSettings.GetPageHeight: TFslGraphicMetre;
Begin
  Result := 0;
End;


Function TFslJobSettings.GetPageWidth: TFslGraphicMetre;
Begin
  Result := 0;
End;


Function TFslJobSettings.GetUseColour: Boolean;
Begin
  Result := False;
End;


Procedure TFslJobSettings.SetUseColour(Const bValue: Boolean);
Begin
End;


Procedure TFslJobSettings.SetOrientation(Const aValue: TFslPrinterOrientation);
Begin
End;


Procedure TFslJobSettings.SetPageHeight(Const iValue: TFslGraphicMetre);
Begin
End;


Procedure TFslJobSettings.SetPageSize(Const aValue: TFslPaperSize);
Begin
End;


Procedure TFslJobSettings.SetPageWidth(Const iValue: TFslGraphicMetre);
Begin
End;


Function TFslJobSettings.ActualHeight: TFslGraphicMetre;
Begin
  If PageSize In [psUnknown, psCustom] Then
    Result := PageHeight
  Else
  Begin
    If Orientation = poLandscape Then
      Result := ADVPAPERSIZE_WIDTHS[PageSize]
    Else
      Result := ADVPAPERSIZE_HEIGHTS[PageSize];
  End;
End;


Function TFslJobSettings.ActualWidth: TFslGraphicMetre;
Begin
  If PageSize In [psUnknown, psCustom] Then
    Result := PageWidth
  Else
  Begin
    If Orientation = poLandscape Then
      Result := ADVPAPERSIZE_HEIGHTS[PageSize]
    Else
      Result := ADVPAPERSIZE_WIDTHS[PageSize];
  End;
End;


function TFslJobSettings.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

Procedure TFslPrinterJob.InitialiseDC;
Var
  aInfo : TDocInfo;
  sTitle : String;
Begin
  Inherited;

  DC := TFslSynchronisedPrinterAPI.CreateDC(Nil, PChar(Settings.Definition.Name), Nil, Settings.Device);
  If DC = 0 Then
    RaiseError('InitialiseDC', StringFormat('Unable to open printer %s: %s', [Settings.Definition.Name, ErrorAsString]));

  FillChar(aInfo, SizeOf(aInfo), 0);
  aInfo.cbSize := SizeOf(aInfo);

  sTitle := Title;
  If Settings.JobNumber <> 0 Then
    StringAppend(sTitle, StringFormat('(Job %d)', [Settings.JobNumber]), ' - ');
  aInfo.lpszDocName := PChar(sTitle);

  If DestinationFileName <> '' Then
    aInfo.lpszOutput := PChar(DestinationFileName);

  StartDoc(DC, aInfo);
  StartPage(DC);
End;


Procedure TFslPrinterJob.InitialiseCanvas;
Begin
  Inherited;

  Canvas.Height := Settings.ActualHeight;
  Canvas.Width := Settings.ActualWidth;
End;


Procedure TFslPrinterJob.OpenDefinition(oDefinition: TFslPrinterDefinition);
Begin
  Settings.Definition := oDefinition;

  Open;
End;


Function TFslPrinterJob.GetSettings: TFslPrinterSettings;
Begin
  Result := TFslPrinterSettings(Inherited Settings);
End;


Function TFslPrinterJob.SettingsClass: TFslJobSettingsClass;
Begin
  Result := TFslPrinterSettings;
End;


Procedure TFslPrinterJob.SetTitle(Const sValue : String);
Begin
  If Started Then
    RaiseError('SetTitle', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetTitle', 'Cannot change the set up of the print job because the printer has not been opened');

  Inherited;
End;


Procedure TFslRenderDeviceJob.InternalStart;
Begin
  Inherited;

  InitialiseDC;
  InitialiseCanvas;
End;


Procedure TFslRenderDeviceJob.InternalNewPage;
Begin
  Inherited;

  EndPage(FDC);
  StartPage(FDC);

  Canvas.Refresh;
End;


Procedure TFslRenderDeviceJob.InternalAbort;
Begin
  Inherited;

  AbortDoc(FDC);
  DeleteDC(FDC);
End;


Procedure TFslRenderDeviceJob.InternalFinish;
Begin
  Inherited;

  EndPage(FDC);
  EndDoc(FDC);
  DeleteDC(FDC);
End;


Procedure TFslRenderDeviceJob.InitialiseDC;
Begin
End;


Procedure TFslRenderDeviceJob.InitialiseCanvas;
Begin
  RecreateCanvas;

  Canvas.AttachToPrinter(FDC);
End;

Constructor TFslRenderJob.Create;
Begin
  Inherited;

  FSettings := SettingsClass.Create;
  FCanvas := Nil;
End;


Destructor TFslRenderJob.Destroy;
Begin
  FSettings.Free;
  FCanvas.Free;

  Inherited;
End;


Function TFslRenderJob.Link: TFslRenderJob;
Begin
  Result := TFslRenderJob(Inherited Link);
End;


Procedure TFslRenderJob.Start;
Begin
  If FStarted Then
    RaiseError('Start', 'This job has already started');
  If Not Opened Then
    RaiseError('Start', 'Unable to start the print Job because the printer has not been opened');

  FPageNumber := 1;
  InternalStart;
  FStarted := True;
End;


Procedure TFslRenderJob.NewPage;
Begin
  If Not FStarted Then
    RaiseError('NewPage', 'This job has not yet been started');

  InternalNewPage;

  Inc(FPageNumber);
End;


Procedure TFslRenderJob.Abort;
Begin
  If Not FStarted Then
    RaiseError('Abort', 'This job has not yet been started');

  InternalAbort;
  FStarted := False;
End;


Procedure TFslRenderJob.Finish;
Begin
  If Not FStarted Then
    RaiseError('Finish', 'This job has not yet been started');

  InternalFinish;
  FStarted := False;
End;


Procedure TFslRenderJob.SetTitle(Const sValue: String);
Begin
  FTitle := sValue;
End;


Procedure TFslRenderJob.InternalAbort;
Begin
End;


Procedure TFslRenderJob.InternalFinish;
Begin
End;


Procedure TFslRenderJob.InternalNewPage;
Begin
End;


Procedure TFslRenderJob.InternalStart;
Begin
End;


Procedure TFslRenderJob.SetDestinationFileName(Const Value: String);
Begin
  If FStarted Then
    RaiseError('SetTitle', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetTitle', 'Cannot change the set up of the print job because the printer has not been opened');

  FDestinationFileName := Value;
End;


Function TFslRenderJob.GetCanvas: TFslPrinterCanvas;
Begin
  Assert(CheckCondition(Assigned(FCanvas), 'FCanvas', 'Canvas is not yet assigned.'));

  Result := FCanvas;
End;


Function TFslRenderJob.CanvasClass: TFslPrinterCanvasClass;
Begin
  Result := TFslPrinterCanvas;
End;


Procedure TFslRenderJob.RecreateCanvas;
Begin
  FCanvas.Free;
  FCanvas := Nil;
  FCanvas := CanvasClass.Create;
End;


Procedure TFslRenderJob.Open;
Begin
  If Not Opened Then
    FSettings.Open;
End;


Procedure TFslRenderJob.OpenSettings(oSettings : TFslJobSettings);
Begin
  FSettings.Assign(oSettings);
  Open;
End;


Procedure TFslRenderJob.Close;
Begin
  FSettings.Close;
End;


Function TFslRenderJob.GetOpened: Boolean;
Begin
  Result := FSettings.Opened;
End;


Function TFslRenderJob.SettingsClass: TFslJobSettingsClass;
Begin
  Result := TFslJobSettings;
End;


Procedure TFslRenderJob.Clear;
Begin
  FTitle := '';
  FStarted := False;
  FPageNumber := 0;
  FDestinationFileName := '';
End;



function TFslRenderJob.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCanvas.sizeInBytes);
  inc(result, FSettings.sizeInBytes);
  inc(result, (FTitle.length * sizeof(char)) + 12);
  inc(result, (FDestinationFileName.length * sizeof(char)) + 12);
end;

Procedure TFslPrinterDelphiCanvas.CreateHandle;
Begin
  UpdateFont;
  Handle := FOwner.Handle;
End;


Procedure TFslPrinterDelphiCanvas.Changing;
Begin
  Inherited;

  UpdateFont;
End;


Procedure TFslPrinterDelphiCanvas.UpdateFont;
Var
  iFontSize: Integer;
Begin
  If FAdjustFont And (FOwner.PixelsPerInchY <> Font.PixelsPerInch) Then
  Begin
    iFontSize := Font.Size;
    Font.PixelsPerInch := FOwner.PixelsPerInchY;
    Font.Size := iFontSize;
  End;
End;


Function TFslPrinterDelphiCanvas.GetPixelsPerInchY: Integer;
Begin
  Result := FOwner.PixelsPerInchY;
End;


Function TFslPrinterDelphiCanvas.GetPixelsPerInchX: Integer;
Begin
  Result := FOwner.PixelsPerInchX;
End;


Procedure TFslPrinterDelphiCanvas.SetMapMode(iMode: Integer);
Begin
  If Windows.SetMapMode(FOwner.Handle, iMode) = 0 Then
    FOwner.LastWindowsError('SetMapMode');

  FNeedReset := True;
End;


Procedure TFslPrinterDelphiCanvas.SetWindowExtEx(i1, i2: Integer);
Begin
  If Not Windows.SetWindowExtEx(FOwner.Handle, i1, i2, Nil) Then
    FOwner.LastWindowsError('SetWindowExtEx');

  FNeedReset := True;
End;


Procedure TFslPrinterDelphiCanvas.SetViewPortExtEx(i1, i2: Integer);
Begin
  If Not Windows.SetViewPortExtEx(FOwner.Handle, i1, i2, Nil) Then
    FOwner.LastWindowsError('SetViewPortExtEx');

  FNeedReset := True;
End;


Function TFslPrinterDelphiCanvas.GetHorizontalResolution: Integer;
Begin
  Result := GetDeviceCaps(FOwner.Handle, HORZRES);
End;


Function TFslPrinterDelphiCanvas.GetVerticalResolution: Integer;
Begin
  Result := GetDeviceCaps(FOwner.Handle, VERTRES);
End;


Procedure TFslPrinterDelphiCanvas.SetWindowOrgEx(i1, i2: Integer);
Begin
  If Not Windows.SetWindowOrgEx(FOwner.Handle, i1, i2, Nil) Then
    FOwner.LastWindowsError('SetWindowOrgEx');

  FNeedReset := True;
End;


Procedure TFslPrinterDelphiCanvas.SelectClipRgn(iHnd: THandle);
Begin
  If Windows.SelectClipRgn(FOwner.Handle, iHnd) = 0 Then
    FOwner.LastWindowsError('SelectClipRgn');

  FNeedReset := True;
End;


Function TFslPrinterDelphiCanvas.SetTextAlign(iMode: Cardinal) : Cardinal;
Begin
  Result := Windows.SetTextAlign(FOwner.Handle, iMode);

  If Result = GDI_ERROR Then
    FOwner.LastWindowsError('SetTextAlign');

  FNeedReset := True;
End;


Procedure TFslPrinterDelphiCanvas.ResetMetrics;
Begin
  If FNeedReset Then
  Begin
    SetMapMode(FOrigMapMode);
    SetWindowExtEx(FOrigWin.cx, FOrigWin.cy);
    SetViewPortExtEx(FOrigView.cx, FOrigView.cy);
    SetWindowOrgEx(FOrigOrg.x, FOrigOrg.y);
    SelectClipRgn(0);
    SetTextAlign(FOrigAlign);

    FNeedReset := False;
  End;
End;


Constructor TFslPrinterCanvas.Create;
Begin
  Inherited;

  FPen := TFslPen.Create;
  FPen.Capability := Self;

  FBrush := TFslBrush.Create;
  FBrush.Capability := Self;

  FFont := TFslFont.Create;
  FFont.Capability := Self;
End;


Destructor TFslPrinterCanvas.Destroy;
Begin
  FPen.Free;
  FBrush.Free;
  FFont.Free;
  FCanvas.Free;

  Inherited;
End;


Function TFslPrinterCanvas.Link : TFslPrinterCanvas;
Begin
  Result := TFslPrinterCanvas(Inherited Link);
End;


Procedure TFslPrinterCanvas.Refresh;
Begin
  FCanvas.Refresh;
  FFont.Clear;
  FPen.Clear;
  FBrush.Clear;
  FCanvas.ResetMetrics;
End;


Procedure TFslPrinterCanvas.SetBrush(oBrush: TFslBrush);
Begin
  FBrush.Free;
  FBrush := oBrush;

  FBrush.Capability := Self;
  FBrush.ClearHandle;
End;


Procedure TFslPrinterCanvas.SetFont(oFont: TFslFont);
Begin
  FFont.Free;
  FFont := oFont;

  FFont.Capability := Self;
  FFont.ClearHandle;
End;


Procedure TFslPrinterCanvas.SetPen(oPen: TFslPen);
Begin
  FPen.Free;
  FPen := oPen;

  FPen.Capability := Self;
  FPen.ClearHandle;
End;


Procedure TFslPrinterCanvas.AttachToPrinter(aHandle : TFslGraphicHandle);
Begin
  FCanvas.Free;
  FCanvas := Nil;

  Handle := aHandle;

  SetGraphicsMode(Handle, GM_ADVANCED);

  PixelsPerInchY := GetDeviceCaps(Handle, LOGPIXELSY);
  PixelsPerInchX := GetDeviceCaps(Handle, LOGPIXELSX);
  PixelsPerGraphicMetreX := GetDeviceCaps(Handle, LOGPIXELSX) / 254;
  PixelsPerGraphicMetreY := PixelsPerInchY / 254;
  PixelsPerGraphicMetre := (PixelsPerGraphicMetreY + PixelsPerGraphicMetreX) / 2;

  OffsetPixelsX := GetDeviceCaps(Handle, PhysicalOffsetX);
  OffsetPixelsY := GetDeviceCaps(Handle, PhysicalOffsetY);

  If PixelsPerGraphicMetreX = 0 Then
    OffsetX := 0
  Else
    OffsetX := Round(OffsetPixelsX / PixelsPerGraphicMetreX);

  If PixelsPerGraphicMetreY = 0 Then
    OffsetY := 0
  Else
    OffsetY := Round(OffsetPixelsY / PixelsPerGraphicMetreY);

  FCanvas := TFslPrinterDelphiCanvas.Create;
  FCanvas.OwnerCanvas := Self;
End;


Procedure TFslPrinterCanvas.AttachToPreview(aHandle: TFslGraphicHandle; iPixelWidth, iPixelHeight: Integer; iLogicalWidth, iLogicalHeight: TFslGraphicMetre);
Begin
  FCanvas.Free;
  FCanvas := Nil;

  Handle := aHandle;

  SetGraphicsMode(Handle, GM_ADVANCED);

  If iLogicalWidth = 0 Then
    PixelsPerGraphicMetreX := 0
  Else
    PixelsPerGraphicMetreX := iPixelWidth / iLogicalWidth;

  If iLogicalHeight = 0 Then
    PixelsPerGraphicMetreY := 0
  Else
    PixelsPerGraphicMetreY := iPixelHeight / iLogicalHeight;

  PixelsPerGraphicMetre := (PixelsPerGraphicMetreY + PixelsPerGraphicMetreX) / 2;
  PixelsPerInchY := Round(PixelsPerGraphicMetreY * 254);
  PixelsPerInchX := Round(PixelsPerGraphicMetreX * 254);

  OffsetX := 0;
  OffsetY := 0;

  FHeight := iLogicalHeight;
  FWidth := iLogicalWidth;

  FCanvas := TFslPrinterDelphiCanvas.Create;
  FCanvas.OwnerCanvas := Self;
End;


Function TFslPrinterCanvas.SelectBrush : THandle;
Begin
  Result := SelectObject(Handle, FBrush.Handle);

  If Result = 0 Then
    RaiseError('SelectBrush', ErrorAsString);

  If FBrush.Style In [absNull, absHorizontal, absVertical, absFDiagonal, absBDiagonal, absCross, absDiagCross] Then
    SetBkMode(Handle, TRANSPARENT)
  Else
  Begin
    SetBkMode(Handle, OPAQUE);
    SetBkColor(Handle, FBrush.Colour);
  End;
End;


Function TFslPrinterCanvas.SelectFont : THandle;
Begin
  Result := SelectObject(Handle, FFont.Handle);

  If Result = 0 Then
    RaiseError('SelectFont', ErrorAsString);

  If SetTextColor(Handle, ColorToRGB(FFont.Colour)) = CLR_INVALID Then
    RaiseError('SelectFont', ErrorAsString);
End;


Function TFslPrinterCanvas.SelectPen : THandle;
Begin
  Result := SelectObject(Handle, FPen.Handle);

  If Result = 0 Then
    RaiseError('SelectPen', ErrorAsString);
End;


Procedure TFslPrinterCanvas.RestoreHandle(aOld: THandle);
Begin
  If SelectObject(Handle, aOld) = 0 Then
    RaiseError('RestoreHandle', ErrorAsString);
End;


Procedure TFslPrinterCanvas.LinePixels(iX, iY, iLen: Integer; iAngle : TFslGraphicAngle);
Var
  rRad : Extended;
  aPen : THandle;
Begin
  FCanvas.ResetMetrics;
  aPen := SelectPen;
  Windows.MoveToEx(Handle, iX  - OffsetPixelsX, iY  - OffsetPixelsY, Nil);
  rRad := DegreesToRadians(360-iAngle);
  Windows.LineTo(Handle, iX - OffsetPixelsX + Trunc(iLen * Cos(rRad)), iY - OffsetPixelsY + Trunc(iLen * Sin(rRad)));
  RestoreHandle(aPen);
End;


Procedure TFslPrinterCanvas.LinePixelsXY(iX1, iY1, iX2, iY2 : Integer);
Var
  aPen : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  Try
    Windows.MoveToEx(Handle, iX1  - OffsetPixelsX, iY1  - OffsetPixelsY, Nil);
    Windows.LineTo(Handle, iX2 - OffsetPixelsX, iY2 - OffsetPixelsY);
  Finally
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.Line(iX, iY, iLen: TFslGraphicMetre; iAngle : TFslGraphicAngle);
Var
  rRad : Extended;
  aPen : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  Try
    Windows.MoveToEx(Handle, ToPixelX(iX), ToPixelY(iY), Nil);

    rRad := DegreesToRadians(360 - iAngle);

    Windows.LineTo(Handle, ToPixelX(iX+Trunc(iLen * Cos(rRad))), ToPixelY(iY+Trunc(iLen * Sin(rRad))));
  Finally
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.PolyLineOffset(aPoints: Array Of TPoint);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aPoints) To High(aPoints) Do
  Begin
    aPoints[iLoop].x := aPoints[iLoop].x - OffsetPixelsX;
    aPoints[iLoop].y := aPoints[iLoop].y - OffsetPixelsY;
  End;
  PolyLine(aPoints);
End;

Procedure TFslPrinterCanvas.PolygonOffset(aPoints: Array Of TPoint);
Var
  iLoop : Integer;
Begin
  For iLoop := Low(aPoints) To High(aPoints) Do
  Begin
    aPoints[iLoop].x := aPoints[iLoop].x - OffsetPixelsX;
    aPoints[iLoop].y := aPoints[iLoop].y - OffsetPixelsY;
  End;
  Polygon(aPoints);
End;


Procedure TFslPrinterCanvas.Polygon(Const aPoints: Array Of TPoint);
Type
  PPoints = ^TPoints;
  TPoints = Array[0..0] Of TPoint;
Var
  aPen : THandle;
  aBrush : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  aBrush := SelectBrush;
  Try
    Windows.Polygon(Handle, PPoints(@aPoints)^, High(aPoints) + 1);
  Finally
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.Polyline(Const aPoints: Array Of TPoint);
Type
  PPoints = ^TPoints;
  TPoints = Array[0..0] Of TPoint;
Var
  aPen : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  Try
    Windows.Polyline(Handle, PPoints(@aPoints)^, High(aPoints) + 1);
  Finally
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.BoxPixels(iX, iY, iWidth, iHeight: TFslGraphicMetre);
Var
  aPoints : Array Of TPoint;
Begin
  SetLength(aPoints, 4);

  aPoints[0].x := iX - OffsetPixelsX;
  aPoints[0].y := iY - OffsetPixelsY;
  aPoints[1].x := iX + iWidth - OffsetPixelsX;
  aPoints[1].y := iY - OffsetPixelsY;
  aPoints[2].x := iX + iWidth - OffsetPixelsX;
  aPoints[2].y := iY + iHeight - OffsetPixelsY;
  aPoints[3].x := iX - OffsetPixelsX;
  aPoints[3].y := iY + iHeight - OffsetPixelsY;

  Polygon(aPoints);
End;


Procedure TFslPrinterCanvas.BoxRectangle(iLeft, iTop, iRight, iBottom : TFslGraphicMetre; iAngle : TFslGraphicAngle = 0);
Begin
  Box(iLeft, iTop, iRight - iLeft, iBottom - iTop, iAngle);
End;


Procedure TFslPrinterCanvas.Box(iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle : TFslGraphicAngle);
Var
  aPoints : Array Of TPoint;
Begin
  SetLength(aPoints, 4);

  aPoints[0].x := ToPixelX(iX);
  aPoints[0].y := ToPixelY(iY);

  If iAngle > 0 Then
  Begin
    aPoints[1].x := ToPixelX(iX+Trunc(iWidth * Cos(DegreesToRadians(360 - iAngle))));
    aPoints[1].y := ToPixelY(iY+Trunc(iWidth * Sin(DegreesToRadians(360 - iAngle))));
    aPoints[2].x := ToPixelX(iX+Trunc(iWidth * Cos(DegreesToRadians(360 - iAngle))) - Trunc(iHeight * Sin(DegreesToRadians(360 - iAngle))));
    aPoints[2].y := ToPixelY(iY+Trunc(iWidth * Sin(DegreesToRadians(360 - iAngle))) + Trunc(iHeight * Cos(DegreesToRadians(360 - iAngle))));
    aPoints[3].x := ToPixelX(iX-Trunc(iHeight * Sin(DegreesToRadians(360 - iAngle))));
    aPoints[3].y := ToPixelY(iY+Trunc(iHeight * Cos(DegreesToRadians(360 - iAngle))));
  End
  Else
  Begin
    aPoints[1].x := ToPixelX(iX + iWidth);
    aPoints[1].y := ToPixelY(iY);
    aPoints[2].x := ToPixelX(iX + iWidth);
    aPoints[2].y := ToPixelY(iY + iHeight);
    aPoints[3].x := ToPixelX(iX);
    aPoints[3].y := ToPixelY(iY + iHeight);
  End;

  Polygon(aPoints);
End;


Function TFslPrinterCanvas.TextHeight(Const sText: String; iAngle: TFslGraphicAngle): TFslGraphicMetre;
Begin
  Result := FromPixel(TextHeightPixels(sText, iAngle));
End;


Function TFslPrinterCanvas.TextHeightPixels(Const sText: String; iAngle: TFslGraphicAngle): Integer;
Var
  aSize : TSize;
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;
  FFont.TextRotation := iAngle;
  aFont := SelectFont;

  If Not GetTextExtentPoint32(Handle, PChar(sText), Length(sText), aSize) Then
    RaiseError('SelectFont', ErrorAsString);

  Result := aSize.cy;
  RestoreHandle(aFont);
End;


Function TFslPrinterCanvas.TextWidth(Const sText: String; iAngle: TFslGraphicAngle): TFslGraphicMetre;
Begin
  Result := FromPixel(TextWidthPixels(sText, iAngle));
End;


Function TFslPrinterCanvas.TextWidthPixels(Const sText: String; iAngle: TFslGraphicAngle): Integer;
Var
  aSize : TSize;
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;

  FFont.TextRotation := iAngle;

  aFont := SelectFont;
  Try
    If Not GetTextExtentPoint32(Handle, PChar(sText), Length(sText), aSize) Then
      RaiseError('SelectFont', ErrorAsString);

    Result := aSize.cx;
  Finally
    RestoreHandle(aFont);
  End;
End;


Procedure TFslPrinterCanvas.TextOutPixels(iX, iY: Integer; Const sText: String);
Begin
  TextOutPixels(clTransparent, iX, iY, sText);
End;


Procedure TFslPrinterCanvas.TextOutPixels(aBackground : TColour; iX, iY: Integer; Const sText: String);
Var
  aPen : THandle;
  aBrush : THandle;
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;

  If aBackground = clTransparent Then
    SetBkMode(FCanvas.Handle, clTransparent)
  Else
  Begin
    SetBkMode(FCanvas.Handle, OPAQUE);
    SetBkColor(FCanvas.Handle, aBackground);
  End;

  FFont.TextRotation := 0;

  aPen := SelectPen;
  aBrush := SelectBrush;
  aFont := SelectFont;
  Try
    Windows.ExtTextOut(Handle, iX - OffsetPixelsX, iY - OffsetPixelsY, 0, Nil, PChar(sText), Length(sText), Nil);
  Finally
    RestoreHandle(aFont);
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.TextOut(iX, iY: TFslGraphicMetre; Const sText: String; iAngle : TFslGraphicAngle);
Var
  aPen : THandle;
  aBrush : THandle;
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;

  FFont.TextRotation := iAngle;

  aPen := SelectPen;
  aBrush := SelectBrush;
  aFont := SelectFont;
  Try
    Windows.ExtTextOut(Handle, ToPixelX(iX), ToPixelY(iY), 0, Nil, PChar(sText), Length(sText), Nil);
  Finally
    RestoreHandle(aFont);
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.TextRect(Const aRect : TRect; iX, iY : TFslGraphicMetre; Const sText : String; iAngle : TFslGraphicAngle);
Var
  aPen : THandle;
  aBrush : THandle;
  aFont : THandle;
  lRect : TRect;
Begin
  FCanvas.ResetMetrics;

  FFont.TextRotation := iAngle;

  aPen := SelectPen;
  aBrush := SelectBrush;
  aFont := SelectFont;
  Try
    lRect.Top := ToPixelY(aRect.Top);
    lRect.Left := ToPixelX(aRect.Left);
    lRect.Bottom := ToPixelY(aRect.Bottom);
    lRect.Right := ToPixelX(aRect.Right);

    Windows.ExtTextOut(Handle, ToPixelX(iX), ToPixelY(iY), ETO_CLIPPED, @lRect, PChar(sText), Length(sText), Nil);
  Finally
    RestoreHandle(aFont);
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.BoxRounded(iX, iY, iWidth, iHeight, iRadius : TFslGraphicMetre);
Var
  aPen : THandle;
  aBrush : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  aBrush := SelectBrush;
  Try
    Windows.RoundRect(Handle, ToPixelX(iX), ToPixelY(iY), ToPixelX(iX + iWidth), ToPixelY(iY + iHeight), ToPixel(iRadius), ToPixel(iRadius));
  Finally
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.BoxRoundedPixels(iX, iY, iWidth, iHeight, iRadius : TFslGraphicMetre);
Var
  aPen : THandle;
  aBrush : THandle;
Begin
  FCanvas.ResetMetrics;

  aPen := SelectPen;
  aBrush := SelectBrush;
  Try
    Windows.RoundRect(Handle, iX - OffsetPixelsX, iY - OffsetPixelsY, iX -OffsetPixelsX + iWidth, iY - OffsetPixelsY + iHeight, iRadius, iRadius);
  Finally
    RestoreHandle(aBrush);
    RestoreHandle(aPen);
  End;
End;


Procedure TFslPrinterCanvas.PrintBitmap(oBitmap: TBitmap; iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle: TFslGraphicAngle);
Begin
  Assert(CheckCondition(iAngle = 0, 'PrintBitmap', 'Cannot print a bitmap with a rotation angle.'));
  PrintBitmapPixels(oBitmap, ToPixelX(iX), ToPixelY(iY), ToPixelWidth(iWidth), ToPixelHeight(iHeight));
End;


Procedure TFslPrinterCanvas.PrintBitmapPixels(oBitmap: TBitmap; iX, iY, iWidth, iHeight: Integer);
Var
  oGpGraphics : TGpGraphics;
  oGpBitmap : TGpBitmap;
Begin
  oGpGraphics := TGpGraphics.Create(Handle);
  oGpBitmap := TGpBitmap.Create(oBitmap.Handle, oBitmap.Palette);
  Try
    RaiseGdiPlusStatusException('PrintBitmapPixels', oGpGraphics, 'oGraphics');
    RaiseGdiPlusStatusException('PrintBitmapPixels', oGpBitmap, 'oGpBitmap');

    oGpGraphics.SetInterpolationMode(InterpolationModeHighQualityBicubic);
    oGpGraphics.SetPageUnit(UnitPixel);

    oGpGraphics.DrawImage(oGpBitmap, MakeRectF(iX, iY, iWidth, iHeight), 0, 0, oGpBitmap.GetWidth, oGpBitmap.GetHeight, UnitPixel);
  Finally
    oGpGraphics.Free;
    oGpBitmap.Free;
  End;
End;


Procedure TFslPrinterCanvas.PrintImage(oImage : TFslGraphic; iX, iY, iWidth, iHeight: TFslGraphicMetre; iAngle: TFslGraphicAngle);
Var
  oBitmap : TBitmap;
Begin
  Assert(Invariants('PrintImage', oImage, TFslGraphic, 'oImage'));

  FCanvas.ResetMetrics;

  If oImage Is TFslBitmapGraphic Then
  Begin
    PrintBitmap(TFslBitmapGraphic(oImage).Handle, iX, iY, iWidth, iHeight, iAngle)
  End
  Else If oImage Is TFslVCLGraphic Then
  Begin
    If TFslVCLGraphic(oImage).Handle Is TBitmap Then
    Begin
      oBitmap := TBitmap(TFslVCLGraphic(oImage).Handle);

      oBitmap.Canvas.Lock;
      Try
        PrintBitmap(oBitmap, iX, iY, iWidth, iHeight, iAngle);
      Finally
        oBitmap.Canvas.Unlock;
      End;
    End
    Else
    Begin
      oBitmap := TBitmap.Create;
      Try
        oBitmap.Canvas.Lock;
        Try
          oBitmap.Assign(TFslVCLGraphic(oImage).Handle);

          PrintBitmap(oBitmap, iX, iY, iWidth, iHeight, iAngle);
        Finally
          oBitmap.Canvas.Unlock;
        End;
      Finally
        oBitmap.Free;
      End;
    End;
  End;
End;


Procedure TFslPrinterCanvas.PrintImagePixels(oImage : TFslGraphic; iX, iY, iWidth, iHeight: Integer);
Var
  oBitmap : TBitmap;
  aRect : TRect;
Begin
  Assert(Invariants('PrintImagePixels', oImage, TFslGraphic, 'oImage'));

  FCanvas.ResetMetrics;

  If oImage Is TFslBitmapGraphic Then
  Begin
    PrintBitmapPixels(TFslBitmapGraphic(oImage).Handle, iX - OffsetPixelsX, iY - OffsetPixelsY, iWidth, iHeight)
  End
  Else If oImage Is TFslVCLGraphic Then
  Begin
    If TFslVCLGraphic(oImage).Handle Is TBitmap Then
    Begin
      oBitmap := TBitmap(TFslVCLGraphic(oImage).Handle);

      oBitmap.Canvas.Lock;
      Try
        PrintBitmapPixels(oBitmap, iX - OffsetPixelsX, iY -  - OffsetPixelsY, iWidth, iHeight);
      Finally
        oBitmap.Canvas.Unlock;
      End;
    End
    Else
    Begin
      oBitmap := TBitmap.Create;
      Try
        oBitmap.Canvas.Lock;
        Try
          oBitmap.Assign(TFslVCLGraphic(oImage).Handle);

          PrintBitmapPixels(oBitmap, iX - OffsetPixelsX, iY - OffsetPixelsY, iWidth, iHeight);
        Finally
          oBitmap.Canvas.Unlock;
        End;
      Finally
        oBitmap.Free;
      End;
    End;
  End
  Else
  Begin
    aRect.Left := iX;
    aRect.Top := iY;
    aRect.Right := iX + iWidth;
    aRect.Bottom := iY + iHeight;
    oImage.StretchDraw(FCanvas, aRect);
  End;
End;


Procedure TFslPrinterCanvas.LastWindowsError(Const sMethod: String);
Begin
  RaiseError(sMethod, ErrorAsString);
End;


Function TFslPrinterCanvas.ToPixelX(iValue: TFslGraphicMetre): Integer;
Begin
  Result := Round((iValue - FOffsetX) * FPixelsPerGraphicMetreX);
End;


Function TFslPrinterCanvas.ToPixelY(iValue: TFslGraphicMetre): Integer;
Begin
  Result := Round((iValue - FOffsetY) * FPixelsPerGraphicMetreY);
End;

Function TFslPrinterCanvas.ToPixelWidth(iValue: TFslGraphicMetre): Integer;
Begin
  Result := Round((iValue) * FPixelsPerGraphicMetreX);
End;


Function TFslPrinterCanvas.ToPixelHeight(iValue: TFslGraphicMetre): Integer;
Begin
  Result := Round((iValue) * FPixelsPerGraphicMetreY);
End;


Function TFslPrinterCanvas.TextExtentPixels(Const sText : String; iAngle: TFslGraphicAngle) : TSize;
Var
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;
  FFont.TextRotation := iAngle;
  aFont := SelectFont;

  If Not GetTextExtentPoint32(Handle, PChar(sText), Length(sText), Result) Then
    RaiseError('TextExtentPixels', ErrorAsString);

  RestoreHandle(aFont);
End;


Function TFslPrinterCanvas.GetTextMetrics : TTextMetric;
Var
  aFont : THandle;
Begin
  FCanvas.ResetMetrics;
  FFont.TextRotation := 0;
  aFont := SelectFont;

  If Not Windows.GetTextMetrics(Handle, Result) Then
    RaiseError('GetTextMetrics', ErrorAsString);
  RestoreHandle(aFont);
End;

Procedure TFslPrinterCanvas.GetTextExtents(Const sText : String; Var aSize : TSize; Const Offsets : PFslPrinterIntegers);
Var
  aFont : THandle;
  iMax : Integer;
Begin
  FCanvas.ResetMetrics;
  FFont.TextRotation := 0;
  aFont := SelectFont;

  If Not GetTextExtentExPoint(Handle, pchar(sText), Length(sText), 1000000, @iMax, @Offsets[0], aSize) Then
    RaiseError('GetTextMetrics', ErrorAsString);

  RestoreHandle(aFont);
End;


Procedure TFslPrinterCanvas.Clip(iLeft, iTop, iRight, iBottom : Integer);
Begin
  Assert(CheckCondition(FClip = 0, 'Clip', 'Unable to nest calls to clip'));

  FClip := CreateRectRgn(iLeft - OffsetPixelsX, iTop - OffsetPixelsY, iRight - OffsetPixelsX, iBottom - OffsetPixelsY);

  SelectClipRgn(Handle, FClip);
End;


Procedure TFslPrinterCanvas.UnClip;
Begin
  If FClip <> 0 Then
  Begin
    SelectClipRgn(Handle, 0);
    DeleteObject(FClip);
    FClip := 0;
  End;
End;


function TFslPrinterCanvas.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPen.sizeInBytes);
  inc(result, FBrush.sizeInBytes);
  inc(result, FFont.sizeInBytes);
end;

Function TFslPrinterDelphiCanvas.GetOwnerCanvas: TFslPrinterCanvas;
Begin
  Result := FOwner;
End;


Procedure TFslPrinterDelphiCanvas.SetOwnerCanvas(Const Value: TFslPrinterCanvas);
Begin
  FAdjustFont := True;
  FOwner := Value;

  If Assigned(Value) Then
  Begin
    FOrigMapMode := GetMapMode(FOwner.Handle);
    GetWindowExtEx(FOwner.Handle, FOrigWin);
    GetViewPortExtEx(FOwner.Handle, FOrigView);
    GetWindowOrgEx(FOwner.Handle, Windows.TPoint(FOrigOrg));
    FOrigAlign := GetTextAlign(FOwner.Handle);
  End;
End;


Procedure TFslPrinterCanvas.PrintImageWithMask(oImage, oMask: TFslBitmapGraphic; aCopyMode : TCopyMode; iX, iY, iWidth, iHeight: TFslGraphicMetre);
Const
  ROP_DstCopy = $00AA0029;
Begin
  Win32Check(MaskBlt(Handle, iX, iY, oImage.Width, oImage.Height,
             oImage.Handle.Canvas.Handle, 0, 0,
             oMask.Handle.Handle, 0, 0, MakeRop4(ROP_DstCopy, aCopyMode)));
{  Win32Check(MaskBlt(FImage.Canvas.Handle, iLeft, iTop, iRight - iLeft, iBottom - iTop,
              TFslBitmapGraphic(oImage).Handle.Canvas.Handle, 0, 0,
              oMask.Handle, 0, 0,  MakeRop4(ROP_DstCopy, aCopyMode)));
}
End;


Function TFslPrinterCanvas.DoesColour: Boolean;
Begin
  If GetDeviceCaps(Handle, RASTERCAPS) And RC_PALETTE > 1 Then
    Result := GetDeviceCaps(Handle, COLORRES) > 2
  Else
    Result := (GetDeviceCaps(Handle, BITSPIXEL) > 1) Or (GetDeviceCaps(Handle, PLANES) > 1)
             Or (GetDeviceCaps(Handle, NUMCOLORS) = -1) Or (GetDeviceCaps(Handle, NUMCOLORS) > 2);
End;

function TFslPrinterCanvas.GetHandle: TFslGraphicHandle;
begin
  result := inherited Handle;
end;


Var
  GSynchroniseLock : TFslLock;


Class Function TFslSynchronisedPrinterAPI.SynchroniseLock: TFslLock;
Begin
  Result := GSynchroniseLock;
End;


Class Function TFslSynchronisedPrinterAPI.AdvancedDocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; pDevModeOutput, pDevModeInput: PDeviceMode): LongInt;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.AdvancedDocumentProperties(hWnd, hPrinter, pDeviceName, pDevModeOutput, pDevModeInput);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.CreateIC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Windows.CreateIC(lpszDriver, lpszDevice, lpszOutput, lpdvmInit);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.DeleteIC(aHDC: HDC): Boolean;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Windows.DeleteDC(aHDC);
  Finally
    SynchroniseLock.Unlock;
  End;
End;

{$IFNDEF VER130}
Class Function TFslSynchronisedPrinterAPI.DocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; fMode: DWORD): LongInt;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.DocumentProperties(hWnd, hPrinter, pDeviceName, nil, nil, fMode);
    if result < 0 then
      RaiseLastOSError;
  Finally
    SynchroniseLock.Unlock;
  End;
End;
{$ENDIF}

Class Function TFslSynchronisedPrinterAPI.DocumentProperties(hWnd: HWND; hPrinter: THandle; pDeviceName: PChar; Const pDevModeOutput: TDeviceMode; Var pDevModeInput: TDeviceMode; fMode: DWORD): LongInt;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.DocumentProperties(hWnd, hPrinter, pDeviceName, pDevModeOutput, pDevModeInput, fMode);
    if result < 0 then
{$IFDEF VER130}
      RaiseLastWin32Error;
{$ELSE}
      RaiseLastOSError;
{$ENDIF}
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.EnumFonts(DC: HDC; lpszFace: PChar; fntenmprc: TFNFontEnumProc; lpszData: PChar): Integer;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Windows.EnumFonts(DC, lpszFace, fntenmprc, lpszData);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.GetPrinter(hPrinter: THandle; Level: DWORD; pPrinter: Pointer; cbBuf: DWORD; pcbNeeded: PDWORD): BOOL;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.GetPrinter(hPrinter, Level, pPrinter, cbBuf, pcbNeeded);
  Finally
    SynchroniseLock.Unlock;
  End;
End;

Class Function TFslSynchronisedPrinterAPI.GetPrinterDriver(hPrinter: THandle; pEnvironment: PChar; Level: DWORD; pDriverInfo: Pointer; cbBuf: DWORD; var pcbNeeded: DWORD): BOOL;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.GetPrinterDriver(hPrinter, pEnvironment, Level, pDriverInfo, cbBuf, pcbNeeded);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.OpenPrinter(pPrinterName: PChar; Var phPrinter: THandle; pDefault: PPrinterDefaults): BOOL;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.OpenPrinter(pPrinterName, phPrinter, pDefault);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.ClosePrinter(hPrinter: THandle): BOOL;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.ClosePrinter(hPrinter);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.DeviceCapabilities(pDevice, pPort: PChar; fwCapability: Word; pOutput: PChar; DevMode: PDeviceMode): Integer;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.DeviceCapabilities(pDevice, pPort, fwCapability, pOutput, DevMode);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.GetDIB(Bitmap: HBITMAP; Palette: HPALETTE; Var BitmapInfo; Var Bits): Boolean;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Graphics.GetDIB(Bitmap, Palette, BitmapInfo, Bits);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Procedure TFslSynchronisedPrinterAPI.GetDIBSizes(Bitmap: HBITMAP; Var InfoHeaderSize, ImageSize: DWORD);
Begin
  SynchroniseLock.Lock;
  Try
    Graphics.GetDIBSizes(Bitmap, InfoHeaderSize, ImageSize);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.StretchDIBits(DC: HDC; DestX, DestY, DestWidth, DestHegiht, SrcX, SrcY, SrcWidth, SrcHeight: Integer; Bits: Pointer; Var BitsInfo: TBitmapInfo; Usage: UINT; Rop: DWORD): Integer;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Windows.StretchDIBits(DC, DestX, DestY, DestWidth, DestHegiht, SrcX, SrcY, SrcWidth, SrcHeight, Bits, BitsInfo, Usage, Rop);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.EnumJobs(hPrinter: THandle; FirstJob, NoJobs, Level: DWORD; pJob: Pointer; cbBuf: DWORD; Var pcbNeeded, pcReturned: DWORD): BOOL;
Begin
  SynchroniseLock.Lock;
  Try
    Result := WinSpool.EnumJobs(hPrinter, FirstJob, NoJobs, Level, pJob, cbBuf, pcbNeeded, pcReturned);
  Finally
    SynchroniseLock.Unlock;
  End;
End;


Class Function TFslSynchronisedPrinterAPI.CreateDC(lpszDriver, lpszDevice, lpszOutput: PChar; lpdvmInit: PDeviceMode): HDC;
Begin
  SynchroniseLock.Lock;
  Try
    Result := Windows.CreateDC(lpszDriver, lpszDevice, lpszOutput, lpdvmInit);
  Finally
    SynchroniseLock.Unlock;
  End;
End;

Function PrinterCapabilitiesToString(aCap : TFslPrinterCapabilitySet) : String;
Var
  aLoop : TFslPrinterCapability;
Begin
  Result := '';
  For aLoop := Low(TFslPrinterCapability) To High(TFslPrinterCapability) Do
  Begin
    If aLoop In aCap Then
      StringAppend(Result, ADVPRINTERCAPABILITY_NAMES[aLoop], ',');
  End;

  Result := '[' + Result + ']';
End;






Constructor TFslPrinterPreviewJob.Create;
Begin
  Inherited;

  FPages := TFslPrinterPreviewPageList.Create;
  FActivePage := Nil;
  FMeta := Nil;
End;


Destructor TFslPrinterPreviewJob.Destroy;
Begin
  FMeta.Free;
  FActivePage.Free;
  FPages.Free;

  Inherited;
End;


Procedure TFslPrinterPreviewJob.InternalStart;
Begin
  FPages.Clear;

  FHandle := CreateDC(Nil, PChar(Settings.Definition.Name), Nil, Settings.Device);
  If FHandle = 0 Then
    RaiseError('Start', StringFormat('Unable to Open Printer %s: %s', [Settings.Definition.Name, ErrorAsString]));

  // for preview support
  FOffsetX := GetDeviceCaps(FHandle, PhysicalOffsetX);
  FOffsetY := GetDeviceCaps(FHandle, PhysicalOffsetY);

  InternalNewPage;
End;


Procedure TFslPrinterPreviewJob.InternalNewPage;
Begin
  ActivePage := TFslPrinterPreviewPage.Create;
  ActivePage.Handle.MMWidth := Settings.ActualWidth * 10;
  ActivePage.Handle.MMHeight := Settings.ActualHeight * 10;

  FPages.Add(ActivePage.Link);

  FMeta.Free;
  FMeta := Nil;
  FMeta := TMetafileCanvas.Create(ActivePage.Handle, FHandle);

  RecreateCanvas;

  Canvas.AttachToPrinter(FMeta.Handle);
  Canvas.Height := Settings.ActualHeight;
  Canvas.Width := Settings.ActualWidth;
End;


Procedure TFslPrinterPreviewJob.InternalAbort;
Begin
  ActivePage := Nil;

  FPages.Clear;
End;


Procedure TFslPrinterPreviewJob.InternalFinish;
Begin
  DeleteDC(FHandle);
  ActivePage := Nil;
  FMeta.Free;
  FMeta := Nil;
End;


Function TFslPrinterPreviewJob.Link: TFslPrinterPreviewJob;
Begin
  Result := TFslPrinterPreviewJob(Inherited Link);
End;


Function TFslPrinterPreviewJob.GetActivePage: TFslPrinterPreviewPage;
Begin
  Assert(Invariants('GetActivePage', FActivePage, TFslPrinterPreviewPage, 'FActivePage'));

  Result := FActivePage;
End;


Procedure TFslPrinterPreviewJob.SetActivePage(Const Value: TFslPrinterPreviewPage);
Begin
  FActivePage.Free;
  FActivePage := Value;
End;


Function TFslPrinterPreviewJob.SettingsClass: TFslJobSettingsClass;
Begin
  Result := TFslPrinterSettings;
End;


Function TFslPrinterPreviewJob.GetSettings: TFslPrinterSettings;
Begin
  Result := TFslPrinterSettings(Inherited Settings);
End;


Procedure TFslPrinterPreviewJob.SetTitle(Const sValue : String);
Begin
  If Started Then
    RaiseError('SetTitle', 'Cannot change the set up of a print job while printing is in progress');

  If Not Opened Then
    RaiseError('SetTitle', 'Cannot change the set up of the print job because the printer has not been opened');

  Inherited;
End;

function TFslPrinterPreviewJob.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FPages.sizeInBytes);
  inc(result, FActivePage.sizeInBytes);
end;

Function TFslPrinterPreviewPageList.GetPage(iIndex: Integer): TFslPrinterPreviewPage;
Begin
  Result := TFslPrinterPreviewPage(ObjectByIndex[iIndex]);
End;


Function TFslPrinterPreviewPageList.ItemClass: TFslObjectClass;
Begin
  Result := TFslPrinterPreviewPage;
End;


Constructor TFslPrinterManager.Create;
Begin
  Inherited;

  FDefinitionList := TFslPrinterDefinitionList.Create;
  FDefaultDefinition := Nil;
End;


Destructor TFslPrinterManager.Destroy;
Begin
  FDefaultDefinition := Nil;
  FDefinitionList.Free;

  Inherited;
End;


Function TFslPrinterManager.ExtractNextString(Var Str: PChar): PChar;
Var
  P: PChar;
Begin
  Result := Str;

  If Str = Nil Then
    Exit;

  P := Str;
  While P^ = ' ' Do
    Inc(P);

  Result := P;

  While (P^ <> #0) And (P^ <> ',') Do
    Inc(P);

  If P^ = ',' Then
  Begin
    P^ := #0;
    Inc(P);
  End;

  Str := P;
End;


Procedure TFslPrinterManager.Open;
Var
  iByteCnt, iStructCnt: DWORD;
  aDefaultPrinter: Array[0..79] Of Char;
  aPrinterInfo: PPrinterInfo5;
  pCurrent : PChar;
  pLineCur : PChar;
  pPort : PChar;
  pBuffer : PAnsiChar;
  pPrinterInfo : PAnsiChar;
  iFlags : Cardinal;
  iCount : Cardinal;
  iNumInfo : Cardinal;
  iLoop : Integer;
  iLevel : Byte;
  sDefaultPrinterName : String;
  oDefinition : TFslPrinterDefinition;
Begin
  FDefinitionList.Clear;

  If SystemIsWindowsNT Then
  Begin
    iFlags := PRINTER_ENUM_CONNECTIONS Or PRINTER_ENUM_LOCAL;
    iLevel := 4;
  End
  Else
  Begin
    iFlags := PRINTER_ENUM_LOCAL;
    iLevel := 5;
  End;

  iCount := 0;
  EnumPrinters(iFlags, Nil, iLevel, Nil, 0, iCount, iNumInfo);

  If iCount <> 0 Then
  Begin
    iByteCnt := 0;
    iStructCnt := 0;

    If Not EnumPrinters(PRINTER_ENUM_DEFAULT, Nil, 5, Nil, 0, iByteCnt, iStructCnt) And (GetLastError <> ERROR_INSUFFICIENT_BUFFER) Then
      sDefaultPrinterName := ''
    Else
    Begin
      MemoryCreate(aPrinterInfo, iByteCnt);
      Try
        ZeroMemory(aPrinterInfo, iByteCnt);
        EnumPrinters(PRINTER_ENUM_DEFAULT, Nil, 5, aPrinterInfo, iByteCnt, iByteCnt, iStructCnt);

        If iStructCnt > 0 Then
          sDefaultPrinterName := aPrinterInfo.pPrinterName
        Else
        Begin
          GetProfileString('windows', 'device', '', aDefaultPrinter, SizeOf(aDefaultPrinter) - 1);

          pCurrent := aDefaultPrinter;

          sDefaultPrinterName := ExtractNextString(pCurrent);
        End;
      Finally
        MemoryDestroy(aPrinterInfo, iByteCnt);
      End;
    End;

    MemoryCreate(pBuffer, iCount);
    Try
      If EnumPrinters(iFlags, Nil, iLevel, PByte(pBuffer), iCount, iCount, iNumInfo) Then
      Begin
        pPrinterInfo := pBuffer;
        For iLoop := 0 To iNumInfo - 1 Do
        Begin
          If iLevel = 4 Then
          Begin
            oDefinition := TFslPrinterDefinition.Create;
            Try
              oDefinition.Name := PPrinterInfo4(pPrinterInfo)^.pPrinterName;
              oDefinition.Driver := '';
              oDefinition.Port := '';
              oDefinition.IsDefault := StringEquals(oDefinition.Name, sDefaultPrinterName);

              If oDefinition.IsDefault Then
                FDefaultDefinition := oDefinition;

              FDefinitionList.Add(oDefinition.Link);
            Finally
              oDefinition.Free;
            End;

            Inc(pPrinterInfo, SizeOf(TPrinterInfo4));
          End
          Else
          Begin
            pLineCur := PPrinterInfo5(pPrinterInfo)^.pPortName;
            pPort := ExtractNextString(pLineCur);

            While pPort^ <> #0 Do
            Begin
              oDefinition := TFslPrinterDefinition.Create;
              Try
                oDefinition.Name := PPrinterInfo5(pPrinterInfo)^.pPrinterName ;
                oDefinition.Driver := '';
                oDefinition.Port := PPrinterInfo5(pPrinterInfo)^.pPortName;
                oDefinition.IsDefault := StringEquals(oDefinition.Name, sDefaultPrinterName);

                If oDefinition.IsDefault Then
                  FDefaultDefinition := oDefinition;

                FDefinitionList.Add(oDefinition.Link);
              Finally
                oDefinition.Free;
              End;

              pPort := ExtractNextString(pLineCur);
            End;

            Inc(pPrinterInfo, SizeOf(TPrinterInfo5));
          End;
        End;
      End;
    Finally
      MemoryDestroy(pBuffer, iCount);
    End;
  End;

  FActive := True;
End;


Procedure TFslPrinterManager.Close;
Begin
  FDefinitionList.Clear;
  FDefaultDefinition := Nil;

  FActive := False;
End;


Function TFslPrinterManager.GetDefaultDefinition: TFslPrinterDefinition;
Begin
  Result := FDefaultDefinition;
End;


Function TFslPrinterManager.HasDefaultDefinition: Boolean;
Begin
  Result := Assigned(FDefaultDefinition);
End;


Procedure TFslPrinterManager.CompilePrinterList(oPrinterList: TFslPrinterList);
Var
  iDefinitionIndex : Integer;
  oPrinter : TFslPrinter;
Begin
  For iDefinitionIndex := 0 To FDefinitionList.Count - 1 Do
  Begin
    oPrinter := TFslPrinter.Create;
    oPrinter.Definition := FDefinitionList[iDefinitionIndex].Link;
    oPrinterList.Add(oPrinter);
  End;
End;


function TFslPrinterManager.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FDefinitionList.sizeInBytes);
  inc(result, FDefaultDefinition.sizeInBytes);
end;

Function TFslMetafile.HandleClass : TGraphicClass;
Begin
  Result := TMetafile;
End;


Function TFslMetafile.HandleNew: TGraphic;
Begin
  Result := TMetafile.Create; // Because TGraphicClass.Create is not a virtual method.
End;


Function TFslMetafile.GetHandle: TMetafile;
Begin
  Result := TMetafile(Inherited Handle);
End;


Procedure TFslMetafile.SetHandle(Const Value: TMetafile);
Begin
  Inherited Handle := Value;
End;


Function TFslMetafileList.GetGraphic(iIndex: Integer): TFslMetafile;
Begin
  Result := TFslMetafile(ObjectByIndex[iIndex]);
End;


Function TFslMetafileList.ItemClass: TFslObjectClass;
Begin
  Result := TFslMetafile;
End;


Initialization
  GSynchroniseLock := TFslLock.Create;
Finalization
  GSynchroniseLock.Free;
  GSynchroniseLock := Nil;
End.
