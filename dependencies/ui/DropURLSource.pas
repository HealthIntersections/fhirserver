{! 1 !}

unit DropURLSource;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Component Names: TDropURLSource
// Module:          DropURLSource
// Description:     Implements Dragging & Dropping of URLs
//                  FROM your application to another.
// Version:         3.7
// Date:            22-APR-1999
// Target:          Win32, Delphi 3 - Delphi 5, C++ Builder 3, C++ Builder 4
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
// Copyright        © 1997-99 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------

interface

uses
  DropSource,
  Classes, ActiveX;

{$include DragDrop.inc}

type
  TDropURLSource = class(TDropSource)
  private
    fURL: String;
    fTitle: String;
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT; Override;
  public
    constructor Create(aOwner: TComponent); Override;
    function CutOrCopyToClipboard: boolean; Override;
  published
    property URL: String Read fURL Write fURL;
    property Title: String Read fTitle Write fTitle;
  end;

procedure Register;

implementation

uses
  Windows,
  SysUtils,
  ClipBrd,
  ShlObj;

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropURLSource]);
end;
// -----------------------------------------------------------------------------

function ConvertURLToFilename(url: string): string;
const
  Invalids = '\/:?*<>,|''"';
var
  i: integer;
begin
  if lowercase(copy(url,1,7)) = 'http://' then
    url := copy(url,8,128) // limit to 120 chars.
  else if lowercase(copy(url,1,6)) = 'ftp://' then
    url := copy(url,7,127)
  else if lowercase(copy(url,1,7)) = 'mailto:' then
    url := copy(url,8,128)
  else if lowercase(copy(url,1,5)) = 'file:' then
    url := copy(url,6,126);

  if url = '' then url := 'untitled';
  result := url;
  for i := 1 to length(result) do
    if result[i] = '/'then
    begin
      result := copy(result,1,i-1);
      break;
    end
    else if pos(result[i],Invalids) <> 0 then
      result[i] := ' ';
   appendstr(result,'.url');
end;

// -----------------------------------------------------------------------------
//      TDropURLSource
// -----------------------------------------------------------------------------

constructor TDropURLSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fURL := '';
  fTitle := '';
  DragTypes := [dtLink]; // Only dtLink allowed

  AddFormatEtc(CF_URL, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILEGROUPDESCRIPTOR, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILECONTENTS, NIL, DVASPECT_CONTENT, 0, TYMED_HGLOBAL);
  AddFormatEtc(CF_TEXT, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
end;
// ----------------------------------------------------------------------------- 

function TDropURLSource.CutOrCopyToClipboard: boolean;
var
  FormatEtcIn: TFormatEtc;
  Medium: TStgMedium;
begin
  result := false;
  FormatEtcIn.cfFormat := CF_URL;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if fURL = '' then exit;
  if GetData(formatetcIn,Medium) = S_OK then
  begin
    Clipboard.SetAsHandle(CF_URL,Medium.hGlobal);
    result := true;
  end else exit;

  //render several formats...
  FormatEtcIn.cfFormat := CF_TEXT;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if GetData(formatetcIn,Medium) = S_OK then
  begin
    Clipboard.SetAsHandle(CF_TEXT,Medium.hGlobal);
    result := true;
  end;
end;
// ----------------------------------------------------------------------------- 

function TDropURLSource.DoGetData(const FormatEtcIn: TFormatEtc; OUT Medium: TStgMedium):HRESULT;
const
  URLPrefix = '[InternetShortcut]'#10'URL=';
var
  pFGD: PFileGroupDescriptor;
  pText: PChar;
begin

  Medium.tymed := 0;
  Medium.UnkForRelease := NIL;
  Medium.hGlobal := 0;

  //--------------------------------------------------------------------------
  if ((FormatEtcIn.cfFormat = CF_URL) or (FormatEtcIn.cfFormat = CF_TEXT)) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(fURL)+1);
    if (Medium.hGlobal = 0) then
      result := E_OUTOFMEMORY
    else
    begin
      medium.tymed := TYMED_HGLOBAL;
      pText := PChar(GlobalLock(Medium.hGlobal));
      try
        StrCopy(pText, PChar(fURL));
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILECONTENTS) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, Length(URLPrefix + fURL)+1);
    if (Medium.hGlobal = 0) then
      result := E_OUTOFMEMORY
    else
    begin
      medium.tymed := TYMED_HGLOBAL;
      pText := PChar(GlobalLock(Medium.hGlobal));
      try
        StrCopy(pText, PChar(URLPrefix + fURL));
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILEGROUPDESCRIPTOR) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GHND, SizeOf(TFileGroupDescriptor));
    if (Medium.hGlobal = 0) then
    begin
      result := E_OUTOFMEMORY;
      Exit;
    end;
    medium.tymed := TYMED_HGLOBAL;
    pFGD := pointer(GlobalLock(Medium.hGlobal));
    try
      with pFGD^ do
      begin
        cItems := 1;
        fgd[0].dwFlags := FD_LINKUI;
        if title = '' then
          StrPCopy(fgd[0].cFileName,ConvertURLToFilename(fURL))
        else
          StrPCopy(fgd[0].cFileName,ConvertURLToFilename(fTitle));
      end;
    finally
      GlobalUnlock(Medium.hGlobal);
    end;
    result := S_OK;
  //--------------------------------------------------------------------------
  end else
    result := DV_E_FORMATETC;
end;
// ----------------------------------------------------------------------------- 
// ----------------------------------------------------------------------------- 

end.
