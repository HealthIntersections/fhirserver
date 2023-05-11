{! 1 !}

unit DropPIDLSource;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Component Names: TDropPIDLSource
// Module:          DropPIDLSource
// Description:     Implements Dragging & Dropping of PIDLs
//                  FROM your application to another.
// Version:         3.7
// Date:            22-JUL-1999
// Target:          Win32, Delphi 3 - Delphi 5, C++ Builder 3, C++ Builder 4
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
// Copyright        © 1997-99 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------

interface

uses
  DropSource,
  Classes, ActiveX, ShlObj;

{$include DragDrop.inc}

type
  TDropPIDLSource = class(TDropSource)
  private
    fPIDLs: TStrings; //NOTE: contains folder PIDL as well as file PIDLs
    fMappedNames: TStrings;
    function GetFilename(index: integer): string; //used internally
    procedure SetMappedNames(names: TStrings);
  protected
    function DoGetData(const FormatEtcIn: TFormatEtc;
      OUT Medium: TStgMedium):HRESULT; Override;
    function CutOrCopyToClipboard: boolean; Override;
  public
    constructor Create(aOwner: TComponent); Override;
    destructor Destroy; Override;
    procedure CopyFolderPidlToList(pidl: PItemIDList);
    procedure CopyFilePidlToList(pidl: PItemIDList);
    property MappedNames: TStrings read fMappedNames write SetMappedNames;
  end;

procedure Register;

//Exposed as also used by DropPIDLTarget...
function PidlToString(pidl: PItemIDList): String;
function JoinPidlStrings(pidl1,pidl2: string): String;

implementation

uses
  Windows, 
  SysUtils,
  ClipBrd;

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropPIDLSource]);
end;

// -----------------------------------------------------------------------------
//      Miscellaneous Functions...
// -----------------------------------------------------------------------------

function GetSizeOfPidl(pidl: PItemIDList): integer;
var
  i: integer;
begin
  result := SizeOf(Word);
  repeat
    i := pSHItemID(pidl)^.cb;
    inc(result,i);
    inc(longint(pidl),i);
  until i = 0;
end;
// -----------------------------------------------------------------------------

function PidlToString(pidl: PItemIDList): String;
var
  PidlLength: integer;
begin
  PidlLength := GetSizeOfPidl(pidl);
  setlength(result,PidlLength);
  Move(pidl^,pchar(result)^,PidlLength);
end;
// -----------------------------------------------------------------------------

function JoinPidlStrings(pidl1,pidl2: string): String;
var
  PidlLength: integer;
begin
  if Length(pidl1) <= 2 then PidlLength := 0
  else PidlLength := Length(pidl1)-2;
  setlength(result,PidlLength+length(pidl2));
  if PidlLength > 0 then Move(pidl1[1],result[1],PidlLength);
  Move(pidl2[1],result[PidlLength+1],length(pidl2));
end;

// -----------------------------------------------------------------------------
//      TDropPIDLSource
// -----------------------------------------------------------------------------
 
constructor TDropPIDLSource.Create(aOwner: TComponent);
begin
  inherited Create(aOwner);
  fPIDLs := TStringList.create;
  fMappedNames := TStringList.Create;
  AddFormatEtc(CF_HDROP, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_IDLIST, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_PREFERREDDROPEFFECT, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILENAMEMAP, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
  AddFormatEtc(CF_FILENAMEMAPW, NIL, DVASPECT_CONTENT, -1, TYMED_HGLOBAL);
end;
// -----------------------------------------------------------------------------

destructor TDropPIDLSource.Destroy;
begin
  fPIDLs.free;
  fMappedNames.free;
  inherited Destroy;
end;
// -----------------------------------------------------------------------------

//this function is used internally by DoGetData()...
function TDropPIDLSource.GetFilename(index: integer): string;
var
  PidlStr: string;
  buff: array [0..MAX_PATH] of char;
begin
  if (index < 1) or (index >= fPIDLs.count) then result := ''
  else
  begin
    PidlStr := JoinPidlStrings(fPIDLs[0], fPIDLs[index]);
    SHGetPathFromIDList(PItemIDList(pChar(PidlStr)),buff);
    result := buff;
  end;
end;
// ----------------------------------------------------------------------------- 

//Note: Once the PIDL has been copied into the list it can be 'freed'.
procedure TDropPIDLSource.CopyFolderPidlToList(pidl: PItemIDList);
begin
  fPIDLs.clear;
  fMappedNames.clear;
  fPIDLs.add(PidlToString(pidl));
end;
// ----------------------------------------------------------------------------- 

//Note: Once the PIDL has been copied into the list it can be 'freed'.
procedure TDropPIDLSource.CopyFilePidlToList(pidl: PItemIDList);
begin
  if fPIDLs.count < 1 then exit; //no folder pidl has been added!
  fPIDLs.add(PidlToString(pidl));
end;
// ----------------------------------------------------------------------------- 

procedure TDropPIDLSource.SetMappedNames(names: TStrings);
begin
  fMappedNames.assign(names);
end;
// -----------------------------------------------------------------------------

function TDropPIDLSource.CutOrCopyToClipboard: boolean;
var
  FormatEtcIn: TFormatEtc;
  Medium: TStgMedium;
begin
  FormatEtcIn.cfFormat := CF_IDLIST;
  FormatEtcIn.dwAspect := DVASPECT_CONTENT;
  FormatEtcIn.tymed := TYMED_HGLOBAL;
  if (fPIDLs.count < 2) then result := false
  else if GetData(formatetcIn,Medium) = S_OK then
  begin
    Clipboard.SetAsHandle(CF_IDLIST,Medium.hGlobal);
    result := true;
  end else result := false;
end;
// ----------------------------------------------------------------------------- 

function TDropPIDLSource.DoGetData(const FormatEtcIn: TFormatEtc;
  OUT Medium: TStgMedium):HRESULT;
var
  i, MemSpace, CidaSize, Offset, StrLength: integer;
  pCIDA: PIDA;
  pInt: ^UINT;
  pOffset: PChar;
  DropEffect: ^DWORD;
  dropfiles: pDropFiles;
  fFiles: string;
  pFileList: PChar;
  pFileW: PWideChar;
begin
  Medium.tymed := 0;
  Medium.UnkForRelease := NIL;
  Medium.hGlobal := 0;
  if fPIDLs.count < 2 then result := E_UNEXPECTED
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_HDROP) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    fFiles := '';
    for i := 1 to fPIDLs.Count-1 do
      AppendStr(fFiles,GetFilename(i)+#0);
    AppendStr(fFiles,#0);

    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT,
        SizeOf(TDropFiles)+length(fFiles));
    if (Medium.hGlobal = 0) then
      result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      dropfiles := GlobalLock(Medium.hGlobal);
      try
        dropfiles^.pfiles := SizeOf(TDropFiles);
        dropfiles^.fwide := False;
        longint(pFileList) := longint(dropfiles)+SizeOf(TDropFiles);
        move(fFiles[1],pFileList^,length(fFiles));
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILENAMEMAP) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) and
    //make sure there is a Mapped Name for each filename...
    (fMappedNames.Count = fPidls.Count-1) then
  begin
    strlength := 0;
    for i := 0 to fMappedNames.Count-1 do
      Inc(strlength, Length(fMappedNames[i])+1);

    Medium.hGlobal :=
      GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, strlength+1);
    if (Medium.hGlobal = 0) then
      result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      pFileList := GlobalLock(Medium.hGlobal);
      try
        for i := 0 to fMappedNames.Count-1 do
        begin
          StrPCopy(pFileList,fMappedNames[i]);
          Inc(pFileList, Length(fMappedNames[i])+1);
        end;
        pFileList^ := #0;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_FILENAMEMAPW) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) and
    //make sure there is a Mapped Name for each filename...
    (fMappedNames.Count = fPidls.Count-1) then
  begin
    strlength := 2;
    for i := 0 to fMappedNames.Count-1 do
      Inc(strlength, (Length(fMappedNames[i])+1)*2);

    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, strlength);
    if (Medium.hGlobal = 0) then
      result:=E_OUTOFMEMORY
    else
    begin
      Medium.tymed := TYMED_HGLOBAL;
      pFileW := GlobalLock(Medium.hGlobal);
      try
        for i := 0 to fMappedNames.Count-1 do
        begin
          StringToWideChar(fMappedNames[i],
              pFileW, (length(fMappedNames[i])+1)*2);
          Inc(pFileW, Length(fMappedNames[i])+1);
        end;
      pFileW^ := #0;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_IDLIST) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    CidaSize := sizeof(UINT)*(1+fPIDLs.Count); //size of CIDA structure
    MemSpace := CidaSize;
    for i := 0 to fPIDLs.Count-1 do
      Inc(MemSpace, Length(fPIDLs[i]));
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, MemSpace);

    if (Medium.hGlobal = 0) then
      result := E_OUTOFMEMORY
    else
    begin
      medium.tymed := TYMED_HGLOBAL;
      pCIDA := PIDA(GlobalLock(Medium.hGlobal));
      try
        pCIDA^.cidl := fPIDLs.count-1; //don't count folder
        pInt := @(pCIDA^.aoffset); //points to aoffset[0];
        pOffset := pChar(pCIDA);
        //move pOffset to point to the Folder PIDL location
        inc(pOffset,CidaSize);
        offset := CidaSize;
        for i := 0 to fPIDLs.Count-1 do
        begin
          pInt^ := offset; //store 'offset' into aoffset[i]
          //copy the PIDL into pOffset
          Move(pointer(fPIDLs[i])^,pOffset^,length(fPIDLs[i]));
          //increase 'offset' by the size of the last pidl
          inc(offset,length(fPIDLs[i]));
          inc(pInt); //increment the aoffset pointer
          //move pOffset ready for the next PIDL
          inc(pOffset,length(fPIDLs[i]));
        end;
      finally
        GlobalUnlock(Medium.hGlobal);
      end;
     result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else if (FormatEtcIn.cfFormat = CF_PREFERREDDROPEFFECT) and
    (FormatEtcIn.dwAspect = DVASPECT_CONTENT) and
    (FormatEtcIn.tymed and TYMED_HGLOBAL <> 0) then
  begin
    Medium.tymed := TYMED_HGLOBAL;
    Medium.hGlobal := GlobalAlloc(GMEM_SHARE or GMEM_ZEROINIT, SizeOf(DWORD));
    if Medium.hGlobal = 0 then
      result:=E_OUTOFMEMORY
    else
    begin
      DropEffect := GlobalLock(Medium.hGlobal);
      try
        DropEffect^ := DWORD(FeedbackEffect);
      finally
        GlobalUnLock(Medium.hGlobal);
      end;
      result := S_OK;
    end;
  end
  //--------------------------------------------------------------------------
  else
    result := DV_E_FORMATETC;
end;
// -----------------------------------------------------------------------------
// -----------------------------------------------------------------------------

end.
