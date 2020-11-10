{! 1 !}

unit DropURLTarget;

// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Component Names: TDropURLTarget
// Module:          DropURLTarget
// Description:     Implements Dragging & Dropping of URLs
//                  TO your application from another.
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
  DropSource, DropTarget,
  Classes, ActiveX;

{$include DragDrop.inc}

type
  TDropURLTarget = class(TDropTarget)
  private
    URLFormatEtc, 
    FileContentsFormatEtc,
    FGDFormatEtc: TFormatEtc;
    fURL: String;
    fTitle: String;
  protected
    procedure ClearData; override;
    function DoGetData: boolean; override;
    function HasValidFormats: boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    property URL: String Read fURL Write fURL;
    property Title: String Read fTitle Write fTitle;
  end;

procedure Register;

implementation

uses
  Windows,
  SysUtils,
  ShlObj;

procedure Register;
begin
  RegisterComponents('DragDrop', [TDropURLTarget]);
end;
// -----------------------------------------------------------------------------

function GetURLFromFile(const Filename: string; var URL: string): boolean;
var
  URLfile    : TStringList;
  i      : integer;
  s      : string;
  p      : PChar;
begin
  Result := False;
  URLfile := TStringList.Create;
  try
    URLFile.LoadFromFile(Filename);
    i := 0;
    while (i < URLFile.Count-1) do
    begin
      if (CompareText(URLFile[i], '[InternetShortcut]') = 0) then
      begin
        inc(i);
        while (i < URLFile.Count) do
        begin
          s := URLFile[i];
          p := PChar(s);
          if (StrLIComp(p, 'URL=', length('URL=')) = 0) then
          begin
            inc(p, length('URL='));
            URL := p;
            Result := True;
            exit;
          end else
            if (p^ = '[') then
              exit;
          inc(i);
        end;
      end;
      inc(i);
    end;
  finally
    URLFile.Free;
  end;
end;

// -----------------------------------------------------------------------------
//      TDropURLTarget
// -----------------------------------------------------------------------------

constructor TDropURLTarget.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DragTypes := [dtLink]; //Only allow links.
  GetDataOnEnter := true;
  with URLFormatEtc do
  begin
    cfFormat := CF_URL;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
  with FileContentsFormatEtc do
  begin
    cfFormat := CF_FILECONTENTS;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := 0;
    tymed := TYMED_HGLOBAL;
  end;
  with FGDFormatEtc do
  begin
    cfFormat := CF_FILEGROUPDESCRIPTOR;
    ptd := nil;
    dwAspect := DVASPECT_CONTENT;
    lindex := -1;
    tymed := TYMED_HGLOBAL;
  end;
end;
// ----------------------------------------------------------------------------- 

//This demonstrates how to enumerate all DataObject formats.
function TDropURLTarget.HasValidFormats: boolean;
var
  GetNum, GotNum: longint;
  FormatEnumerator: IEnumFormatEtc;
  tmpFormatEtc: TformatEtc;
begin
  result := false;
  //Enumerate available DataObject formats
  //to see if any one of the wanted formats is available...
  if (DataObject.EnumFormatEtc(DATADIR_GET,FormatEnumerator) <> S_OK) or
     (FormatEnumerator.Reset <> S_OK) then
    exit;
  GetNum := 1; //get one at a time...
  while (FormatEnumerator.Next(GetNum, tmpFormatEtc, @GotNum) = S_OK) and
        (GetNum = GotNum) do
    with tmpFormatEtc do
      if (ptd = nil) and (dwAspect = DVASPECT_CONTENT) and
         {(lindex <> -1) or} (tymed and TYMED_HGLOBAL <> 0) and
         ((cfFormat = CF_URL) or (cfFormat = CF_FILECONTENTS) or
         (cfFormat = CF_HDROP) or (cfFormat = CF_TEXT)) then
      begin
        result := true;
        break;
      end;
end;
// ----------------------------------------------------------------------------- 

procedure TDropURLTarget.ClearData;
begin
  fURL := '';
end;
// ----------------------------------------------------------------------------- 

function TDropURLTarget.DoGetData: boolean;
var
  medium: TStgMedium;
  cText: pchar;
  tmpFiles: TStringList;
  pFGD: PFileGroupDescriptor;
begin
  fURL := '';
  fTitle := '';
  result := false;
  //--------------------------------------------------------------------------
  if (DataObject.GetData(URLFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then
        exit;
      cText := PChar(GlobalLock(medium.HGlobal));
      fURL := cText;
      GlobalUnlock(medium.HGlobal);
      result := true;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  //--------------------------------------------------------------------------
  else if (DataObject.GetData(TextFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then
        exit;
      cText := PChar(GlobalLock(medium.HGlobal));
      fURL := cText;
      GlobalUnlock(medium.HGlobal);
      result := true;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  //--------------------------------------------------------------------------
  else if (DataObject.GetData(FileContentsFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then
        exit;
      cText := PChar(GlobalLock(medium.HGlobal));
      fURL := cText;
      fURL := copy(fURL,24,250);
      GlobalUnlock(medium.HGlobal);
      result := true;
    finally
      ReleaseStgMedium(medium);
    end;
  end
  //--------------------------------------------------------------------------
  else if (DataObject.GetData(HDropFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then exit;
      tmpFiles := TStringList.create;
      try
        if GetFilesFromHGlobal(medium.HGlobal,TStrings(tmpFiles)) and
          (lowercase(ExtractFileExt(tmpFiles[0])) = '.url') and
             GetURLFromFile(tmpFiles[0], fURL) then
        begin
            fTitle := extractfilename(tmpFiles[0]);
            delete(fTitle,length(fTitle)-3,4); //deletes '.url' extension
            result := true;
        end;
      finally
        tmpFiles.free;
      end;
    finally
      ReleaseStgMedium(medium);
    end;
  end;

  if (DataObject.GetData(FGDFormatEtc, medium) = S_OK) then
  begin
    try
      if (medium.tymed <> TYMED_HGLOBAL) then exit;
      pFGD := pointer(GlobalLock(medium.HGlobal));
      fTitle := pFGD^.fgd[0].cFileName;
      GlobalUnlock(medium.HGlobal);
      delete(fTitle,length(fTitle)-3,4); //deletes '.url' extension
    finally
      ReleaseStgMedium(medium);
    end;
  end
  else if fTitle = '' then fTitle := fURL;
end;
// ----------------------------------------------------------------------------- 
// ----------------------------------------------------------------------------- 

end.
