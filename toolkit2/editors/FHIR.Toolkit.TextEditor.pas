unit FHIR.Toolkit.TextEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, ComCtrls, Menus, SynEdit, SynEditHighlighter,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Toolkit.Context;

type

  { TTextEditor }

  TTextEditor = class (TToolkitEditor)
  private
    btnNavigate : TToolButton;
    btnEncoding : TToolButton;
    btnLineMarkers : TToolButton;
    btnBOM : TToolButton;
    pmEncoding : TPopupMenu;
    mnAscii : TMenuItem;
    mnUtf8 : TMenuItem;
    mnUtf16L : TMenuItem;
    mnUtf16R : TMenuItem;
    pmLineMarkers : TPopupMenu;
    mnCRLF : TMenuItem;
    mnLF : TMenuItem;
    mnCR : TMenuItem;
    pmBOM : TPopupMenu;
    mnNoBOM : TMenuItem;
    mnBOM : TMenuItem;
    pmNavigation : TPopupMenu;

    procedure DoBtnNavigation(sender : TObject);
    procedure DoBtnEncoding(sender : TObject);
    procedure DoBtnLineMarkers(sender : TObject);
    procedure DoBtnBOM(sender : TObject);
    procedure DoMnuEncoding(sender : TObject);
    procedure DoMnuLineMarkers(sender : TObject);
    procedure DoMnuBOM(sender : TObject);
    procedure DoMnuNavigate(sender : TObject);
    procedure ShowPopup(btn: TToolButton; mnu: TPopupMenu);
    procedure DoTextEditorChange(sender : TObject);
  protected
    TextEditor : TSynEdit;
    HighLighter : TSynCustomHighlighter;


    function GetCanBeSaved: boolean; override;

    procedure GotoLine(line : integer);

    function AddToolbarButtons : boolean; virtual;
    function makeHighlighter : TSynCustomHighlighter; virtual;
    procedure getNavigationList(ts : TStringList); virtual;
    function MustBeUnicode : boolean; virtual;
  public
    function GetBytes: TBytes; override;
    procedure LoadBytes(bytes: TBytes); override;
    procedure bindToTab(tab : TTabSheet); override;
    procedure locate(location : TSourceLocation); override;
    function location : String; override;
    procedure redo; override;
    procedure updateToolbarButtons; virtual;
  end;

implementation

uses
  frm_main;

{ TTextEditor }

function TTextEditor.GetBytes: TBytes;
var
  b : TStringBuilder;
  s : String;
  first : boolean;
begin
  b := TStringBuilder.create;
  try
    first := true;
    for s in TextEditor.lines do
    begin
      if first then
        first := false
      else case Session.EndOfLines of
          slCR : b.append(#13);
          slLF : b.append(#10);
        else
          b.append(#13#10);
        end;
      b.append(s);
    end;
    case Session.Encoding of
      senASCII : result := TEncoding.ASCII.GetBytes(b.toString);
      senUTF16BE : result := TEncoding.BigEndianUnicode.GetBytes(b.toString);
      senUTF16LE : result := TEncoding.Unicode.GetBytes(b.toString);
    else
      result := TEncoding.UTF8.GetBytes(b.toString);
    end;
  finally
    b.free;
  end;
end;

//
//begin
//  // try to automatically detect the buffer encoding
//
//  // detected encoding points to Default and param Encoding requests some other
//  // type of Encoding; set the Encoding param to UTF8 as it can also read ANSI (Default)
//  if (LEncoding = TEncoding.Default) and (Encoding <> TEncoding.Default) then
//    Encoding := TEncoding.UTF8
//  else
//    Encoding := LEncoding;
//
//  FDetectBOM := False;
//end;
//

function oddBytesZero(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 1;
  while (i < length(bytes)) do
  begin
    if bytes[i] <> 0 then
      exit(false);
    inc(i, 2);
  end;
end;

function evenBytesZero(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 0;
  while (i < length(bytes)) do
  begin
    if bytes[i] <> 0 then
      exit(false);
    inc(i, 2);
  end;
end;

function allBytesAscii(bytes : TBytes) : boolean;
var
  i : integer;
begin
  result := true;
  i := 0;
  while (i < length(bytes)) do
  begin
    if not (bytes[i] in [9, 10, 13, 32..127]) then
      exit(false);
    inc(i);
  end;
end;

procedure TTextEditor.LoadBytes(bytes: TBytes);
var
  encoding: TEncoding;
  start : integer;
  src : String;
  b : TBytes;
  lf, cr, crlf : boolean;
  cursor, linestart : integer;
  ch : char;
begin
  if (length(bytes) = 0) then
  begin
    Session.HasBOM := false;
    Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
    Session.Encoding := senUTF8;
    TextEditor.Clear;
  end
  else
  begin
    encoding := nil;
    // do we have a BOM?
    start := TEncoding.GetBufferEncoding(bytes, encoding);
    if (start <> 0) then
    begin
      Session.HasBOM := true;
      if encoding = TEncoding.UTF8 then
        Session.Encoding := senUTF8
      else if encoding = TEncoding.BigEndianUnicode then
        Session.Encoding := senUTF16BE
      else if encoding = TEncoding.Unicode then
        Session.Encoding := senUTF16LE
      else
        Session.Encoding := senUTF8; // ...?
    end
    else if (encoding = nil) then
    begin
      Session.HasBOM := false;
      if (length(bytes) > 10) then
      begin
        b := copy(bytes, 1, 10);
        if oddBytesZero(b) and not evenBytesZero(b) then
        begin
          Session.Encoding := senUTF16BE;
          encoding := TEncoding.BigEndianUnicode;
        end
        else if not oddBytesZero(b) and evenBytesZero(b) then
        begin
          Session.Encoding := senUTF16LE;
          encoding := TEncoding.Unicode;
        end
      end;
      if encoding = nil then
      begin
        if allBytesAscii(bytes) and not MustBeUnicode then
        begin
          Session.Encoding := senASCII;
          encoding := TEncoding.ASCII;
        end
        else
        begin
          Session.Encoding := senUTF8;
          encoding := TEncoding.UTF8;
        end;
      end;
    end
    else if encoding = TEncoding.Unicode then
      Session.Encoding := senUTF16LE
    else if encoding = TEncoding.BigEndianUnicode then
      Session.Encoding := senUTF16BE
    else if encoding = TEncoding.ASCII then
      Session.Encoding := senASCII
    else
      Session.Encoding := senUTF8;

    src := encoding.GetString(bytes, start, length(bytes) - start);

    lf := false;
    cr := false;
    crlf := false;
    TextEditor.BeginUpdate(false);
    try
      TextEditor.Lines.Clear;
      lineStart := 1;
      Cursor := 1;
      while (Cursor <= length(src)) do
      begin
        ch := src[Cursor];
        if (ch in [#13,#10]) then
        begin
          TextEditor.Lines.add(copy(src, lineStart, cursor-linestart));
          if (Cursor < length(src)) and (ch = #13) and (src[Cursor+1] = #10) then
          begin
            crlf := true;
            inc(cursor, 2);
          end
          else
          begin
            lf := lf or (ch = #10);
            cr := cr or (ch = #13);
            inc(cursor);
          end;
          lineStart := cursor;
        end
        else
          inc(cursor);
      end;
      if (LineStart < length(src)) then
        TextEditor.Lines.add(copy(src, lineStart, cursor-linestart))
      else
        TextEditor.Lines.add('');
    finally
      TextEditor.EndUpdate;
    end;
    if crlf then
      Session.EndOfLines := slCRLF
    else if cr then
      Session.EndOfLines := slCR
    else
      Session.EndOfLines := slLF
  end;
  updateToolbarButtons;
end;

function TTextEditor.GetCanBeSaved: boolean;
begin
  result := true;
end;

procedure TTextEditor.GotoLine(line: integer);
begin
  TextEditor.CaretXY := Point(1,line+1);
end;

function TTextEditor.AddToolbarButtons: boolean;
begin
  result := false;
end;

procedure TTextEditor.updateToolbarButtons;
begin
  case Session.EndOfLines of
    slUnknown : btnLineMarkers.ImageIndex := 0;
    slCRLF: btnLineMarkers.ImageIndex := 0;
    slLF : btnLineMarkers.ImageIndex := 2;
    slCR : btnLineMarkers.ImageIndex := 3;
  end;
  case Session.Encoding of
    senUnknown: btnEncoding.ImageIndex := 4;
    senBinary: btnEncoding.ImageIndex := 5;
    senUTF8: btnEncoding.ImageIndex := 7;
    senASCII: btnEncoding.ImageIndex := 6;
    senUTF16BE: btnEncoding.ImageIndex := 8;
    senUTF16LE: btnEncoding.ImageIndex := 9;
  end;
  if Session.Encoding in [senUnknown, senBinary, senASCII] then
  begin
    btnBOM.Enabled := false;
    btnBOM.ImageIndex := 10;
  end
  else
  begin
    btnBOM.Enabled := true;
    if Session.HasBOM then
      btnBOM.ImageIndex := 11
    else
      btnBOM.ImageIndex := 10;
  end;
end;

function TTextEditor.makeHighlighter: TSynCustomHighlighter;
begin
  result := nil;
end;

procedure TTextEditor.getNavigationList(ts: TStringList);
begin
end;

function TTextEditor.MustBeUnicode: boolean;
begin
  result := false;
end;

function addButtonToToolbar(bar: TToolBar; imgIndex : integer; event : TNotifyEvent) : TToolButton;
var
  index : integer;
begin
  result := TToolButton.Create(bar);
  result.ImageIndex := imgIndex;
  index := bar.ButtonCount - 1;
  if index > -1 then
    result.Left := bar.Buttons[index].Left + bar.Buttons[index].Width
  else
    result.Left := 0;
  result.Parent := bar;
  result.OnClick := event;
end;

function addMenuItem(pm : TPopupMenu; s : String; imgIndex : integer; event : TNotifyEvent) : TMenuItem;
begin
  result := TMenuItem.create(nil);
  result.Caption := s;
  result.ImageIndex := imgIndex;
  result.OnClick := event;
  pm.Items.Add(result);
end;

procedure TTextEditor.bindToTab(tab: TTabSheet);
var
  tb : TToolBar;
  tbtn : TToolButton;
begin
  inherited bindToTab(tab);

  // 1. the content toolbar...
  tb := TToolBar.create(tab);
  tb.parent := tab;
  tb.align := alTop;
  tb.Images := MainToolkitForm.imgContext;

  btnNavigate := addButtonToToolbar(tb, 12, DoBtnNavigation);
  addButtonToToolbar(tb, 0, nil).Style := tbsDivider;
  if AddToolbarButtons then
    addButtonToToolbar(tb, 0, nil).Style := tbsDivider;
  btnEncoding := addButtonToToolbar(tb, 0, DoBtnEncoding);
  btnLineMarkers := addButtonToToolbar(tb, 4, DoBtnLineMarkers);
  btnBOM := addButtonToToolbar(tb, 10, DoBtnBOM);

  pmLineMarkers := TPopupMenu.create(tab);
  pmLineMarkers.Images := MainToolkitForm.imgContext;
  mnCRLF := addMenuItem(pmLineMarkers, 'Windows', 0, DoMnuLineMarkers);
  mnLF := addMenuItem(pmLineMarkers, 'Unix', 2, DoMnuLineMarkers);
  mnCR := addMenuItem(pmLineMarkers, 'Macintosh', 3, DoMnuLineMarkers);

  pmEncoding := TPopupMenu.create(tab);
  pmEncoding.Images := MainToolkitForm.imgContext;
  mnAscii := addMenuItem(pmEncoding, 'ASCII', 6, DoMnuEncoding);
  mnUtf8 := addMenuItem(pmEncoding, 'UTF8 (Unicode)', 7, DoMnuEncoding);
  mnUtf16L := addMenuItem(pmEncoding, 'UTF16 BE', 8, DoMnuEncoding);
  mnUtf16R := addMenuItem(pmEncoding, 'UTF16 LE', 9, DoMnuEncoding);

  pmBOM := TPopupMenu.create(tab);
  pmBOM.Images := MainToolkitForm.imgContext;
  mnNoBOM := addMenuItem(pmBOM, 'No BOM', 10, DoMnuBOM);
  mnBOM := addMenuItem(pmBOM, 'BOM', 11, DoMnuBOM);

  pmNavigation := TPopupMenu.create(tab);

  // 2. the Synedit
  Highlighter := makeHighlighter;
  TextEditor := TSynEdit.create(tab);
  TextEditor.parent := tab;
  TextEditor.align := alClient;
  TextEditor.Highlighter := HighLighter;
  TextEditor.OnChange := DoTextEditorChange;


  // add the tool bar
  // add the syn edit
  // set up the high lighting
  //

end;

procedure TTextEditor.ShowPopup(btn : TToolButton; mnu : TPopupMenu);
var
  pt, pt2: TPoint;
begin
  pt.x := MainToolkitForm.pgEditors.Left + btn.Left;
  pt.y := MainToolkitForm.pgEditors.top + (MainToolkitForm.pgEditors.height - Tab.height)  + btn.top + btn.Height;
  pt2 := MainToolkitForm.ClientToScreen(pt);
  mnu.PopUp(pt2.x, pt2.y);
end;

procedure TTextEditor.DoTextEditorChange(sender: TObject);
begin
  canRedo := TextEditor.CanRedo;
  Session.NeedsSaving := true;
  lastChange := GetTickCount64;
  lastChangeChecked := false;
  Context.OnUpdateActions(self);;
end;

procedure TTextEditor.DoBtnNavigation(sender: TObject);
var
  ts : TStringList;
  i : integer;
begin
  pmNavigation.Items.clear;
  ts := TStringList.create;
  try
    ts.addObject('Start', TObject(0));
    getNavigationList(ts);
    ts.addObject('End', TObject($FFFFFFF));
    for i := 0 to ts.count - 1 do
      addMenuItem(pmNavigation, ts[i], -1, DoMnuNavigate).Tag := integer(ts.objects[i]);
  finally
    ts.free;
  end;
  ShowPopup(btnNavigate, pmNavigation);
end;

procedure TTextEditor.DoBtnEncoding(sender : TObject);
begin
  ShowPopup(btnEncoding, pmEncoding);
end;

procedure TTextEditor.DoBtnLineMarkers(sender : TObject);
begin
  ShowPopup(btnLineMarkers, pmLineMarkers);
end;

procedure TTextEditor.DoBtnBOM(sender : TObject);
begin
  ShowPopup(btnBOM, pmBOM);
end;

procedure TTextEditor.DoMnuEncoding(sender: TObject);
begin
  case (Sender as TMenuItem).ImageIndex of
    4: Session.Encoding := senUnknown;
    5: Session.Encoding := senBinary;
    6: Session.Encoding := senASCII;
    7: Session.Encoding := senUTF8;
    8: Session.Encoding := senUTF16BE;
    9: Session.Encoding := senUTF16LE;
  end;
  updateToolbarButtons;
end;

procedure TTextEditor.DoMnuLineMarkers(sender: TObject);
begin
  case (Sender as TMenuItem).ImageIndex of
    0: Session.EndOfLines := slCRLF;
    1: Session.EndOfLines := slLF;
    2: Session.EndOfLines := slLF;
    3: Session.EndOfLines := slCR;
  end;
  updateToolbarButtons;
end;

procedure TTextEditor.DoMnuBOM(sender: TObject);
begin
  case (Sender as TMenuItem).ImageIndex of
    10: Session.HasBOM := false;
    11: Session.HasBOM := true;
  end;
  updateToolbarButtons;
end;

procedure TTextEditor.DoMnuNavigate(sender: TObject);
var
  mnu : TMenuItem;
begin
  mnu := sender as TMenuItem;
  if mnu.Tag = $FFFFFFF then
    GotoLine(TextEditor.lines.count - 1)
  else
    GotoLine(mnu.tag);
end;

procedure TTextEditor.locate(location: TSourceLocation);
begin

end;

function TTextEditor.location: String;
begin
  result := 'Ln: '+inttostr(TextEditor.CaretXY.Y)+', Col: '+inttostr(TextEditor.CaretXY.X);
end;

procedure TTextEditor.redo;
begin
  TextEditor.redo;
end;

end.

