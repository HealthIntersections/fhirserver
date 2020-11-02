unit FHIR.Toolkit.TextEditor;

{$i fhir.inc}

interface

uses
  Classes, SysUtils,
  Controls, ComCtrls, Menus, SynEdit, SynEditHighlighter,
  FHIR.Support.Base, FHIR.Support.Utilities, FHIR.Support.Stream,
  FHIR.Toolkit.Context, FHIR.Toolkit.BaseEditor;

type

  { TTextEditor }

  TTextEditor = class (TBaseEditor)
  private
    actNavigate : TContentAction;
    actEncoding : TContentAction;
    actLineMarkers : TContentAction;
    actBOM : TContentAction;

    procedure MakeNavigationItems(sender : TObject);
    procedure DoMnuEncoding(sender : TObject);
    procedure DoMnuLineMarkers(sender : TObject);
    procedure DoMnuBOM(sender : TObject);
    procedure DoMnuNavigate(sender : TObject);
    procedure DoTextEditorChange(sender : TObject);
  protected
    TextEditor : TSynEdit;
    HighLighter : TSynCustomHighlighter;
    FEditorPopup : TPopupMenu;

    function GetCanBeSaved: boolean; override;
    procedure GotoLine(line : integer);
    function AddActions : boolean; virtual;
    function makeHighlighter : TSynCustomHighlighter; virtual;
    procedure getNavigationList(ts : TStringList); virtual;
    function MustBeUnicode : boolean; virtual;
    procedure validate; virtual;
    procedure checkForEncoding(s : String; line : integer);
  public
    function GetBytes: TBytes; override;
    procedure LoadBytes(bytes: TBytes); override;
    procedure bindToTab(tab : TTabSheet); override;
    procedure locate(location : TSourceLocation); override;
    function location : String; override;
    procedure redo; override;
    procedure updateToolbarButtons; virtual;
    procedure getFocus(content : TMenuItem); override;
    procedure loseFocus(); override;
    procedure EditPause; override;
  end;

implementation
//
//uses
//  frm_main;

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

function TTextEditor.AddActions: boolean;
begin
  result := false;
end;

procedure TTextEditor.updateToolbarButtons;
begin
  case Session.EndOfLines of
    slUnknown : actLineMarkers.ImageIndex := 51;
    slCRLF: actLineMarkers.ImageIndex := 51;
    slLF : actLineMarkers.ImageIndex := 53;
    slCR : actLineMarkers.ImageIndex := 54;
  end;
  case Session.Encoding of
    senUnknown: actEncoding.ImageIndex := 55;
    senBinary: actEncoding.ImageIndex := 56;
    senUTF8: actEncoding.ImageIndex := 58;
    senASCII: actEncoding.ImageIndex := 57;
    senUTF16BE: actEncoding.ImageIndex := 59;
    senUTF16LE: actEncoding.ImageIndex := 60;
  end;
  if Session.Encoding in [senUnknown, senBinary, senASCII] then
  begin
    actBOM.Enabled := false;
    actBOM.ImageIndex := 61;
  end
  else
  begin
    actBOM.Enabled := true;
    if Session.HasBOM then
      actBOM.ImageIndex := 62
    else
      actBOM.ImageIndex := 61;
  end;
end;

procedure TTextEditor.getFocus(content: TMenuItem);
begin
  inherited getFocus(content);
  TextEditor.SetFocus;
end;

procedure TTextEditor.loseFocus();
begin
  inherited loseFocus();
end;

procedure TTextEditor.EditPause;
begin
  inherited EditPause;
  validate;
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

procedure TTextEditor.validate;
begin
  // nothing
end;

procedure TTextEditor.checkForEncoding(s: String; line: integer);
begin

end;


procedure TTextEditor.bindToTab(tab: TTabSheet);
var
  tb : TToolBar;
  mnu : TMenuItem;
begin
  inherited bindToTab(tab);

  FEditorPopup := TPopupMenu.create(tab);
  FEditorPopup.Images := Context.images;

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditUndo');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditCut');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditCopy');
  FEditorPopup.Items.Add(mnu);

  mnu := TMenuItem.create(tab);
  mnu.Action := Context.actions.ActionByName('actionEditPaste');
  FEditorPopup.Items.Add(mnu);

  tb := TToolBar.create(tab);
  tb.parent := tab;
  tb.align := alTop;
  tb.Images := Context.Images;

  actNavigate := makeAction(tb, '&Navigate', 63);
  actNavigate.OnPopulate := MakeNavigationItems;
  makeDivider(tb);
  if AddActions then
    makeDivider(tb);
  actEncoding := makeAction(tb, 'Encoding', 0);
  makeSubAction(actEncoding, 'ASCII', 57, 0, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF8 (Unicode)', 58, 1, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF16 BE', 59, 2, DoMnuEncoding);
  makeSubAction(actEncoding, 'UTF16 LE', 60, 3, DoMnuEncoding);

  actLineMarkers := makeAction(tb, 'End of Lines', 4);
  makeSubAction(actLineMarkers, 'Windows (CR/LF)', 51, 0, DoMnuLineMarkers);
  makeSubAction(actLineMarkers, 'Unix (LF)', {$IFDEF OSX}52{$ELSE}53{$ENDIF}, 1, DoMnuLineMarkers);
  makeSubAction(actLineMarkers, 'Macintosh (CR)', 54, 3, DoMnuLineMarkers);

  actBOM := makeAction(tb, 'Byte Order Mark', 10);
  makeSubAction(actBOM, 'No BOM', 61, 0, DoMnuBOM);
  makeSubAction(actBOM, 'BOM', 62, 1, DoMnuBOM);

  // 2. the Synedit
  Highlighter := makeHighlighter;
  TextEditor := TSynEdit.create(tab);
  TextEditor.parent := tab;
  TextEditor.align := alClient;
  TextEditor.Highlighter := HighLighter;
  TextEditor.OnChange := DoTextEditorChange;
  TextEditor.PopupMenu := FEditorPopup;

end;

procedure TTextEditor.DoTextEditorChange(sender: TObject);
begin
  canRedo := TextEditor.CanRedo;
  Session.NeedsSaving := true;
  lastChange := GetTickCount64;
  lastChangeChecked := false;
  Context.OnUpdateActions(self);;
end;

procedure TTextEditor.MakeNavigationItems(sender: TObject);
var
  ts : TStringList;
  i : integer;
begin
  actNavigate.subItems.clear;
  makeSubAction(actNavigate, 'Start', -1, 0, DoMnuNavigate);
  ts := TStringList.create;
  try
    getNavigationList(ts);
    for i := 0 to ts.count - 1 do
      makeSubAction(actNavigate, ts[i], -1, integer(ts.objects[i]), DoMnuNavigate);
  finally
    ts.free;
  end;
  makeSubAction(actNavigate, 'End', -1, $FFFFFFF, DoMnuNavigate);
end;

procedure TTextEditor.DoMnuEncoding(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    10: Session.Encoding := senUnknown;
    11: Session.Encoding := senBinary;
    0: Session.Encoding := senASCII;
    1: Session.Encoding := senUTF8;
    2: Session.Encoding := senUTF16BE;
    3: Session.Encoding := senUTF16LE;
  end;
  updateToolbarButtons;
end;

procedure TTextEditor.DoMnuLineMarkers(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: Session.EndOfLines := slCRLF;
    1: Session.EndOfLines := slLF;
    2: Session.EndOfLines := slLF;
    3: Session.EndOfLines := slCR;
  end;
  updateToolbarButtons;
end;

procedure TTextEditor.DoMnuBOM(sender: TObject);
begin
  case (Sender as TMenuItem).Tag of
    0: Session.HasBOM := false;
    1: Session.HasBOM := true;
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
  TextEditor.SetFocus;
end;

procedure TTextEditor.locate(location: TSourceLocation);
begin
  TextEditor.CaretY := location.line;
  TextEditor.CaretX := location.col;
  TextEditor.SetFocus;
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

