unit ftk_editor_md;

{
Copyright (c) 2001-2021, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  Classes, SysUtils, Controls,
  SynEditHighlighter, SynHighlighterHtml, HTMLView,
  MarkdownProcessor,
  fsl_base, fhir_xhtml, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { TMarkdownEditor }

  TMarkdownEditor = class (TBaseEditor)
  private
    FHtml : THtmlViewer;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;

    function hasDesigner : boolean; override;
    procedure makeDesigner; override;
    procedure updateDesigner; override;
  end;


implementation

function TMarkdownEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHtmlSyn.Create(nil);
end;

procedure TMarkdownEditor.getNavigationList(navpoints: TStringList);
var
  i : integer;
  s : String;
begin
  updateToContent;
  for i := 0 to FContent.count - 1 do
  begin
    s := FContent[i];
    if (s.StartsWith('# ') or s.StartsWith('## ') or s.StartsWith('### ') or s.StartsWith('#### ')) then
      navpoints.AddObject(s, TObject(i))
  end;
end;

procedure TMarkdownEditor.makeDesigner;
begin
  inherited makeDesigner;
  FHtml := THtmlViewer.Create(FDesignerPanelWork);
  FHtml.parent := FDesignerPanelWork;
  FHtml.align := alClient;
end;

constructor TMarkdownEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
//  FParser := TMHtmlParser.Create;
end;

destructor TMarkdownEditor.Destroy;
begin
//  FParser.free;
  inherited Destroy;
end;

procedure TMarkdownEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '# Heading'+#13#10+'Some Markdown content'+#13#10+''+#13#10+'* Item 1'+#13#10+'* Item 2'+#13#10+''+#13#10;
  updateToolbarButtons;
end;

function TMarkdownEditor.FileExtension: String;
begin
  result := 'md';
end;

procedure TMarkdownEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  //Html : TMHtmlDocument;
  t : QWord;
  s1, s2, s3, s4 : String;
begin
  updateToContent;
  t := StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := TextEditor.lines[i];
      if (validate) then
        checkForEncoding(s, i);
      if (s.StartsWith('# ')) then
      begin
        s1 := s.Substring(2);
        s2 := '';
        s3 := '';
        s4 := '';
      end
      else if (s.StartsWith('## ')) then
      begin
        s2 := s.Substring(3);
        s3 := '';
        s4 := '';
      end
      else if (s.StartsWith('### ')) then
      begin
        s3 := s.Substring(4);
        s4 := '';
      end
      else if (s.StartsWith('#### ')) then
      begin
        s4 := s.Substring(5);
      end;

      if (i = cursor.line) then
      begin
        if s1 <> '' then
          inspection.AddPair('H1', s1);
        if s2 <> '' then
          inspection.AddPair('H2', s2);
        if s3 <> '' then
          inspection.AddPair('H3', s3);
        if s4 <> '' then
          inspection.AddPair('H4', s4);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;

function TMarkdownEditor.hasDesigner : boolean;
begin
  Result := true;
end;

procedure TMarkdownEditor.updateDesigner;
var
  html : String;
  proc : TMarkdownProcessor;
begin
  inherited updateDesigner;

  proc := TMarkdownProcessor.createDialect(mdCommonMark); // or flavor of your choice
  try
    proc.AllowUnsafe := false;
    html := proc.process(FContent.text);
  finally
    proc.free;
  end;
  FHtml.LoadFromString(html);
end;


end.

