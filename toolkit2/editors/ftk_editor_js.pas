unit ftk_editor_js;

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
  Classes, SysUtils, SynEditHighlighter, SynHighlighterJscript,
  fsl_base, fsl_utilities, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { TJavascriptEditor }

  TJavascriptEditor = class (TBaseEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    function GetCanEscape : boolean; override;
    function escapeText(text : String): String; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
  end;


implementation

function TJavascriptEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJscriptSyn.Create(nil);
end;

procedure TJavascriptEditor.getNavigationList(navpoints: TStringList);
begin
end;

function TJavascriptEditor.GetCanEscape: boolean;
begin
  Result := sourceHasFocus;
end;

function TJavascriptEditor.escapeText(text: String): String;
begin
  Result := jsonEscape(text, false);
end;

constructor TJavascriptEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
end;

destructor TJavascriptEditor.Destroy;
begin
  inherited Destroy;
end;

procedure TJavascriptEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := 'function test() {'+#13#10+'  name = "value";'+#13#10+'}'+#13#10;
  updateToolbarButtons;
end;

function TJavascriptEditor.FileExtension: String;
begin
  result := 'Javascript';
end;

procedure TJavascriptEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
begin
  updateToContent;
  t := StartValidating;
  try
    if (validate) then
    begin
      for i := 0 to FContent.count - 1 do
      begin
        s := TextEditor.lines[i];
        checkForEncoding(s, i);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

