unit ftk_editor_json;

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
  Classes, SysUtils, SynEditHighlighter, SynHighlighterJson,
  fsl_base, fsl_utilities, fsl_json, fsl_logging, fsl_stream,
  ftk_context, ftk_store,
  ftk_editor_base;

type

  { TJsonEditor }

  TJsonEditor = class (TBaseEditor)
  private
    FParser : TJsonParser;
    FJson : TJsonNode;
    actPretty : TContentSubAction;
    actDense : TContentSubAction;
    procedure DoMnuPretty(sender : TObject);
    procedure DoMnuCondense(sender : TObject);
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    procedure ContentChanged; override;
    function hasFormatCommands: boolean; override;
    function GetCanEscape : boolean; override;
    function escapeText(text : String): String; override;
    procedure makeTextTab; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
    function canExecute : boolean; override;
  end;


implementation

constructor TJsonEditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TJsonParser.Create;
end;

destructor TJsonEditor.Destroy;
begin
  FParser.free;
  FJson.free;
  inherited Destroy;
end;

procedure TJsonEditor.DoMnuPretty(sender: TObject);
var
  j : TJsonObject;
begin
  j := TJSONParser.Parse(TextEditor.text);
  try
    SetContentUndoable(TJsonWriter.writeObjectStr(j, true));
  finally
    j.free;
  end;
end;

procedure TJsonEditor.DoMnuCondense(sender: TObject);
var
  j : TJsonObject;
begin
  j := TJSONParser.Parse(TextEditor.text);
  try
    SetContentUndoable(TJsonWriter.writeObjectStr(j, false));
  finally
    j.free;
  end;
end;

function TJsonEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynJSonSyn.Create(nil);
end;

procedure TJsonEditor.getNavigationList(navpoints: TStringList);
var
  c : integer;
  arr : TJsonArray;
  obj : TJsonObject;
  n : TJsonNode;
  s : String;
begin
  if (FJson = nil) then
  try
    FJson := FParser.parse(FContent.text);
  except
  end;
  if (FJson <> nil) then
  begin
    if (FJson is TJsonArray) then
    begin
      arr := FJson as TJsonArray;
      if arr.Count < 20 then
      begin
        c := 0;
        for n in arr do
        begin
          navpoints.AddObject('Item '+inttostr(c), TObject(n.LocationStart.line));
          inc(c);
        end;
      end;
    end
    else
    begin
      obj := FJson as TJsonObject;
      if obj.properties.count > 0 then
      begin
        for s in obj.properties.SortedKeys do
        begin
          navpoints.AddObject(s, TObject(obj.properties[s].LocationStart.line));
        end;
      end;
    end;
  end;
end;

procedure TJsonEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '{'+#13#10+'  "name": "value'+#13#10+'}'+#13#10;
  updateToolbarButtons;
end;

function TJsonEditor.FileExtension: String;
begin
  result := 'json';
end;

procedure TJsonEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  path : TFslList<TJsonPointerMatch>;
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
    FJson.free;
    FJson := nil;
    try
      FJson := FParser.parseNode(FContent.text);
      path := FJson.findLocation(cursor);
      try
        inspection.AddPair('Path', FJson.describePath(path));
      finally
        path.free;
      end;
    except
      on e : EParserException do
      begin
        validationError(e.Location, e.message);
      end;
      on e : Exception do
      begin
        validationError(TSourceLocation.CreateNull, 'Error Parsing Json: '+e.message);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;

function TJsonEditor.canExecute: boolean;
var
  j : TJsonObject;
begin
  try
    j := TJSONParser.Parse(TextEditor.text);
    try
      result := j.str['type'] = 'web.runner';
    finally
      j.free;
    end;
  except
    result := false;
  end;
end;

procedure TJsonEditor.ContentChanged;
begin
  FJson.free;
  FJson := nil;
end;

function TJsonEditor.hasFormatCommands: boolean;
begin
  Result := true;
end;

function TJsonEditor.GetCanEscape: boolean;
begin
  Result := sourceHasFocus;
end;

function TJsonEditor.escapeText(text: String): String;
begin
  Result := jsonEscape(text, false);
end;

procedure TJsonEditor.makeTextTab;
begin
  inherited makeTextTab;
  makeSubAction(actFormat, 'Pretty', 88, 0, DoMnuPretty);
  makeSubAction(actFormat, 'Condensed', 87, 0, DoMnuCondense);
  makeSubAction(actFormat, 'Condensed', 87, 0, DoMnuCondense);
end;

end.

