unit ftk_editor_ini;

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
  Classes, SysUtils, SynEditHighlighter, SynHighlighterIni,
  fsl_logging, fsl_stream,
  ftk_context, ftk_editor_base;

type

  { TIniEditor }

  TIniEditor = class (TBaseEditor)
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
  end;


implementation

function TIniEditor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynIniSyn.create(nil);
end;

procedure TIniEditor.getNavigationList(navpoints: TStringList);
var
  i : integer;
  s : String;
begin
  updateToContent;
  for i := 0 to FContent.count - 1 do
  begin
    s := FContent[i];
    if (s.StartsWith('[') and s.EndsWith(']')) then
     navpoints.AddObject(s, TObject(i));
  end;
end;

procedure TIniEditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

  TextEditor.Text := '[Section]'+#13#10+'; comments here'+#13#10+'name=value'+#13#10;
  updateToolbarButtons;
end;

function TIniEditor.FileExtension: String;
begin
  result := 'ini';
end;

procedure TIniEditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  section : String;
begin
  updateToContent;
  t := StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := FContent[i];
      if (validate) then
        checkForEncoding(s, i);
      if (s <> '') and not s.StartsWith(';') then
      begin
        s := s.trim;
        if s.StartsWith('[') then
        begin
          if s.EndsWith(']') then
            section := s.Substring(1, length(s)-2)
          else if validate then
            validationError(TSourceLocation.Create(i, 0), 'Improperly terminated section name - doesn''t end with ]');
        end
        else if (validate) and (not s.contains('=')) then
          validationWarning(TSourceLocation.Create(i, 0), 'No = found on non-comment line');
      end;
      if (i = cursor.line) then
        inspection.AddPair('Section', section);
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

