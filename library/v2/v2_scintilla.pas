unit v2_scint;

{
Copyright (c) 2014+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

{$I fhir.inc}

interface

uses
  SysUtils, Classes, Graphics,
  ScintEdit,
  fsl_base, fsl_stream,
  cda_base, cda_objects, cda_documents;
(*
  gwCDA, v3Infrastructure, XMLBuilder, ReferenceModelBase, CDATypes;
*)


type
  TV2StylerStyle = (
    stSegCode,
    stText,
    stDelimiter,
    stEscape
  );

  TV2Styler = class(TScintCustomStyler)
  private
  protected
    procedure CommitStyle(Style: TV2StylerStyle);
    procedure GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes); override;
    function LineTextSpans(const S: TScintRawString): Boolean; override;
    procedure StyleNeeded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; Override;
  end;

implementation

{ TV2Styler }

constructor TV2Styler.Create(AOwner: TComponent);
begin
  inherited;
end;

destructor TV2Styler.Destroy;
begin
  inherited;
end;

procedure TV2Styler.CommitStyle(Style: TV2StylerStyle);
begin
  LineState := Ord(Style);
  inherited CommitStyle(LineState);
end;

procedure TV2Styler.GetStyleAttributes(const Style: Integer; var Attributes: TScintStyleAttributes);
var
  st : TV2StylerStyle;
begin
  st := TV2StylerStyle(style and $F);
  case st of
    stSegCode : Attributes.FontStyle := [fsBold];
    stText : ; // nothing
    stDelimiter : Attributes.ForeColor := clPurple;
    stEscape :
      begin
      Attributes.ForeColor := clMaroon;
      Attributes.FontStyle := [fsItalic];
      end;
  end;
end;

function TV2Styler.LineTextSpans(const S: TScintRawString): Boolean;
begin
  result := false;
end;

procedure TV2Styler.StyleNeeded;
begin
  if hasToken('MSH') then
  begin
    ConsumeChar;
    ConsumeChar;
    ConsumeChar;
    CommitStyle(stSegCode);
    ConsumeChar;
    CommitStyle(stDelimiter);
    ConsumeChar;
    ConsumeChar;
    ConsumeChar;
    ConsumeChar;
    CommitStyle(stText);
  end
  else
  begin
    ConsumeChar;
    ConsumeChar;
    ConsumeChar;
    CommitStyle(stSegCode);
  end;
  while not EndOfLine do
  begin
    if CurCharIn(['|', '^', '~', '&']) then
    begin
      ConsumeChar;
      CommitStyle(stDelimiter);
    end
    else if CurCharIs('\') then
    begin
      ConsumeChar;
      CommitStyle(stDelimiter);
      ConsumeCharsNot(['\']);
      CommitStyle(stEscape);
      ConsumeChar;
      CommitStyle(stDelimiter);
    end
    else
    begin
     ConsumeCharsNot(['|', '^', '~', '&', '\']);
     CommitStyle(stText);
    end;
  end;
end;

end.
