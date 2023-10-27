unit ftk_editor_hl7;

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
  Classes, SysUtils, SynEditHighlighter, fui_syn_hl7,
  fsl_base, fsl_logging, fsl_stream,
  v2_message,
  ftk_context, ftk_store, ftk_editor_base;

type

  { THL7Editor }

  THL7Editor = class (TBaseEditor)
  private
    FParser : TV2Parser;
    FMsg : TV2Message;
  protected
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    procedure ContentChanged; override;
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


constructor THL7Editor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  inherited Create(context, session, store);
  FParser := TV2Parser.Create;
end;

destructor THL7Editor.Destroy;
begin
  FParser.free;
  FMsg.free;
  inherited Destroy;
end;

function THL7Editor.makeHighlighter: TSynCustomHighlighter;
begin
  Result := TSynHL7Syn.Create(nil);
end;

procedure THL7Editor.getNavigationList(navpoints: TStringList);
var
  seg : TV2Segment;
begin
  if (FMsg = nil) then
  begin
    try
      FMsg := FParser.parse(FContent.text, []);
    except
    end;
  end;
  if (FMsg <> nil) then
  begin
    if FMsg.segmentList.count < 20 then
      for seg in FMsg.segmentList do
        navpoints.AddObject(seg.code, TObject(seg.LocationData.ParseStart.line));
  end;
end;

procedure THL7Editor.ContentChanged;
begin
  FMsg.free;
  FMsg := nil;
end;

function THL7Editor.GetCanEscape: boolean;
begin
  Result := sourceHasFocus;
end;

function THL7Editor.escapeText(text: String): String;
begin
  Result := text;
end;

procedure THL7Editor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCR;
  Session.Encoding := senASCII;

  TextEditor.Text := 'MSH|^~\&|GHH LAB|ELAB-3|GHH OE|BLDG4|200202150930||ORU^R01|CNTRL-3456|P|2.4'+#13+
    'PID|||555-44-4444||EVERYWOMAN^EVE^E^^^^L|JONES|19620320|F|||153 FERNWOOD DR.^^STATESVILLE^OH^35292||(206)3345232|(206)752-121||||AC555444444||67-A4335^OH^20030520'+#13+
    'OBR|1|845439^GHH OE|1045813^GHH LAB|15545^GLUCOSE|||200202150730||||||||| 555-55-5555^PRIMARY^PATRICIA P^^^^MD|||||||||F||||||444-44-4444^HIPPOCRATES^HOWARD H^^^^MD'+#13+
    'OBX|1|SN|1554-5^GLUCOSE^POST 12H CFST:MCNC:PT:SER/PLAS:QN||^182|mg/dl|70_105|H|||F'+#13;
  updateToolbarButtons;
end;

function THL7Editor.FileExtension: String;
begin
  result := 'hl7';
end;

procedure THL7Editor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  section : String;
  path : TV2Location;
begin
  updateToContent;
  t := StartValidating;
  try
    for i := 0 to FContent.count - 1 do
    begin
      s := FContent[i];
      if (validate) then
        checkForEncoding(s, i);
    end;
    FMsg.free;
    FMsg := nil;
    try
      FMsg := FParser.parse(FContent.text, [v2Validating]);
      path := FMsg.findLocation(cursor);
      try
        if path.Segment <> nil then
          inspection.AddPair('Segment', path.Segment.Code+' ('+path.Segment.id+')');
        if path.Field <> nil then
          inspection.AddPair('Field', path.Field.id);
        if path.Element <> nil then
          inspection.AddPair('Repeat', path.Element.id);
        if path.Component <> nil then
          inspection.AddPair('Component', path.Component.id);
        if path.SubComponent <> nil then
          inspection.AddPair('SubComponent', path.SubComponent.id);
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
        validationError(TSourceLocation.CreateNull, 'Error Parsing HL7: '+e.message);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;


end.

