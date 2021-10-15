unit ftk_editor_fhir;

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
  Classes, SysUtils,
  Controls, StdCtrls, ComCtrls,
  SynEditHighlighter, SynHighlighterXml, SynHighlighterJson,
  fsl_base, fsl_utilities, fsl_xml, fsl_json, fsl_logging, fsl_stream,
  fsl_http,
  fhir_objects, fhir_factory, fhir_parser,
  {fhir2_parsers, fhir3_parsers, }fhir4_factory, {fhir5_parsers, }
  fhir4_resources_canonical,
  fui_lcl_managers,
  ftk_context, ftk_store,

  ftk_frame_resource, ftk_frame_codesystem, ftk_frame_resource_tree, ftk_frame_patient,

  ftk_editor_base;

type

  { TFHIREditor }

  TFHIREditor = class (TBaseEditor)
  private
    FFormat : TFHIRFormat;
    FVersion : TFHIRVersion;
    FFactory : TFHIRFactory;
    FResource : TFHIRResourceV;
    FDesigner : TResourceDesignerFrame;
    actTestEditing : TContentAction;
    FSync : TFHIRSynEditSynchroniser;
    actSwitch : TContentSubAction;
    function parseResource(source : String) : TFHirResourceV;
    procedure DoTestEditing(sender : TObject);
    procedure DoMnuPretty(sender : TObject);
    procedure DoMnuCondense(sender : TObject);
    procedure DoMnuSwitch(sender : TObject);
    function getCurrentFormat : TFHIRFormat;
    procedure doSelectSourceRange(sender : TObject; start, stop : TPoint);
    function frameFactory(rType : String) : TResourceDesignerFrame;
  protected
    function AddActions(tb : TToolBar) : boolean; override;
    function makeHighlighter : TSynCustomHighlighter; override;
    procedure getNavigationList(navpoints : TStringList); override;
    function hasFormatCommands: boolean; override;
    procedure makeTextTab; override;
    procedure updateFormatMenu; override;
    function GetCanEscape : boolean; override;
    function escapeText(text : String): String; override;
  public
    constructor Create(context : TToolkitContext; session : TToolkitEditSession; store : TStorageService); override;
    destructor Destroy; override;

    procedure ContentChanged; override;
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
    procedure saveStatus; override;

    function hasDesigner : boolean; override;
    procedure makeDesigner; override;
    procedure updateDesigner; override;
  end;


implementation

constructor TFHIREditor.Create(context: TToolkitContext; session: TToolkitEditSession; store: TStorageService);
begin
  FFormat := TFHIRFormat(ord(StringArrayIndexOf(CODES_TFHIRFormat, session.info.Values['Format'])));
  inherited Create(context, session, store);
  FFactory := context.factory(fhirVersionRelease4);
end;

destructor TFHIREditor.Destroy;
begin
  FResource.Free;
  FFactory.Free;
  FSync.Free;
  inherited Destroy;
end;

function TFHIREditor.parseResource(source: String): TFHirResourceV;
var
  p : TFHIRParser;
begin
  p := FFactory.makeParser(nil, getCurrentFormat, THTTPLanguages.Create('en'));
  try
    p.KeepParseLocations := true;
    result := p.parseResource(source);
  finally
    p.Free;
  end;
end;

procedure TFHIREditor.DoTestEditing(sender: TObject);
var
  cs : TFHIRCodeSystem;
begin
  //if FSync.resource <> nil then
  //begin
  //  FSync.Format := getCurrentFormat;
  //  FSync.load;
  //end;
  //cs := FSync.resource as TFHIRCodeSystem;
  //Fsync.changeProperty(cs, cs.nameElement);
  //cs.name := 'My Test';
  //Fsync.commit;
end;

function TFHIREditor.AddActions(tb : TToolBar): boolean;
begin
  actTestEditing := makeAction(tb, 'Test Editing', 11, 0, DoTestEditing);
  Result := true;
end;

function TFHIREditor.makeHighlighter: TSynCustomHighlighter;
begin
  if FFormat = ffJson then
    Result := TSynJSonSyn.create(nil)
  else
    Result := TSynXmlSyn.create(nil);
end;

procedure TFHIREditor.getNavigationList(navpoints: TStringList);
var
  de, e : TMXmlElement;
  c : integer;
  properties : TFHIRPropertyList;
  prop : TFHIRProperty;
  incNext : boolean;
  v : TFHIRObject;
begin
  if (FResource = nil) then
  try
    FResource := parseResource(FContent.text);
  except
  end;
  if FResource <> nil then
  begin
    properties := FResource.createPropertyList(false);
    try
      for prop in properties do
      begin
        if prop.hasValue then
        begin
          if incNext then
          begin
            navpoints.addObject(prop.Name, TObject(prop.Values[0].LocationData.ParseStart.line));
            incNext := false;
          end
          else if prop.Name = 'text' then
          begin
            incNext := true;
          end
          else if prop.Name = 'contained' then
          begin
            for v in prop.Values do
              if (navpoints.count < 30) then
                navpoints.addObject(v.fhirType, TObject(v.LocationData.ParseStart.line));
          end
          else if prop.Type_ = 'BackboneElement' then
          begin
            for v in prop.Values do;
              if (navpoints.count < 30) then
                navpoints.addObject(prop.name, TObject(v.LocationData.ParseStart.line));
          end;
        end;
      end;
    finally
      properties.Free;
    end;
  end;
end;

function TFHIREditor.hasFormatCommands: boolean;
begin
  Result := true;
end;

procedure TFHIREditor.makeTextTab;
begin
  inherited makeTextTab;
  makeSubAction(actFormat, 'Pretty', 88, 0, DoMnuPretty);
  makeSubAction(actFormat, 'Condensed', 87, 0, DoMnuCondense);
  if FFormat = ffJson then
    actSwitch := makeSubAction(actFormat, 'To XML', 90, 0, DoMnuSwitch)
  else
    actSwitch := makeSubAction(actFormat, 'To Json', 91, 0, DoMnuSwitch);
end;

procedure TFHIREditor.updateFormatMenu;
begin
  inherited updateFormatMenu;
  if getCurrentFormat = ffJson then
  begin
    actSwitch.caption := 'To XML';
    actSwitch.imageIndex := 90;
  end
  else
  begin
    actSwitch.caption := 'To Json';
    actSwitch.imageIndex := 91;
  end;
end;

function TFHIREditor.GetCanEscape: boolean;
begin
  Result := sourceHasFocus;
end;

function TFHIREditor.escapeText(text: String): String;
begin
  if FFormat = ffJson then
    result := jsonEscape(text, false)
  else
    result := FormatTextToXML(text, xmlText);
end;

procedure TFHIREditor.ContentChanged;
begin
  FResource.Free;
  FResource := nil;
end;

procedure TFHIREditor.newContent();
begin
  Session.HasBOM := false;
  Session.EndOfLines := PLATFORM_DEFAULT_EOLN;
  Session.Encoding := senUTF8;

  TextEditor.Text := '<Resource xmlns="http://hl7.org/fhir">'+#13#10+'  <id value="xx"/>'+#13#10+'</Resource>'+#13#10;
  updateToolbarButtons;
end;

function TFHIREditor.FileExtension: String;
begin
  if FFormat = ffJson then
    result := 'json'
  else
    result := 'xml';
end;

function asPathExpression(path : TFslList<TFHIRLocatedNode>) : String;
var
  loc : TFHIRLocatedNode;
begin
  result := '';
  for loc in path do
  begin
    if loc.prop = nil then
      result := loc.value.fhirType
    else if loc.prop.IsList then
      result := result + '.'+ loc.prop.Name+'['+inttostr(loc.prop.values.IndexByReference(loc.value))+']'
    else if loc.prop.name.endsWith('[x]') then
      result := result + '.'+ loc.prop.Name.replace('[x]', '.ofType('+loc.value.fhirType+')')
    else
      result := result + '.'+ loc.prop.Name;
  end;
end;

procedure TFHIREditor.validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList);
var
  i : integer;
  s : String;
  t : QWord;
  path : TFslList<TFHIRLocatedNode>;
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
    FResource.Free;
    FResource := nil;
    try
      FResource := parseResource(FContent.text);
      path := FResource.findLocation(cursor);
      try
        inspection.AddPair('Version', FFactory.versionString);
        inspection.AddPair('Path', asPathExpression(path));
        if (path.count > 0) then
        begin
          inspection.AddPair('Type', path[path.count - 1].value.fhirType);
          if (path.count > 1) then
          begin
            if (path[path.count - 1].prop.IsList) then
              inspection.AddPair('Repeats', 'True')
            else
              inspection.AddPair('Repeats', 'False');
          end;
        end;
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
        validationError(TSourceLocation.CreateNull, 'Error Parsing Resource: '+e.message);
      end;
    end;
  finally
    finishValidating(validate, t);
  end;
end;

procedure TFHIREditor.saveStatus;
begin
  inherited saveStatus;
  if FDesigner <> nil then
    FDesigner.saveStatus;
end;

function TFHIREditor.hasDesigner: boolean;
begin
  Result := true;
end;

function TFHIREditor.frameFactory(rType : String) : TResourceDesignerFrame;
begin
  if rType = 'Patient' then
    result := TPatientFrame.create(FDesignerPanelWork)
  else
    result := TResourceTreeFrame.create(FDesignerPanelWork);
end;


procedure TFHIREditor.makeDesigner;
begin
  inherited makeDesigner;
  FSync := TFHIRSynEditSynchroniser.create;
  FSync.SynEdit := TextEditor;
  FSync.Factory := context.factory(fhirVersionRelease4);
  FSync.Format := FFormat;

  if (FResource <> nil) then
  begin
    FDesigner := frameFactory(FResource.fhirType);
    FDesigner.parent := FDesignerPanelWork;
    FDesigner.Align := alClient;
    FDesigner.context := Context.link;
    FDesigner.sync := FSync.link;
    FDesigner.initialize;
  end;
end;

procedure TFHIREditor.DoMnuPretty(sender: TObject);
var
  x : TMXmlDocument;
  j : TJsonObject;
begin
  if getCurrentFormat = ffXml then
  begin
    x := TMXmlParser.parse(TextEditor.Text, [xpDropWhitespace]);
    try
      SetContentUndoable(x.ToXml(true, false));
    finally
      x.Free;
    end;
  end
  else
  begin
    j := TJSONParser.Parse(TextEditor.text);
    try
      SetContentUndoable(TJsonWriter.writeObjectStr(j, true));
    finally
      j.Free;
    end;
  end;
end;

procedure TFHIREditor.DoMnuCondense(sender: TObject);
var
  x : TMXmlDocument;
  j : TJsonObject;
begin
  if getCurrentFormat = ffXml then
  begin
    x := TMXmlParser.parse(TextEditor.Text, [xpDropWhitespace]);
    try
      SetContentUndoable(x.ToXml(false, false));
    finally
      x.Free;
    end;
  end
  else
  begin
    j := TJSONParser.Parse(TextEditor.text);
    try
      SetContentUndoable(TJsonWriter.writeObjectStr(j, false));
    finally
      j.Free;
    end;
  end;
end;

procedure TFHIREditor.DoMnuSwitch(sender: TObject);
var
  res : TFHIRResourceV;
  p : TFHIRParser;
  c : TFHIRComposer;
  tgt : TFHIRFormat;
begin
  if getCurrentFormat = ffJson then
    tgt := ffXml
  else
    tgt := ffJson;

  p := FFactory.makeParser(nil, getCurrentFormat, THTTPLanguages.create('en'));
  try
    c := FFactory.makeComposer(nil, tgt, THTTPLanguages.create('en'), OutputStylePretty);
    try
      res := p.parseResource(TextEditor.text);
      try
        SetContentUndoable(c.Compose(res));
      finally
        res.free;
      end;
    finally
      c.Free;
    end;
  finally
    p.free;
  end;
  FFormat := tgt;
end;

function TFHIREditor.getCurrentFormat: TFHIRFormat;
begin
  if TextEditor.Text.trim.StartsWith('{') then
    result := ffJson
  else
    result := ffXml;
end;

procedure TFHIREditor.updateDesigner;
begin
  FResource.Free;
  FResource := nil;
  FSync.Format := getCurrentFormat;
  FSync.load;
  FResource := FSync.Resource.link;
  if (FDesigner = nil) then
  begin
    FDesigner := frameFactory(FResource.fhirType);
    FDesigner.parent := FDesignerPanelWork;
    FDesigner.Align := alClient;
    FDesigner.OnSelectSourceRange := doSelectSourceRange;
    FDesigner.context := Context.link;
    FDesigner.sync := FSync.link;
    FDesigner.initialize;
  end;
  FDesigner.Client := Store.clientForAddress(Session.Address).Link;
  FDesigner.resource := FResource.link;
end;


procedure TFHIREditor.doSelectSourceRange(sender : TObject; start, stop : TPoint);
begin
  TextEditor.SelStart := TextEditor.RowColToCharIndex(start);
  TextEditor.SelEnd := TextEditor.RowColToCharIndex(stop);
end;

end.

