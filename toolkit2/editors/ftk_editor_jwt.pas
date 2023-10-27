unit ftk_editor_jwt;

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
  Classes, SysUtils, Math,
  Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus, Forms, Dialogs,
  SynEdit, SynEditHighlighter, SynEditTypes, SynHighlighterJson,
  fsl_base, fsl_xml, fsl_logging, fsl_stream, fsl_crypto, fsl_json, fsl_fetcher,
  fhir_healthcard,
  ftk_context,
  ftk_editor_base;

type

  { TJWTEditor }

  TJWTEditor = class (TBaseEditor)
  private
    pnlMain : TPanel;
    synHeader : TSynEdit;
    synPayload : TSynEdit;
    edtPublicKey, edtPrivateKey : TEdit;
    lblSig : TLabel;
    btnUpdate, btnValidate, btnResource : TButton;
    FUpdating : boolean;
    procedure doTabResize(sender : TObject);
    function fetchJson(address : String) : TJsonObject;
    procedure DoContentChange(sender : TObject);
    procedure doRegenerate(sender : TObject);
    procedure doValidate(sender : TObject);
    function FindResource(json : TJsonNode) : TJsonObject;

    procedure doHeaderPretty(sender : TObject);
    procedure doHeaderDense(sender : TObject);
    procedure doPayloadPretty(sender : TObject);
    procedure doPayloadDense(sender : TObject);
    procedure doOpenResource(sender : TObject);
  protected
    procedure ContentChanged; override;
    procedure makeTextTab; override;
    function getFixedEncoding : TSourceEncoding; override;
    function preEditSource(src : String) : string; override;
  public
    procedure newContent(); override;
    function FileExtension : String; override;
    procedure validate(validate : boolean; inspect : boolean; cursor : TSourceLocation; inspection : TStringList); override;
  end;

implementation

const
  EXAMPLE_JWKS =
    '{'+
    '"kty": "EC",'+
    '"d": "LPjj9XcDoyLC9gly93oxYj4YmSV-92bcrEy9fj9WuvU",'+
    '"use": "sig",'+
    '"crv": "P-256",'+
    '"kid": "QvVUYmIjO-FjJ8DWRgL_G01SQhm5xO4lBryOrx656QY",'+
    '"x": "CaJ_Mgg7_2AeO30KwMeK_d6SjVW-pcKmxEWXhyxZrZA",'+
    '"y": "45HXkNYbguoJX6A0dKi5yY1sBIejGbROTaI58f91a6Y",'+
    '"alg": "ES256"'+
    '}';

{ TJWTEditor }

procedure TJWTEditor.doTabResize(sender: TObject);
begin
  pnlMain.height := tab.Height - 80;
end;

function TJWTEditor.fetchJson(address: String): TJsonObject;
begin
  if FileExists(address) then
    result := TJSONParser.ParseFile(address)
  else
    result := TInternetFetcher.fetchJson(address);
end;

procedure TJWTEditor.DoContentChange(sender: TObject);
var
  json, res : TJsonObject;
begin
  if not FUpdating then
  begin
    btnValidate.enabled := edtPublicKey.text <> '';
    btnUpdate.enabled := edtPrivateKey.text <> '';
  end;
  btnResource.enabled := false;
  try
    json := TJSONParser.Parse(synPayload.text);
    try
      btnResource.enabled := FindResource(json) <> nil;
    finally
      json.free;
    end;
  except
  end;
end;


procedure TJWTEditor.doRegenerate(sender: TObject);
var
  jwt : TJWT;
  jwk : TJWK;
  json : TJsonObject;
  s : String;
begin
  jwt := TJWT.Create;
  try
    try
       jwt.header := TJSONParser.Parse(synHeader.text);
    except
      on e : Exception do
      begin
        MessageDlg('Header is not valid: '+e.message, mtError, [mbok], 0);
        synHeader.SetFocus;
      end;
    end;
    try
       jwt.payload := TJSONParser.Parse(synPayload.text);
    except
      on e : Exception do
      begin
        MessageDlg('Payload is not valid: '+e.message, mtError, [mbok], 0);
        synPayload.SetFocus;
      end;
    end;
    json := fetchJson(edtPrivateKey.text);
    try
      jwk := TJWK.Create(json.link);
      try
        s := TJWTUtils.encodeJWT(jwt, jwk);
      finally
        jwk.free;
      end;
      TextEditor.Text := s;
      TextEditor.SelStart := 0;
      TextEditor.SelEnd := 0;
    finally
      json.free;
    end;
  finally
    jwt.free;
  end;
end;

procedure TJWTEditor.doValidate(sender: TObject);
begin
  ContentChanged;
end;

function TJWTEditor.FindResource(json: TJsonNode): TJsonObject;
var
  n : TJsonNode;
  o, t : TJsonObject;
  a : TJsonArray;
begin
  result := nil;
  if (json = nil) then
    exit;
  if (json.kind = jnkObject) then
  begin
    o := json as TJsonObject;
    if (o.has('resourceType')) then
      exit(o);
    for n in o.properties.Values do
    begin
      if n.kind = jnkObject then
      begin
        t := FindResource(n);
        if (t <> nil) then
          exit(t);
      end;
    end;
  end;
  if n.kind = jnkArray then
  begin
    a := n as TJsonArray;
    for n in a do
    begin
      t := FindResource(n);
      if (t <> nil) then
        exit(t);
    end;
  end;
end;

procedure TJWTEditor.doHeaderPretty(sender: TObject);
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(synHeader.text);
  try
    synHeader.text := TJSONWriter.writeObjectStr(json, true);
  finally
    json.free;
  end;
  DoContentChange(self);
end;

procedure TJWTEditor.doHeaderDense(sender: TObject);
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(synHeader.text);
  try
    synHeader.text := TJSONWriter.writeObjectStr(json, false);
  finally
    json.free;
  end;
  DoContentChange(self);
end;

procedure TJWTEditor.doPayloadPretty(sender: TObject);
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(synPayload.text);
  try
    synPayload.text := TJSONWriter.writeObjectStr(json, true);
  finally
    json.free;
  end;
  DoContentChange(self);
end;

procedure TJWTEditor.doPayloadDense(sender: TObject);
var
  json : TJsonObject;
begin
  json := TJSONParser.Parse(synPayload.text);
  try
    synPayload.text := TJSONWriter.writeObjectStr(json, false);
  finally
    json.free;
  end;
  DoContentChange(self);
end;

procedure TJWTEditor.doOpenResource(sender: TObject);
var
  json, res : TJsonObject;
  cnt : TBytes;
begin
  json := TJSONParser.Parse(synPayload.text);
  try
    res := FindResource(json);
    if (res <> nil) then
    begin
      cnt := TJSONWriter.WriteObject(res);
      context.OnOpenSource(self, cnt, sekFHIR);
    end;
  finally
    json.free;
  end;
end;

procedure TJWTEditor.ContentChanged;
var
  jwt : TJWT;
  kid : String;
  jwks : TJWKList;
begin
  inherited;
  FUpdating := true;
  try
    jwt := TJWTUtils.decodeJWT(TextEditor.text.trim);
    try
      synHeader.Text := TEncoding.UTF8.GetString(jwt.headerBytes);
      synPayload.Text := TEncoding.UTF8.GetString(jwt.payloadBytes);
      kid := jwt.kid;
      try
        if kid = 'QvVUYmIjO-FjJ8DWRgL_G01SQhm5xO4lBryOrx656QY' then
          jwks := TJWKList.Create(EXAMPLE_JWKS)
        else if edtPublicKey.text <> '' then
          jwks := TJWKList.Create(fetchJson(edtPublicKey.text))
        else
          jwks := nil;
        if jwks <> nil then
        begin
          try
            TJWTUtils.verifyJWT(jwt, jwks, false);
          finally
            jwks.free;
          end;
          if jwt.valid then
            lblSig.Caption := 'All Valid against key '+kid
          else
            lblSig.Caption := 'Signature checking failed: '+jwt.validationMessage;
        end
        else
          lblSig.Caption := 'Not validated due to lack of key information';
      except
        on e : Exception do
          lblSig.Caption := 'Error validating: '+e.message;
      end;
    finally
      jwt.free;
    end;
  except
    on e : Exception do
    begin
      synHeader.text := 'Unable to decode JWT: '+e.message;
      synPayload.text := '';
    end;
  end;
  FUpdating := false;
end;

procedure TJWTEditor.makeTextTab;
var
  panel, pnlSub : TPanel;
  lbl : TLabel;
  btn : TButton;
  splitter : TSplitter;
begin
  inherited makeTextTab;

  pnlMain := TPanel.Create(tab);
  pnlMain.parent := tab;
  pnlMain.align := alBottom;
  pnlMain.height := tab.Height - 100;
  pnlMain.caption := '';
  pnlMain.BevelOuter := bvNone;
  tab.OnResize := doTabResize;

  panel := TPanel.Create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alTop;
  panel.height := 140;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.Create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 30;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Header';
  pnlSub.BevelOuter := bvNone;

  btn := TButton.Create(pnlSub);
  btn.parent := pnlSub;
  btn.top := 2;
  btn.left := 100;
  btn.width := 50;
  btn.caption := 'Pretty';
  btn.Anchors := [akTop, akLeft];
  btn.OnClick := doHeaderPretty;

  btn := TButton.Create(pnlSub);
  btn.parent := pnlSub;
  btn.top := 2;
  btn.left := 160;
  btn.width := 50;
  btn.caption := 'Dense';
  btn.Anchors := [akTop, akLeft];
  btn.OnClick := doHeaderDense;

  synHeader := TSynEdit.Create(panel);
  synHeader.parent := panel;
  synHeader.align := alClient;
  synHeader.Font.Size := 10;
  synHeader.Highlighter := TSynJSonSyn.Create(nil);
  synHeader.OnChange := DoContentChange;
  //synHeader.OnStatusChange := DoTextEditorStatusChange;
  //synHeader.PopupMenu := FEditorPopup;


  panel := TPanel.Create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alBottom;
  panel.height := 110;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.Create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 24;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Signature';
  pnlSub.BevelOuter := bvNone;

  lblSig := TLabel.Create(panel);
  lblSig.parent := panel;
  lblSig.top := 28;
  lblSig.left := 20;
  lblSig.Caption := 'The JWT has not been validated';

  lbl := TLabel.Create(panel);
  lbl.parent := panel;
  lbl.top := 51;
  lbl.left := 20;
  lbl.Caption := 'Public Key';

  edtPublicKey := TEdit.Create(panel);
  edtPublicKey.parent := panel;
  edtPublicKey.top := 48;
  edtPublicKey.left := 80;
  edtPublicKey.Width := panel.Width - 180;
  edtPublicKey.Anchors := [akTop, akLeft, akRight];
  edtPublicKey.TextHint := '(File/URL of JSON JWKS)';
  edtPublicKey.OnChange := DoContentChange;

  btnValidate := TButton.Create(panel);
  btnValidate.parent := panel;
  btnValidate.top := 48;
  btnValidate.left := panel.Width - 90;
  btnValidate.width := 70;
  btnValidate.caption := 'Validate';
  btnValidate.Anchors := [akTop, akRight];
  btnValidate.OnClick := doValidate;
  btnValidate.enabled := false;

  lbl := TLabel.Create(panel);
  lbl.parent := panel;
  lbl.top := 77;
  lbl.left := 20;
  lbl.Caption := 'Private Key';

  edtPrivateKey := TEdit.Create(panel);
  edtPrivateKey.parent := panel;
  edtPrivateKey.top := 74;
  edtPrivateKey.left := 80;
  edtPrivateKey.Width := panel.Width - 180;
  edtPrivateKey.Anchors := [akTop, akLeft, akRight];
  edtPrivateKey.TextHint := '(File/URL of JSON JWK that includes )';
  edtPrivateKey.OnChange := DoContentChange;

  btnUpdate := TButton.Create(panel);
  btnUpdate.parent := panel;
  btnUpdate.top := 74;
  btnUpdate.left := panel.Width - 90;
  btnUpdate.width := 70;
  btnUpdate.caption := 'Update';
  btnUpdate.Anchors := [akTop, akRight];
  btnUpdate.OnClick := doRegenerate;
  btnUpdate.enabled := false;

  panel := TPanel.Create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alClient;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.Create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 30;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Payload';
  pnlSub.BevelOuter := bvNone;

  btn := TButton.Create(pnlSub);
  btn.parent := pnlSub;
  btn.top := 2;
  btn.left := 100;
  btn.width := 50;
  btn.caption := 'Pretty';
  btn.Anchors := [akTop, akLeft];
  btn.OnClick := doPayloadPretty;

  btn := TButton.Create(pnlSub);
  btn.parent := pnlSub;
  btn.top := 2;
  btn.left := 160;
  btn.width := 50;
  btn.caption := 'Dense';
  btn.Anchors := [akTop, akLeft];
  btn.OnClick := doPayloadDense;

  btnResource := TButton.Create(pnlSub);
  btnResource.parent := pnlSub;
  btnResource.top := 2;
  btnResource.left := 240;
  btnResource.width := 100;
  btnResource.caption := 'Open Resource';
  btnResource.Anchors := [akTop, akLeft];
  btnResource.OnClick := doOpenResource;


  synPayload := TSynEdit.Create(panel);
  synPayload.parent := panel;
  synPayload.align := alClient;
  synPayload.Font.Size := 10;
  synPayload.Highlighter := TSynJSonSyn.Create(nil);
  synPayload.OnChange := DoContentChange;
  //synPayload.OnStatusChange := DoTextEditorStatusChange;
  //synPayload.PopupMenu := FEditorPopup;
end;

function TJWTEditor.getFixedEncoding: TSourceEncoding;
begin
  Result := senASCII;
end;

function TJWTEditor.preEditSource(src: String): string;
begin
  if src.StartsWith('shc:/') then
    result := THealthcareCardUtilities.readQR(src)
  else
    Result := inherited preEditSource(src);
end;

procedure TJWTEditor.newContent();
var
  jwk :  TJWK;
  json : TJsonObject;
  jwt : TJWT;
begin
  Session.HasBOM := false;
  Session.EndOfLines := slCRLF;
  Session.Encoding := senASCII;

  json := TJSONParser.Parse(EXAMPLE_JWKS);
  try
    jwk := TJWK.Create(json.Link);
    try
      jwt := TJWT.Create;
      try
        jwt.header := TJsonObject.Create;
        jwt.payload := TJSONParser.Parse('{ "content" : "example JSON" }');
        TextEditor.Text := TJWTUtils.encodeJWT(jwt, jwt_es256, jwk, 'DEF');
      finally
        jwt.free;
      end;
    finally
      jwk.free;
    end;
  finally
    json.free;
  end;
  updateToolbarButtons;
  ContentChanged;
end;

function TJWTEditor.FileExtension: String;
begin
  result := 'jwt';
end;

procedure TJWTEditor.validate(validate: boolean; inspect: boolean; cursor: TSourceLocation; inspection: TStringList);
begin
  inherited validate(validate, inspect, cursor, inspection);
end;

end.

