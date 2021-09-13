unit ftk_editor_jwt;

{$i fhir.inc}

interface

uses
  Classes, SysUtils, Math,
  Graphics, Controls, StdCtrls, ExtCtrls, ComCtrls, Menus,
  SynEdit, SynEditHighlighter, SynEditTypes, SynHighlighterJson,
  fsl_base, fsl_xml, fsl_logging, fsl_stream, fsl_crypto, fsl_json, fsl_fetcher,
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
    procedure doTabResize(sender : TObject);
    function fetchJson(address : String) : TJsonObject;
  protected
    procedure ContentChanged; override;
    procedure makeTextTab; override;
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

procedure TJWTEditor.ContentChanged;
var
  jwt : TJWT;
  kid : String;
  jwks : TJWKList;
begin
  inherited;
  try
    jwt := TJWTUtils.decodeJWT(TextEditor.text.trim);
    try
      synHeader.Text := TEncoding.UTF8.GetString(jwt.headerBytes);
      synPayload.Text := TEncoding.UTF8.GetString(jwt.payloadBytes);
      kid := jwt.kid;
      try
        if kid = 'QvVUYmIjO-FjJ8DWRgL_G01SQhm5xO4lBryOrx656QY' then
          jwks := TJWKList.create(EXAMPLE_JWKS)
        else if edtPublicKey.text <> '' then
          jwks := TJWKList.create(fetchJson(edtPublicKey.text))
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
end;

procedure TJWTEditor.makeTextTab;
var
  panel, pnlSub : TPanel;
  lbl : TLabel;
begin
  inherited makeTextTab;
  pnlMain := TPanel.create(tab);
  pnlMain.parent := tab;
  pnlMain.align := alBottom;
  pnlMain.height := tab.Height - 100;
  pnlMain.caption := '';
  pnlMain.BevelOuter := bvNone;
  tab.OnResize := doTabResize;

  panel := TPanel.create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alTop;
  panel.height := 100;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 24;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Header';
  pnlSub.BevelOuter := bvNone;

  synHeader := TSynEdit.create(panel);
  synHeader.parent := panel;
  synHeader.align := alClient;
  synHeader.Font.Size := 10;
  synHeader.Highlighter := TSynJSonSyn.create(nil);
  //synHeader.OnChange := DoTextEditorChange;
  //synHeader.OnStatusChange := DoTextEditorStatusChange;
  //synHeader.PopupMenu := FEditorPopup;

  panel := TPanel.create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alBottom;
  panel.height := 110;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 24;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Signature';
  pnlSub.BevelOuter := bvNone;

  lblSig := TLabel.create(panel);
  lblSig.parent := panel;
  lblSig.top := 28;
  lblSig.left := 20;
  lblSig.Caption := 'The JWT has not been validated';

  lbl := TLabel.create(panel);
  lbl.parent := panel;
  lbl.top := 51;
  lbl.left := 20;
  lbl.Caption := 'Public Key';

  edtPublicKey := TEdit.create(panel);
  edtPublicKey.parent := panel;
  edtPublicKey.top := 48;
  edtPublicKey.left := 80;
  edtPublicKey.Width := panel.Width - 100;
  edtPublicKey.Anchors := [akTop, akLeft, akRight];
  edtPublicKey.TextHint := '(File/URL of JSON JWKS)';

  lbl := TLabel.create(panel);
  lbl.parent := panel;
  lbl.top := 77;
  lbl.left := 20;
  lbl.Caption := 'Private Key';

  edtPrivateKey := TEdit.create(panel);
  edtPrivateKey.parent := panel;
  edtPrivateKey.top := 74;
  edtPrivateKey.left := 80;
  edtPrivateKey.Width := panel.Width - 100;
  edtPrivateKey.Anchors := [akTop, akLeft, akRight];
  edtPrivateKey.TextHint := '(File/URL of JSON JWK that includes )';


  panel := TPanel.create(pnlMain);
  panel.parent := pnlMain;
  panel.align := alClient;
  panel.caption := '';
  panel.BevelOuter := bvNone;

  pnlSub := TPanel.create(panel);
  pnlSub.parent := panel;
  pnlSub.align := alTop;
  pnlSub.height := 24;
  pnlSub.Alignment := taLeftJustify;
  pnlSub.caption := '  Payload';
  pnlSub.BevelOuter := bvNone;

  synPayload := TSynEdit.create(panel);
  synPayload.parent := panel;
  synPayload.align := alClient;
  synPayload.Font.Size := 10;
  synPayload.Highlighter := TSynJSonSyn.create(nil);
  //synPayload.OnChange := DoTextEditorChange;
  //synPayload.OnStatusChange := DoTextEditorStatusChange;
  //synPayload.PopupMenu := FEditorPopup;

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
    jwk := TJWK.create(json.Link);
    try
      jwt := TJWT.create;
      try
        jwt.header := TJsonObject.create;
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

