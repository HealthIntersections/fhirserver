unit fhir4_questionnaire2;

{
Copyright (c) 2017+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS 'AS IS' AND
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
  SysUtils,
  fsl_utilities,
  fsl_base,
  fhir_htmlgen,
  fhir_objects, FHIR.Version.Client,
  fhir4_types, fhir4_resources, fhir4_utilities, fhir4_factory;

type
  TQuestionnaireRenderer = class (TFslObject)
  private
    FTerminologyServer: TFHIRClient;
    FQuestionnaire: TFHIRQuestionnaire;
    procedure SetQuestionnaire(const Value: TFHIRQuestionnaire);
    procedure SetTerminologyServer(const Value: TFHIRClient);

    function prefix(item : TFHIRQuestionnaireItem) : string;
    procedure generateItem(html : THtmlPublisher; level : integer; item : TFHIRQuestionnaireItem);
    procedure header(html : THtmlPublisher);
    procedure footer(html : THtmlPublisher);
    procedure generate(html : THtmlPublisher);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;

    property questionnaire : TFHIRQuestionnaire read FQuestionnaire write SetQuestionnaire;
    property TerminologyServer : TFHIRClient read FTerminologyServer write SetTerminologyServer;

    function render : String;
  end;

implementation

{ TQuestionnaireRenderer }

destructor TQuestionnaireRenderer.Destroy;
begin
  FQuestionnaire.free;
  FTerminologyServer.Free;

  inherited;
end;

procedure TQuestionnaireRenderer.footer(html: THtmlPublisher);
begin
  html.EndForm;
end;

procedure TQuestionnaireRenderer.generate(html: THtmlPublisher);
var
  item : TFHIRQuestionnaireItem;
begin
  header(html);
  for item in questionnaire.itemList do
    generateItem(html, 1, item);
  footer(html);
end;

type
  TValueType = (vtBoolean, vtDecimal, vtInteger, vtDate, vtDateTime, vtTime, vtString, vtUrl, vtQuantity);

procedure TQuestionnaireRenderer.generateItem(html: THtmlPublisher; level : integer; item: TFHIRQuestionnaireItem);
  function value(vt : TValueType) : String;
  begin
    if item.initial = nil then
      exit('');
    case vt of
      vtBoolean: result := item.initial.primitiveValue;
      vtDecimal: result := item.initial.primitiveValue;
      vtInteger: result := item.initial.primitiveValue;
      vtDate: result := item.initial.primitiveValue;
      vtDateTime: result := item.initial.primitiveValue;
      vtTime: result := item.initial.primitiveValue;
      vtString: result := item.initial.primitiveValue;
      vtUrl: result := item.initial.primitiveValue;
      vtQuantity: result := (item.initial as TFhirQuantity).value+' '+(item.initial as TFhirQuantity).unit_;
    end;
  end;
var
  child : TFHIRQuestionnaireItem;
begin
  html.startDiv;
  case item.type_ of
    ItemTypeGroup : html.Heading(level, prefix(item)+item.text);
    ItemTypeDisplay : html.Heading(level, prefix(item)+item.text);
    ItemTypeBoolean : html.checkbox(item.linkId, value(vtBoolean) = 'true', item.text);
    ItemTypeDecimal : html.TextInput(item.linkId, value(vtDecimal), item.text);
    ItemTypeInteger : html.TextInput(item.linkId, value(vtInteger), item.text);
    ItemTypeDate : html.TextInput(item.linkId, value(vtDate), item.text);
    ItemTypeDateTime : html.TextInput(item.linkId, value(vtDateTime), item.text);
    ItemTypeTime : html.TextInput(item.linkId, value(vtTime), item.text);
    ItemTypeString : html.TextInput(item.linkId, value(vtString), item.text);
    ItemTypeText : html.memo(item.linkId, value(vtString), item.text);
    ItemTypeUrl : html.TextInput(item.linkId, value(vtUrl), item.text);
//    ItemTypeChoice : html.TextInput(item.linkId, value(item, TFhirType), item.text);
//    ItemTypeOpenChoice : html.TextInput(item.linkId, value(item, TFhirDecimal), item.text);
//    ItemTypeAttachment : html.TextInput(item.linkId, value(item, TFhirDecimal), item.text);
    ItemTypeReference : html.TextInput(item.linkId, '', item.text);
    ItemTypeQuantity : html.TextInput(item.linkId, value(vtQuantity), item.text);
  end;
  if item.itemList.Count > 0 then
  begin
    html.StartBlockQuote;
    for child in item.itemList do
      generateItem(html, level+1, child);
    html.EndBlockQuote;
  end;
  html.endDiv;
end;

procedure TQuestionnaireRenderer.header(html: THtmlPublisher);
begin
  html.AddTitle('Questionnaire rendered '+TFslDateTime.makeLocal.toString('c'));
  html.StartForm('POST', '/');
end;

function TQuestionnaireRenderer.prefix(item: TFHIRQuestionnaireItem): string;
begin
  if item.prefix = '' then
    result := ''
  else
    result := item.prefix+' ';
end;

function TQuestionnaireRenderer.render: String;
var
  html : THtmlPublisher;
begin
  html := THtmlPublisher.create(TFHIRFactoryR4.create);
  try
    generate(html);
    result := html.output;
  finally
    html.free;
  end;
end;

procedure TQuestionnaireRenderer.SetQuestionnaire(const Value: TFHIRQuestionnaire);
begin
  FQuestionnaire.free;
  FQuestionnaire := Value;
end;

procedure TQuestionnaireRenderer.SetTerminologyServer(const Value: TFHIRClient);
begin
  FTerminologyServer.Free;
  FTerminologyServer := Value;
end;

function TQuestionnaireRenderer.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FTerminologyServer.sizeInBytes);
  inc(result, FQuestionnaire.sizeInBytes);
end;

end.
