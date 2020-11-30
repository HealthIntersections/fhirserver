unit v2_engine;
{
Copyright (c) 2011+, HL7 and Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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
  SysUtils, Classes,
  fsl_base, fsl_utilities,
  FHIR.Javascript, fsl_javascript, fhir_javascript,
  v2_message, v2_javascript,
  fhir_client, fhir_pathengine, fhir_factory;

type
  TV2ConversionEngineScript = class (TFslObject)
  private
    FName: String;
    FContent: String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    property name : String read FName write FName;
    property content : String read FContent write FContent;
  end;

  TV2ConversionEngine = class;
  TV2ConversionEngineGetScriptEvent = function (engine : TV2ConversionEngine; name : String) : TV2ConversionEngineScript of object;

  TV2ConversionEngine = class (TFslObject)
  private
    FMessage: TV2Message;
    FScriptName: String;
    FClient: TFhirClientV;
    FFactory: TFHIRFactory;
    FJSFactory: TRegisterFHIRTypes;
    FOnGetScript: TV2ConversionEngineGetScriptEvent;
    procedure SetClient(const Value: TFhirClientV);
    procedure SetMessage(const Value: TV2Message);
    procedure SetFactory(const Value: TFHIRFactory);
  protected
    function sizeInBytesV : cardinal; override;
  public
    destructor Destroy; override;
    function Link : TV2ConversionEngine; overload;

    property message : TV2Message read FMessage write SetMessage;
    property client : TFhirClientV read FClient write SetClient;
    property factory : TFHIRFactory read FFactory write SetFactory;
    property jsFactory : TRegisterFHIRTypes read FJSFactory write FJSFactory;
    property scriptName : String read FScriptName write FScriptName;
    property onGetScript : TV2ConversionEngineGetScriptEvent read FOnGetScript write FOnGetScript;

    procedure execute;
  end;

implementation

{ TV2ConversionEngine }

destructor TV2ConversionEngine.Destroy;
begin
  FClient.free;
  FMessage.Free;
  FFactory.Free;

  inherited;
end;

function TV2ConversionEngine.Link: TV2ConversionEngine;
begin
  result := TV2ConversionEngine(inherited Link);
end;

procedure TV2ConversionEngine.SetClient(const Value: TFhirClientV);
begin
  FClient.free;
  FClient := Value;
end;

procedure TV2ConversionEngine.SetFactory(const Value: TFHIRFactory);
begin
  FFactory.Free;
  FFactory := Value;
end;

procedure TV2ConversionEngine.SetMessage(const Value: TV2Message);
begin
  FMessage.Free;
  FMessage := Value;
end;

procedure TV2ConversionEngine.execute;
var
  js : TFHIRJavascript;
  fpe : TFHIRPathEngineV;
  script : TV2ConversionEngineScript;
begin
  fpe := factory.makePathEngine(client.Worker.link, nil);
  try
    fpe.registerExtension(TV2FHIRPathExtensions.create);
    js := TFHIRJavascript.Create;
    try
//      js.registerFactory();
      TV2JavascriptHelper.registerv2Objects(js, fpe);
      script := FOnGetScript(self, scriptName);
      try
        js.executeObj(script.content, '', script.name, [message, client, factory]);
      finally
        script.Free;
      end;
    finally
      js.free;
    end;
  finally
    fpe.free;
  end;
end;

function TV2ConversionEngine.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FMessage.sizeInBytes);
  inc(result, (FScriptName.length * sizeof(char)) + 12);
  inc(result, FClient.sizeInBytes);
  inc(result, FFactory.sizeInBytes);
  inc(result, FJSFactory.sizeInBytes);
  inc(result, FOnGetScript.sizeInBytes);
end;

function TV2ConversionEngineScript.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FName.length * sizeof(char)) + 12);
  inc(result, (FContent.length * sizeof(char)) + 12);
end;

end.
