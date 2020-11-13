unit bundlebuilder;

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


interface

uses
  SysUtils,
  fsl_base, fsl_utilities,
  fhir_objects, fhir_common, fhir_factory;

type
  TFHIRBundleBuilder = class (TFslObject)
  private
    FHasSecureOp: boolean;
  protected
    FBundle : TFHIRBundleW;
    FFactory : TFHIRFactory;
  public
    constructor Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
    destructor Destroy; override;

    Property hasSecureOp : boolean read FHasSecureOp write FHasSecureOp;

    procedure setId(id : string);
    procedure setLastUpdated(dt : TFslDateTime);
    procedure setTotal(t : integer);
    procedure tag(n, v : String);
    procedure addLink(rt, url : String);
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); virtual; abstract;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; virtual; abstract;
    function getBundle : TFHIRResourceV; virtual; abstract;
    function makeEntry : TFhirBundleEntryW; virtual;
  end;

  TFHIRBundleBuilderSimple = class (TFHIRBundleBuilder)
  public
    procedure addEntry(entry : TFhirBundleEntryW; first : boolean); override;
    function moveToFirst(res : TFhirResourceV) : TFhirBundleEntryW; override;
    function getBundle : TFHIRResourceV; override;
  end;


implementation


{ TFHIRBundleBuilder }

constructor TFHIRBundleBuilder.Create(factory : TFHIRFactory; bundle : TFHIRBundleW);
begin
  inherited Create;
  FBundle := bundle;
  FFactory := factory;
end;

destructor TFHIRBundleBuilder.Destroy;
begin
  FBundle.Free;
  FFactory.Free;
  inherited;
end;

procedure TFHIRBundleBuilder.addLink(rt, url: String);
begin
  FBundle.links[rt] := url;
end;

procedure TFHIRBundleBuilder.setId(id: string);
begin
  FBundle.id := id;
end;

procedure TFHIRBundleBuilder.setLastUpdated(dt: TFslDateTime);
begin
  FBundle.lastUpdated := dt;
end;

procedure TFHIRBundleBuilder.setTotal(t: integer);
begin
  FBundle.total := t;
end;

procedure TFHIRBundleBuilder.tag(n, v: String);
begin
  FBundle.Tags[n] := v;
end;

function TFHIRBundleBuilder.makeEntry: TFhirBundleEntryW;
begin
  result := Ffactory.wrapBundleEntry(Ffactory.makeByName('Bundle.entry'));
end;

{ TFHIRBundleBuilderSimple }

procedure TFHIRBundleBuilderSimple.addEntry(entry: TFhirBundleEntryW; first : boolean);
begin
  FBundle.addEntry(entry, first);
end;


function TFHIRBundleBuilderSimple.getBundle: TFHIRResourceV;
begin
  result := FBundle.Resource.Link;
end;

function TFHIRBundleBuilderSimple.moveToFirst(res: TFhirResourceV): TFhirBundleEntryW;
begin
  result := FBundle.MoveToFirst(res);
end;


end.
