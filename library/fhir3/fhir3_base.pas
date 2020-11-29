unit fhir3_base;

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
  fsl_base,
  fhir_objects;

type
  TFHIRObject3 = class (TFHIRObject)
  public
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function JSType : String; override;
  end;

  TFHIRObjectX = TFHIRObject3;

  TFHIRResource3 = class (TFHIRResourceV)
  public
    function makeStringValue(v : String) : TFHIRObject; override;
    function makeCodeValue(v : String) : TFHIRObject; override;
    function makeIntValue(v : String) : TFHIRObject; override;
    function GetFhirObjectVersion: TFHIRVersion; override;
    function JSType : String; override;
  end;

  TFHIRResourceX = TFHIRResource3;

  // a set of named properties
  TFHIRTuple3 = class (TFHIRObject3)
  private
    FProperties : TFslMap<TFHIRSelectionList>;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function createPropertyValue(propName : string): TFHIRObject; override;
    function setProperty(propName : string; propValue : TFHIRObject) : TFHIRObject; override;
    function hasExtensions : boolean; override;
    function fhirType : String; override;
    function JSType : String; override;
    function getId : String; override;
    procedure setIdValue(id : String); override;
    procedure GetChildrenByName(name: string; list: TFHIRSelectionList); override;

    procedure addProperty(name : String; values : TFHIRSelectionList);
  end;
  TFHIRTuple = TFHIRTuple3;

implementation

uses
  fhir3_types;


{ TFHIRObject3 }

function TFHIRObject3.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRObject3.JSType: String;
begin
  result := fhirType+'3';
end;

function TFHIRObject3.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRObject3.makeIntValue(v: String): TFHIRObject;
begin
  result := TFHIRInteger.Create(v);
end;

function TFHIRObject3.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

{ TFHIRResource3 }

function TFHIRResource3.GetFhirObjectVersion: TFHIRVersion;
begin
  result := fhirVersionRelease3;
end;

function TFHIRResource3.JSType: String;
begin
  result := fhirType+'3';
end;

function TFHIRResource3.makeCodeValue(v: String): TFHIRObject;
begin
  result := TFhirCode.Create(v);
end;

function TFHIRResource3.makeIntValue(v: String): TFHIRObject;
begin
  result := TFHIRInteger.Create(v);
end;

function TFHIRResource3.makeStringValue(v: String): TFHIRObject;
begin
  result := TFhirString.Create(v);
end;

{ TFHIRTuple3 }

constructor TFHIRTuple3.Create;
begin
  inherited;
  FProperties := TFslMap<TFHIRSelectionList>.create('tuple');
end;

destructor TFHIRTuple3.Destroy;
begin
  FProperties.free;
  inherited;
end;

procedure TFHIRTuple3.addProperty(name: String; values: TFHIRSelectionList);
begin
  FProperties.Add(name, values);
end;

function TFHIRTuple3.createPropertyValue(propName: string): TFHIRObject;
begin
  raise EFHIRException.Create('Operation not supported on Tuple');
end;

function TFHIRTuple3.fhirType: String;
begin
  result := 'Tuple';
end;

procedure TFHIRTuple3.GetChildrenByName(name: string; list: TFHIRSelectionList);
begin
  if FProperties.ContainsKey(name) then
    list.addAll(FProperties[name]);
end;

function TFHIRTuple3.getId: String;
begin
  result := '';
end;

function TFHIRTuple3.hasExtensions: boolean;
begin
  result := false;
end;

function TFHIRTuple3.JSType: String;
begin
  result := fhirType+'3';
end;

procedure TFHIRTuple3.setIdValue(id: String);
begin
  raise EFHIRException.Create('Operation not supported on Tuple');
end;

function TFHIRTuple3.setProperty(propName: string; propValue: TFHIRObject) : TFHIRObject;
begin
  raise EFHIRException.Create('Operation not supported on Tuple');
end;

function TFHIRTuple3.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FProperties.sizeInBytes);
end;

end.
