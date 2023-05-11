unit FHIRStringEdit;

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


interface

uses
  System.SysUtils, System.Classes, FMX.Types, FMX.Controls,
  FMX.Controls.Presentation, FMX.Edit,
  fhir4_types;

type
  TFHIRStringEdit = class(TEdit)
  private
    fFHIRString: tFHIRString;
    fOnChange : TNotifyEvent;
    procedure MyOnChange(Sender : TObject);
  protected
    function GeTFHIRStringValue: String;
    procedure SeTFHIRStringValue(AValue: string);
    function GeTFHIRString: TFHIRString;
    procedure SeTFHIRString(AValue: TFHIRstring);
  public
    constructor Create(AOwner: TComponent); override;
    property FHIRPropertyValue: String read GeTFHIRStringValue write seTFHIRStringValue;
    property FHIRProperty: TFHIRString read GeTFHIRString write seTFHIRString;
    procedure load;
    function associate(AValue: TFHIRstring): TFHIRString;
    procedure useObject(var AValue: TFHIRstring);
  end;



implementation

constructor TFHIRStringEdit.Create(AOwner: TComponent);
begin
     inherited Create(AOwner);
     Inherited OnChange := MyOnChange;
end;

procedure TFHIRStringEdit.MyOnChange(Sender : TObject);
begin
  if fFHIRString <> nil then
    fFHIRstring.value := text;
end;

procedure TFHIRStringEdit.load;
begin
  if fFHIRString<>nil then
  text := fFHIRString.value;
end;



function TFHIRStringEdit.GeTFHIRStringValue: string;
begin
  if fFHIRString <> nil then
  begin
    result := fFHIRString.Value;
  end
  else
  begin
    result := 'No external object attached';
  end;
end;

procedure TFHIRStringEdit.SeTFHIRStringValue(AValue: String);
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
  end;
  fFHIRString.Value := AValue;
end;

function TFHIRStringEdit.GeTFHIRString: TFHIRString;
begin
  result := fFHIRString;
  if fFHIRString <> nil then
    text := fFHIRString.value;
end;

procedure TFHIRStringEdit.SeTFHIRString(AValue: TFHIRString);
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
    text := '';
  end
  else begin
    fFHIRString := AValue;
    text := fFHIRString.value;
  end;

end;

function TFHIRStringEdit.associate(AValue: TFHIRString): TFHIRString;
begin
  if fFHIRString = nil then
  begin
    fFHIRString := TFHIRString.Create;
    text := '';
  end
  else begin
    fFHIRString := AValue;
    text := fFHIRString.value;
  end;
  result := fFhirString;
end;


procedure TFHIRStringEdit.useObject(var AValue: TFHIRString);
begin
aValue := associate(aValue);
end;

end.
