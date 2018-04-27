Unit FHIR.Ucum.Handlers;

{
Copyright (c) 2001-2013, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Interface

Uses
  SysUtils,
  DecimalSupport,
  FHIR.Ucum.Base,
  AdvStringLists,
  AdvNames,
  AdvObjects;

Type
  TUcumUnitHandler = class (TAdvName)
  private
    FUnits: String;
    FValue: TSmartDecimal;
    FCode: String;
  public
    Constructor Create(Code : String); Overload;

    Property Code : String read FCode;
    Property Units : String read FUnits;
    Property Value : TSmartDecimal read FValue;
  End;

  TUcumCelsiusHandler = class (TUcumUnitHandler)
  public
    Constructor Create; Override;
  End;

  TUcumFahrenheitHandler = class (TUcumUnitHandler)
  public
    Constructor Create; Override;
  End;

  TUcumHoldingHandler = class (TUcumUnitHandler)
  public
    Constructor Create(sCode, sUnits : String; oValue : integer = 1); Overload;
  End;

  TUcumRegistry = class (TAdvNameList)
  private
    function GetHandlerByCode(code: String): TUcumUnitHandler;
  public
    function Link : TUcumRegistry; Overload;
    procedure Register;
    property HandlerByCode[code : String] : TUcumUnitHandler read GetHandlerByCode;
  End;


Implementation

{ TUcumRegistry }

function TUcumRegistry.GetHandlerByCode(code: String): TUcumUnitHandler;
begin
  result := GetByName(code) as TUcumUnitHandler;
end;

function TUcumRegistry.Link: TUcumRegistry;
begin
  result := TUcumRegistry(Inherited Link);
end;

procedure TUcumRegistry.Register;
begin
  Add(TUcumCelsiusHandler.Create());
  Add(TUcumFahrenheitHandler.Create());
  Add(TUcumHoldingHandler.Create('[p''diop]', 'deg'));
  Add(TUcumHoldingHandler.Create('%[slope]', 'deg'));
  Add(TUcumHoldingHandler.Create('[hp_X]', '1'));
  Add(TUcumHoldingHandler.Create('[hp_C]', '1'));
  Add(TUcumHoldingHandler.Create('[pH]', 'mol/l'));
  Add(TUcumHoldingHandler.Create('Np', '1'));
  Add(TUcumHoldingHandler.Create('B', '1'));
  Add(TUcumHoldingHandler.Create('B[SPL]', '10*-5.Pa', 2));
  Add(TUcumHoldingHandler.Create('B[V]', 'V'));
  Add(TUcumHoldingHandler.Create('B[10.nV]', 'V'));
  Add(TUcumHoldingHandler.Create('B[mV]', 'mV'));
  Add(TUcumHoldingHandler.Create('B[uV]', 'uV'));
  Add(TUcumHoldingHandler.Create('B[W]', 'W'));
  Add(TUcumHoldingHandler.Create('B[kW]', 'kW'));
  Add(TUcumHoldingHandler.Create('bit_s', '1'));
  Add(TUcumHoldingHandler.Create('[hp''_C]', '1'));
  Add(TUcumHoldingHandler.Create('[hp''_X]', '1'));
  Add(TUcumHoldingHandler.Create('[hp''_M]', '1'));
  Add(TUcumHoldingHandler.Create('[hp''_Q]', '1'));
  Add(TUcumHoldingHandler.Create('[degRe]', '1'));
  Add(TUcumHoldingHandler.Create('[m/s2/Hz^(1/2)]', '1'));
end;

{ TUcumUnitHandler }

constructor TUcumUnitHandler.Create(Code : String);
begin
  inherited create;
  Name := code;
  FCode := Code;
end;


{ TUcumCelsiusHandler }

constructor TUcumCelsiusHandler.Create;
begin
  inherited Create('Cel');
  FUnits := 'K';
  FValue := TSmartDecimal.valueOf(1);
end;

{ TUcumFahrenheitHandler }

constructor TUcumFahrenheitHandler.Create;
begin
  inherited Create('[degF]');
  FUnits := 'F';
  FValue := TSmartDecimal.valueOf('0.5555555555555555555555555555');
end;

{ TUcumHoldingHandler }

constructor TUcumHoldingHandler.Create(sCode, sUnits: String; oValue : integer);
begin
  inherited Create(sCode);
  FUnits := sUnits;
  FValue := TSmartDecimal.valueOf(oValue);
end;



End.
