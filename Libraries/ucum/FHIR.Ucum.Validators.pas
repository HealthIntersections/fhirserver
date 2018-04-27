Unit FHIR.Ucum.Validators;

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
  FHIR.Ucum.Expressions,
  FHIR.Ucum.Handlers,
  AdvStringLists,
  AdvNames,
  AdvObjects;

Type
  TUcumValidator = class (TAdvObject)
  Private
    Fmodel : TUcumModel;
    Fresult : TAdvStringList;
    Fhandlers : TUcumRegistry;

    procedure checkCodes;
    procedure checkUnits;
    procedure checkUnitCode(code : String; primary : boolean);
  Public
    Constructor Create(oModel : TUcumModel; handlers : TUcumRegistry);
    Destructor Destroy; Override;

    Procedure validate(oList : TAdvStringList);
  End;

Implementation



Constructor TUcumValidator.Create(oModel : TUcumModel; handlers : TUcumRegistry);
Begin
  Inherited Create;
  FModel := oModel;
  FHandlers := Handlers;
End;

Destructor TUcumValidator.Destroy;
Begin
  FModel.Free;
  Fhandlers.Free;
  FResult.Free;
  Inherited;
End;

Procedure TUcumValidator.validate(oList :  TAdvStringList);
Begin
  Fresult := oList.Link;
  checkCodes();
  checkUnits();
End;

procedure TUcumValidator.checkCodes;
var
  i : Integer;
Begin
  for i := 0 to Fmodel.BaseUnits.Count - 1 Do
    checkUnitCode(FModel.BaseUnits[i].Code, true);

  for i := 0 to Fmodel.DefinedUnits.Count - 1 Do
    checkUnitCode(FModel.DefinedUnits[i].Code, true);
End;


procedure TUcumValidator.checkUnits;
var
  i : integer;
Begin
  for i := 0 to Fmodel.DefinedUnits.Count - 1 Do
    if not Fmodel.DefinedUnits[i].isSpecial Then
      checkUnitCode(Fmodel.DefinedUnits[i].Value.Unit_, false)
    else if not Fhandlers.ExistsByName(Fmodel.DefinedUnits[i].Code) Then
       Fresult.add('No Handler for '+Fmodel.DefinedUnits[i].Code);
End;


procedure TUcumValidator.checkUnitCode(code : String; primary : boolean);
var
  term  : TUcumTerm;
  oCan : TUcumCanonical;
  c : String;
  inBrack : boolean;
  nonDigits : boolean;
  i : integer;
  oConv : TUcumConverter;
Begin
  try
    term := TUcumExpressionParser.parse(Fmodel, code);
    try
      c := TUcumExpressionComposer.compose(term);
      if (c <> code) Then
        FResult.add('Round trip failed: '+code+' -> '+c);
      oConv := TUcumConverter.Create(Fmodel.Link, Fhandlers.Link);
      Try
        oCan := oConv.convert(term);
        Try
          // what? oCan.Unit_;
        Finally
          oCan.Free;
        End;
      Finally
        oConv.Free;
      End;
    Finally
      term.Free;
    End;
  except
    on e : exception do
      FResult.Add(Code+': '+e.Message);
  End;

  if (primary) Then
  try
    // there can't be any codes that have digits in them that aren't inside []
    inBrack := false;
    nonDigits := false;
    for i := 1 to length(code) Do
    begin
      if (code[i] = '[') Then
        if (inBrack) Then
          raise Exception.create('nested [')
	else
          inBrack := true;
      if (code[i] = ']') Then
        if (not inBrack) Then
          raise Exception.Create('] without [')
        else
          inBrack := false;
      nonDigits := nonDigits or not ((code[i] >= '0') and (code[i] <= '9'));
      if ((code[i] >= '0') and (code[i] <= '9')) And not inBrack and nonDigits Then
        raise Exception.Create('code '+code+' is ambiguous because  it has digits outside []');
    End;
  except
    on e : exception do
      FResult.Add(Code+': '+e.Message);
  End;
End;



End.