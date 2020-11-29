Unit ftx_ucum_base;

{
Copyright (c) 2001+, Health Intersections Pty Ltd (http://www.healthintersections.com.au)
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

Interface

Uses
  Sysutils,
  fsl_utilities,
  fsl_base,
  
  fsl_collections;

Const Ucum_CACHE_VERSION = 3;

type
  TConceptKind  = (UcumNull, UcumPREFIX, UcumBASEUNIT, UcumUNIT);

  TUcumProperty = class (TFslName)
  private
    FCommonUnits : TFslStringList;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TUcumProperty; Overload;

    Property CommonUnits : TFslStringlist read FCommonUnits;
  end;

  TUcumConcept = class (TFslObject)
  private
    Fcode : String;
    FcodeUC : String;
    FprintSymbol : String;
    Fnames : TFslStringList;
    FText: String;
  protected
    Function GetKind : TConceptKind; virtual;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TUcumConcept; Overload;

    Property kind : TConceptKind read Getkind;
    Property code : String read Fcode write FCode;    // case sensitive code for this concept
    Property codeUC : String read FcodeUC write FcodeUC;    // case insensitive code for this concept
    Property printSymbol : String read FprintSymbol write FPrintSymbol;    // print symbol for this code
    Property names : TFslStringList read Fnames;    // names for the concept
    Property Text : String read FText write FText;
  End;

  TUcumPrefix = class (TUcumConcept)
  private
    Fvalue : TFslDecimal;
  protected
    Function GetKind : TConceptKind; Override;
    function sizeInBytesV : cardinal; override;
  public

    function Link : TUcumPrefix; Overload;
    Property value : TFslDecimal read Fvalue write FValue;  //value for the prefix - 1^-24 through to 1^24

    procedure SetPrecision(i : integer);
  End;

  TUcumUnit = class (TUcumConcept)
  private
    FProperty : string;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    function Link : TUcumUnit; Overload;
    Property PropertyType : string read FProperty write FProperty; // the kind of thing this represents
  End;

  TUcumBaseUnit = class (TUcumUnit)
  private
    Fdim : Char;
  protected
    Function GetKind : TConceptKind; Override;
    function sizeInBytesV : cardinal; override;
  public
    function Link : TUcumBaseUnit; Overload;
    Property dim : Char read FDim write FDim;
  End;

  TUcumValue = class (TFslObject)
  private
    Funit : String;
    FunitUC : String;
    Fvalue : TFslDecimal;
    Ftext : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    function Link : TUcumValue; Overload;
    Property unit_ : String read Funit write FUnit;
    Property unitUC : String read FunitUC write FUnitUC;
    Property value : TFslDecimal read FValue write FValue;
    Property text : String read Ftext write FText;

    procedure SetPrecision(i : integer);
  End;

  TUcumDefinedUnit = class (TUcumUnit)
  private
    Fmetric : boolean;
    FisSpecial : boolean;
    Fclass_ : String;
    Fvalue : TUcumValue;
  protected
    Function GetKind : TConceptKind; Override;
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;
    function Link : TUcumDefinedUnit; Overload;
    Property metric : boolean read Fmetric write FMetric; // whether this is a metric unit or not
    Property isSpecial : boolean read FisSpecial write FIsSpecial; // special means?
    Property class_ : String read Fclass_ write FClass_; // The class of this unit
    Property value : TUcumValue read Fvalue; // Value details
  End;

  TUcumModel = class (TFslObject)
  private
    FProperties : TFslMap<TUcumProperty>;
    Fprefixes : TFslList<TUcumPrefix>;
    FbaseUnits : TFslMap<TUcumBaseUnit>;
    FdefinedUnits : TFslMap<TUcumDefinedUnit>;
    FVersion : String;
    FRevisionDate : String;
  protected
    function sizeInBytesV : cardinal; override;
  public
    constructor Create; Override;
    destructor Destroy; Override;

    function Link : TUcumModel; Overload;

    Procedure clear;

    Function GetUnit(sCode : String) : TUcumUnit;

    Property prefixes : TFslList<TUcumPrefix> read Fprefixes;
    Property baseUnits : TFslMap<TUcumBaseUnit> read FbaseUnits;
    Property definedUnits : TFslMap<TUcumDefinedUnit> read FdefinedUnits;
    Property Version : String read FVersion write FVersion;
    Property RevisionDate : String read FRevisionDate write FRevisionDate;
    Property Properties : TFslMap<TUcumProperty> read FProperties;
  End;

const
  CODES_CONCEPT_KIND : array [TConceptKind] of string = ('null', 'PREFIX', 'BASEUNIT', 'UNIT');

implementation

{ TUcumConcept }

constructor TUcumConcept.Create;
begin
  inherited;
  Fnames := TFslStringList.Create;
end;

destructor TUcumConcept.Destroy;
begin
  FNames.Free;
  inherited;
end;

function TUcumConcept.GetKind: TConceptKind;
begin
  result := UcumNull;
end;

function TUcumConcept.Link: TUcumConcept;
begin
  result := TUcumConcept(Inherited Link);
end;


function TUcumConcept.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fcode.length * sizeof(char)) + 12);
  inc(result, (FcodeUC.length * sizeof(char)) + 12);
  inc(result, (FprintSymbol.length * sizeof(char)) + 12);
  inc(result, Fnames.sizeInBytes);
  inc(result, (FText.length * sizeof(char)) + 12);
end;

{ TUcumPrefix }

function TUcumPrefix.GetKind: TConceptKind;
begin
  result := UcumPREFIX;
end;

function TUcumPrefix.Link: TUcumPrefix;
begin
  result := TUcumPrefix(Inherited Link);
end;


procedure TUcumPrefix.SetPrecision(i: integer);
begin
  FValue.Precision := i;
end;

function TUcumPrefix.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TUcumUnit }

function TUcumUnit.Link: TUcumUnit;
begin
  result := TUcumUnit(Inherited Link);
end;

function TUcumUnit.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FProperty.length * sizeof(char)) + 12);
end;

{ TUcumBaseUnit }

function TUcumBaseUnit.GetKind: TConceptKind;
begin
  result := UcumBASEUNIT;
end;

function TUcumBaseUnit.Link: TUcumBaseUnit;
begin
  result := TUcumBaseUnit(Inherited Link);

end;

function TUcumBaseUnit.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

{ TUcumValue }

function TUcumValue.Link: TUcumValue;
begin
  result := TUcumValue(Inherited Link);
end;


procedure TUcumValue.SetPrecision(i: integer);
begin
  FValue.Precision  := i;
end;

function TUcumValue.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Funit.length * sizeof(char)) + 12);
  inc(result, (FunitUC.length * sizeof(char)) + 12);
  inc(result, (Ftext.length * sizeof(char)) + 12);
end;

{ TUcumDefinedUnit }

constructor TUcumDefinedUnit.Create;
begin
  inherited;
  Fvalue := TUcumValue.Create;
end;

destructor TUcumDefinedUnit.Destroy;
begin
  Fvalue.Free;
  inherited;
end;

function TUcumDefinedUnit.GetKind: TConceptKind;
begin
  result := UcumUNIT;
end;

function TUcumDefinedUnit.Link: TUcumDefinedUnit;
begin
  result := TUcumDefinedUnit(Inherited Link);
end;

function TUcumDefinedUnit.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (Fclass_.length * sizeof(char)) + 12);
  inc(result, Fvalue.sizeInBytes);
end;

{ TUcumModel }

procedure TUcumModel.clear;
begin
  Fprefixes.Clear;
  FbaseUnits.Clear;
  FdefinedUnits.Clear;
  FVersion := '';
  FRevisionDate := '';

end;

constructor TUcumModel.Create;
begin
  inherited;
  Fprefixes := TFslList<TUcumPrefix>.Create;
  FbaseUnits := TFslMap<TUcumBaseUnit>.Create('Ucum.base');
  FbaseUnits.defaultValue := nil;
  FdefinedUnits := TFslMap<TUcumDefinedUnit>.Create('Ucum.defined');
  FdefinedUnits.defaultValue := nil;
  FProperties := TFslMap<TUcumProperty>.Create('Ucum.properties');
end;

destructor TUcumModel.Destroy;
begin
  Fprefixes.Free;
  FbaseUnits.Free;
  FdefinedUnits.Free;
  FProperties.Free;
  inherited;
end;

function TUcumModel.GetUnit(sCode: String): TUcumUnit;
begin
  result := FbaseUnits[sCode];
  if result = nil Then
    result := FdefinedUnits[sCode];
end;

function TUcumModel.Link: TUcumModel;
begin
  result := TUcumModel(inherited Link);
end;

function TUcumModel.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FProperties.sizeInBytes);
  inc(result, Fprefixes.sizeInBytes);
  inc(result, FbaseUnits.sizeInBytes);
  inc(result, FdefinedUnits.sizeInBytes);
  inc(result, (FVersion.length * sizeof(char)) + 12);
  inc(result, (FRevisionDate.length * sizeof(char)) + 12);
end;

{ TUcumProperty }

constructor TUcumProperty.Create;
begin
  inherited;
  FCommonUnits := TFslStringList.Create;
end;

destructor TUcumProperty.Destroy;
begin
  FCommonUnits.Free;
  inherited;
end;

function TUcumProperty.Link: TUcumProperty;
begin
  result := TUcumProperty(Inherited Link);
end;

function TUcumProperty.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FCommonUnits.sizeInBytes);
end;

End.
