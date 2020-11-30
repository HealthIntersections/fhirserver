Unit LOINC;

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

Interface

Uses 
  
  AdvPersistentLists;

Type
  TLOINCStrucure = class (TFslObject)

  End;

  TLOINCCode = class (TFslName)
  private
    FDisplayName: String;
  protected
    function sizeInBytesV : cardinal; override;
  Public
    Procedure Define(oFiler : TFslFiler); Override;
    Procedure Assign(oSource : TFslObject); Override;
    Function Link :TLOINCCode; Overload;
    Function Clone :TLOINCCode; Overload;

    Property DisplayName : String read FDisplayName write FDisplayName;
  End;

  TLOINCList = class (TFslNameList)
  private
    function GetCode(iIndex: integer): TLOINCCode;
  Protected
      Function ItemClass : TFslObjectClass; Override;
  Public
    Function GetByName(Const sName : String) : TLOINCCode; Overload;

    Property Code[iIndex : integer] : TLOINCCode read GetCode; default;
  End;

implementation

{ TLOINCCode }

procedure TLOINCCode.Assign(oSource: TFslObject);
begin
  inherited;
  FDisplayName := TLOINCCode(oSource).FDisplayName;
end;

function TLOINCCode.Clone: TLOINCCode;
begin
  result := TLOINCCode(Inherited Clone);
end;

procedure TLOINCCode.Define(oFiler: TFslFiler);
begin
  inherited;
  oFiler['DisplayMame'].DefineString(FDisplayName);
end;

function TLOINCCode.Link: TLOINCCode;
begin
  result := TLOINCCode(Inherited Link);
end;

function TLOINCCode.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, (FDisplayName.length * sizeof(char)) + 12);
end;

{ TLOINCList }

function TLOINCList.GetByName(const sName: String): TLOINCCode;
begin
  result := TLOINCCode(Inherited GetByName(sname));
end;

function TLOINCList.GetCode(iIndex: integer): TLOINCCode;
begin
  result := TLOINCCode(Inherited ObjectByIndex[iIndex]);
end;

function TLOINCList.ItemClass: TFslObjectClass;
begin
  result := TLOINCCode;
end;

End.
