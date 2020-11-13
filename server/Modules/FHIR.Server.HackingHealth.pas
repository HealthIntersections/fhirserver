unit FHIR.Server.HackingHealth;

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
interface

uses
  SysUtils,
  
  fhir3_resources, fhir3_utilities,
  cds_hooks_server;

type
  THackingHealthBNPLogic = class (TCDSHooksProcessor)
  private
//    function aTestIsBNP : boolean;
//    function clinicIsED : boolean;
//    function hasDyspnoea : boolean;
//    function isDyspnoea(cond : TFhirCondition) : boolean;
  public
    function execute : boolean; override;
  end;

implementation

{ THackingHealthBNPLogic }

//function THackingHealthBNPLogic.aTestIsBNP: boolean;
////var
////  res : TFHIRResource;
////  req : TFhirProcedureRequest;
//begin
//  result := false;
//  raise EFHIRException.create('to do');
//(*  for res in request.context do
//    if res is TFhirProcedureRequest then
//    begin
//      req := res as TFhirProcedureRequest;
//      if req.code.text = 'BNP' then
//        exit(true);
//    end;*)
//end;
//
//function THackingHealthBNPLogic.clinicIsED: boolean;
////var
////  enc : TFhirEncounter;
//begin
//  raise EFHIRException.create('to do');
//(*  if not request.preFetch.ContainsKey('encounter') then
//    result := false
//  else
//  begin
//    enc := request.preFetch['encounter'].resource as TFhirEncounter;
//    result := (enc.class_ <> nil) and (enc.class_.code = 'emergency');
//  end;*)
//end;
//
function THackingHealthBNPLogic.execute: boolean;
begin
  result := false;
//  if (aTestIsBNP) then
//  begin
//    if not clinicIsED then
//      addCard('BNP will not be paid by Medicare because this is not an emergency presentation', '', 'warning', 'MBS rules', '')
//    else if not hasDyspnoea then
//      addCard('BNP will not be paid by Medicare if the patient does not have Dyspnoea', '', 'warning', 'MBS rules', '');
//  end;
end;

//function THackingHealthBNPLogic.hasDyspnoea: boolean;
////var
////  bnd : TFhirBundle;
////  be : TFhirBundleEntry;
//begin
//  raise EFHIRException.create('to do');
//(*  bnd := request.preFetch['problems'].resource as TFhirBundle;
//  result := false;
//  for be in bnd.entryList do
//    if (be.resource <> nil) and (be.resource is TFhirCondition) then
//      if isDyspnoea(be.resource as TFhirCondition) then
//        exit(true);*)
//end;
//
//function THackingHealthBNPLogic.isDyspnoea(cond: TFhirCondition): boolean;
//begin
//  result := cond.code.text = 'Dyspnoea';
//end;
//
end.
