unit HelpContexts;

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

const
  Help_Strings : array of String = [
   {0}   '',
   {1}   'ui: A List of servers that have previously been used',
   {2}   'ui: A list of files that have been previously been opened',
   {3}   'ui:View/Edit the server''s CapabilityStatement',
   {4}   'ui: Special for the San Diego Connectathon',
   {5}   'search:Patient.name',
   {6}   'search:Patient.birthdate',
   {7}   'search:Patient.deceased',
   {8}   'search:Patient.death-date',
   {9}   'search:Patient.gender',
   {10}  'search:Patient.telecom',
   {11}  'search:Patient.identifier',
   {12}  'search:Patient.active',
   {13}  'search:Patient.animal-species',
   {14}  'search:Patient.language',
   {14}  'search:Patient._text',
   {15}  'search:Patient._tag',

   {16}  'fhir:ValueSet.url',
   {17}  'fhir:ValueSet.identifier',
   {18}  'fhir:ValueSet.version',
   {19}  'fhir:ValueSet.name',
   {20}  'fhir:ValueSet.title',
   {21}  'fhir:ValueSet.status',
   {22}  'fhir:ValueSet.experimental',
   {23}  'fhir:ValueSet.date',
   {24}  'fhir:ValueSet.publisher',
   {25}  'fhir:ValueSet.contact',
   {26}  'fhir:ValueSet.description',
   {27}  'fhir:ValueSet.useContext',
   {28}  'fhir:ValueSet.jurisdiction',
   {29}  'fhir:ValueSet.immutable',
   {30}  'fhir:ValueSet.purpose',
   {31}  'fhir:ValueSet.copyright',
   {32}  'fhir:ValueSet.extensible',
   {38}  'fhir:ValueSet.compose',
   {33}  'fhir:ValueSet.compose.lockedDate',
   {34}  'fhir:ValueSet.compose.inactive',
   {35}  'fhir:ValueSet.compose.include.system',
   {36}  'fhir:ValueSet.compose.include.version',
   {37}  'fhir:ValueSet.compose.include.concept.code,fhir:ValueSet.compose.include.concept.display',
   {39}  'fhir:ValueSet.compose.include.concept.designation,fhir:ValueSet.compose.include.concept.designation.language,fhir:ValueSet.compose.include.concept.designation.use,fhir:ValueSet.compose.include.concept.designation.value',
   {43}  'fhir:ValueSet.compose.include.filter.property,fhir:ValueSet.compose.include.filter.op,fhir:ValueSet.compose.include.filter.value',
   {44}  'fhir:ValueSet.expansion',
   {46}  'fhir:ValueSet.expansion.identifier',
   {47}  'fhir:ValueSet.expansion.timestamp',
   {48}  'fhir:ValueSet.expansion.total',
   {49}  'fhir:ValueSet.expansion.offset',
   {40}  'fhir:ValueSet.expansion.parameter',
   {52}  'fhir:ValueSet.expansion.contains,fhir:ValueSet.expansion.contains.abstract,fhir:ValueSet.expansion.contains.inactive',
   {58}  'fhir:ValueSet.expansion.contains.designation',
   {60}  'fhir:ValueSet.compose.inactive',
   {61}  'ui:Ask the server to expand the value set given it''s current definition',
   
   {end} ''];

implementation

end.
