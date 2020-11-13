unit fhir5_authmap;

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

{$I fhir5.inc}

uses
  fhir_common,
  fhir5_resources;

// categories for web login
// tcClinical, tcData, tcMeds, tcSchedule, tcAudit, tcDocuments, tcFinancial, tcOther

const
  RESOURCE_CATEGORY : array [TFHIRResourceType] of TTokenCategory =
    (
    tcOther, // frtNull
{$IFDEF FHIR_ACCOUNT}                            tcFinancial, {$ENDIF}
{$IFDEF FHIR_ACTIVITYDEFINITION}                 tcOther, {$ENDIF}
{$IFDEF FHIR_ADMINISTRABLEPRODUCTDEFINITION}     tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_ADVERSEEVENT}                       tcClinical, {$ENDIF}
{$IFDEF FHIR_ALLERGYINTOLERANCE}                 tcClinical, {$ENDIF}
{$IFDEF FHIR_APPOINTMENT}                        tcSchedule, {$ENDIF}
{$IFDEF FHIR_APPOINTMENTRESPONSE}                tcSchedule, {$ENDIF}
{$IFDEF FHIR_AUDITEVENT}                         tcAudit, {$ENDIF}
{$IFDEF FHIR_BASIC}                              tcClinical, {$ENDIF}
{$IFDEF FHIR_BINARY}                             tcDocuments, {$ENDIF}
{$IFDEF FHIR_BIOLOGICALLYDERIVEDPRODUCT}         tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_BODYSTRUCTURE}                      tcClinical, {$ENDIF}
{$IFDEF FHIR_BUNDLE}                             tcDocuments, {$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT}                tcOther, {$ENDIF}
{$IFDEF FHIR_CAPABILITYSTATEMENT2}               tcOther, {$ENDIF}
{$IFDEF FHIR_CAREPLAN}                           tcClinical, {$ENDIF}
{$IFDEF FHIR_CARETEAM}                           tcClinical, {$ENDIF}
{$IFDEF FHIR_CATALOGENTRY}                       tcOther, {$ENDIF}
{$IFDEF FHIR_CHARGEITEM}                         tcFinancial, {$ENDIF}
{$IFDEF FHIR_CHARGEITEMDEFINITION}               tcFinancial, {$ENDIF}
{$IFDEF FHIR_CITATION}                           tcData, {$ENDIF}
{$IFDEF FHIR_CLAIM}                              tcFinancial, {$ENDIF}
{$IFDEF FHIR_CLAIMRESPONSE}                      tcFinancial, {$ENDIF}
{$IFDEF FHIR_CLINICALIMPRESSION}                 tcClinical, {$ENDIF}
{$IFDEF FHIR_CLINICALUSEISSUE}                   tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_CODESYSTEM}                         tcOther, {$ENDIF}
{$IFDEF FHIR_COMMUNICATION}                      tcDocuments, {$ENDIF}
{$IFDEF FHIR_COMMUNICATIONREQUEST}               tcDocuments, {$ENDIF}
{$IFDEF FHIR_COMPARTMENTDEFINITION}              tcOther, {$ENDIF}
{$IFDEF FHIR_COMPOSITION}                        tcDocuments, {$ENDIF}
{$IFDEF FHIR_CONCEPTMAP}                         tcOther, {$ENDIF}
{$IFDEF FHIR_CONDITION}                          tcClinical, {$ENDIF}
{$IFDEF FHIR_CONDITIONDEFINITION}                tcOther, {$ENDIF}
{$IFDEF FHIR_CONSENT}                            tcData, {$ENDIF}
{$IFDEF FHIR_CONTRACT}                           tcDocuments, {$ENDIF}
{$IFDEF FHIR_COVERAGE}                           tcFinancial, {$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYREQUEST}         tcFinancial, {$ENDIF}
{$IFDEF FHIR_COVERAGEELIGIBILITYRESPONSE}        tcFinancial, {$ENDIF}
{$IFDEF FHIR_DETECTEDISSUE}                      tcClinical, {$ENDIF}
{$IFDEF FHIR_DEVICE}                             tcData, {$ENDIF}
{$IFDEF FHIR_DEVICEDEFINITION}                   tcOther, {$ENDIF}
{$IFDEF FHIR_DEVICEMETRIC}                       tcData, {$ENDIF}
{$IFDEF FHIR_DEVICEREQUEST}                      tcClinical, {$ENDIF}
{$IFDEF FHIR_DEVICEUSESTATEMENT}                 tcClinical, {$ENDIF}
{$IFDEF FHIR_DIAGNOSTICREPORT}                   tcClinical, {$ENDIF}
{$IFDEF FHIR_DOCUMENTMANIFEST}                   tcDocuments, {$ENDIF}
{$IFDEF FHIR_DOCUMENTREFERENCE}                  tcDocuments, {$ENDIF}
{$IFDEF FHIR_ENCOUNTER}                          tcSchedule, {$ENDIF}
{$IFDEF FHIR_ENDPOINT}                           tcData, {$ENDIF}
{$IFDEF FHIR_ENROLLMENTREQUEST}                  tcFinancial, {$ENDIF}
{$IFDEF FHIR_ENROLLMENTRESPONSE}                 tcFinancial, {$ENDIF}
{$IFDEF FHIR_EPISODEOFCARE}                      tcSchedule, {$ENDIF}
{$IFDEF FHIR_EVENTDEFINITION}                    tcOther, {$ENDIF}
{$IFDEF FHIR_EVIDENCE}                           tcOther, {$ENDIF}
{$IFDEF FHIR_EVIDENCEREPORT}                     tcOther, {$ENDIF}
{$IFDEF FHIR_EVIDENCEVARIABLE}                   tcOther, {$ENDIF}
{$IFDEF FHIR_EXAMPLESCENARIO}                    tcOther, {$ENDIF}
{$IFDEF FHIR_EXPLANATIONOFBENEFIT}               tcFinancial, {$ENDIF}
{$IFDEF FHIR_FAMILYMEMBERHISTORY}                tcClinical, {$ENDIF}
{$IFDEF FHIR_FLAG}                               tcClinical, {$ENDIF}
{$IFDEF FHIR_GOAL}                               tcClinical, {$ENDIF}
{$IFDEF FHIR_GRAPHDEFINITION}                    tcOther, {$ENDIF}
{$IFDEF FHIR_GROUP}                              tcData, {$ENDIF}
{$IFDEF FHIR_GUIDANCERESPONSE}                   tcClinical, {$ENDIF}
{$IFDEF FHIR_HEALTHCARESERVICE}                  tcData, {$ENDIF}
{$IFDEF FHIR_IMAGINGSTUDY}                       tcClinical, {$ENDIF}
{$IFDEF FHIR_IMMUNIZATION}                       tcClinical, {$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONEVALUATION}             tcClinical, {$ENDIF}
{$IFDEF FHIR_IMMUNIZATIONRECOMMENDATION}         tcClinical, {$ENDIF}
{$IFDEF FHIR_IMPLEMENTATIONGUIDE}                tcOther, {$ENDIF}
{$IFDEF FHIR_INGREDIENT}                         tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_INSURANCEPLAN}                      tcFinancial, {$ENDIF}
{$IFDEF FHIR_INVOICE}                            tcFinancial, {$ENDIF}
{$IFDEF FHIR_LIBRARY}                            tcOther, {$ENDIF}
{$IFDEF FHIR_LINKAGE}                            tcData, {$ENDIF}
{$IFDEF FHIR_LIST}                               tcDocuments, {$ENDIF}
{$IFDEF FHIR_LOCATION}                           tcData, {$ENDIF}
{$IFDEF FHIR_MANUFACTUREDITEMDEFINITION}         tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_MEASURE}                            tcOther, {$ENDIF}
{$IFDEF FHIR_MEASUREREPORT}                      tcData, {$ENDIF}
{$IFDEF FHIR_MEDICATION}                         tcMeds, {$ENDIF}
{$IFDEF FHIR_MEDICATIONADMINISTRATION}           tcMeds, {$ENDIF}
{$IFDEF FHIR_MEDICATIONDISPENSE}                 tcMeds, {$ENDIF}
{$IFDEF FHIR_MEDICATIONKNOWLEDGE}                tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_MEDICATIONREQUEST}                  tcMeds, {$ENDIF}
{$IFDEF FHIR_MEDICATIONUSAGE}                    tcMeds, {$ENDIF}
{$IFDEF FHIR_MEDICINALPRODUCTDEFINITION}         tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_MESSAGEDEFINITION}                  tcOther, {$ENDIF}
{$IFDEF FHIR_MESSAGEHEADER}                      tcData, {$ENDIF}
{$IFDEF FHIR_MOLECULARSEQUENCE}                  tcClinical, {$ENDIF}
{$IFDEF FHIR_NAMINGSYSTEM}                       tcOther, {$ENDIF}
{$IFDEF FHIR_NUTRITIONINTAKE}                    tcClinical, {$ENDIF}
{$IFDEF FHIR_NUTRITIONORDER}                     tcClinical, {$ENDIF}
{$IFDEF FHIR_NUTRITIONPRODUCT}                   tcOther, {$ENDIF}
{$IFDEF FHIR_OBSERVATION}                        tcClinical, {$ENDIF}
{$IFDEF FHIR_OBSERVATIONDEFINITION}              tcOther, {$ENDIF}
{$IFDEF FHIR_OPERATIONDEFINITION}                tcOther, {$ENDIF}
{$IFDEF FHIR_OPERATIONOUTCOME}                   tcData, {$ENDIF}
{$IFDEF FHIR_ORGANIZATION}                       tcData, {$ENDIF}
{$IFDEF FHIR_ORGANIZATIONAFFILIATION}            tcData, {$ENDIF}
{$IFDEF FHIR_PACKAGEDPRODUCTDEFINITION}          tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_PARAMETERS}                         tcData, {$ENDIF}
{$IFDEF FHIR_PATIENT}                            tcData, {$ENDIF}
{$IFDEF FHIR_PAYMENTNOTICE}                      tcFinancial, {$ENDIF}
{$IFDEF FHIR_PAYMENTRECONCILIATION}              tcFinancial, {$ENDIF}
{$IFDEF FHIR_PERMISSION}                         tcData, {$ENDIF}
{$IFDEF FHIR_PERSON}                             tcData, {$ENDIF}
{$IFDEF FHIR_PLANDEFINITION}                     tcOther, {$ENDIF}
{$IFDEF FHIR_PRACTITIONER}                       tcData, {$ENDIF}
{$IFDEF FHIR_PRACTITIONERROLE}                   tcData, {$ENDIF}
{$IFDEF FHIR_PROCEDURE}                          tcClinical, {$ENDIF}
{$IFDEF FHIR_PROVENANCE}                         tcAudit, {$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRE}                      tcOther, {$ENDIF}
{$IFDEF FHIR_QUESTIONNAIRERESPONSE}              tcClinical, {$ENDIF}
{$IFDEF FHIR_REGULATEDAUTHORIZATION}             tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_RELATEDPERSON}                      tcData, {$ENDIF}
{$IFDEF FHIR_REQUESTGROUP}                       tcOther, {$ENDIF}
{$IFDEF FHIR_RESEARCHSTUDY}                      tcData, {$ENDIF}
{$IFDEF FHIR_RESEARCHSUBJECT}                    tcData, {$ENDIF}
{$IFDEF FHIR_RISKASSESSMENT}                     tcClinical, {$ENDIF}
{$IFDEF FHIR_SCHEDULE}                           tcSchedule, {$ENDIF}
{$IFDEF FHIR_SEARCHPARAMETER}                    tcOther, {$ENDIF}
{$IFDEF FHIR_SERVICEREQUEST}                     tcClinical, {$ENDIF}
{$IFDEF FHIR_SLOT}                               tcSchedule, {$ENDIF}
{$IFDEF FHIR_SPECIMEN}                           tcClinical, {$ENDIF}
{$IFDEF FHIR_SPECIMENDEFINITION}                 tcOther, {$ENDIF}
{$IFDEF FHIR_STRUCTUREDEFINITION}                tcOther, {$ENDIF}
{$IFDEF FHIR_STRUCTUREMAP}                       tcOther, {$ENDIF}
{$IFDEF FHIR_SUBSCRIPTION}                       tcData, {$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONSTATUS}                 tcData, {$ENDIF}
{$IFDEF FHIR_SUBSCRIPTIONTOPIC}                  tcData, {$ENDIF}
{$IFDEF FHIR_SUBSTANCE}                          tcData, {$ENDIF}
{$IFDEF FHIR_SUBSTANCEDEFINITION}                tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUBSTANCENUCLEICACID}               tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUBSTANCEPOLYMER}                   tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUBSTANCEPROTEIN}                   tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUBSTANCEREFERENCEINFORMATION}      tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUBSTANCESOURCEMATERIAL}            tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_SUPPLYDELIVERY}                     tcData, {$ENDIF}
{$IFDEF FHIR_SUPPLYREQUEST}                      tcData, {$ENDIF}
{$IFDEF FHIR_TASK}                               tcData, {$ENDIF}
{$IFDEF FHIR_TERMINOLOGYCAPABILITIES}            tcOther, {$ENDIF}
{$IFDEF FHIR_TESTREPORT}                         tcOther, {$ENDIF}
{$IFDEF FHIR_TESTSCRIPT}                         tcOther, {$ENDIF}
{$IFDEF FHIR_VALUESET}                           tcOther, {$ENDIF}
{$IFDEF FHIR_VERIFICATIONRESULT}                 tcMedicationDefinition, {$ENDIF}
{$IFDEF FHIR_VISIONPRESCRIPTION}                 tcClinical, {$ENDIF}
    tcOther); // frtCustom)

implementation

end.