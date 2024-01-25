unit fhir5_enums;

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

{$i fhir.inc}
{$i fhir5.inc}

interface

// Generated on Thu, Nov 10, 2022 for FHIR v5.0.0



uses
  Classes, SysUtils,
  fsl_base, fsl_utilities, fsl_crypto, fsl_stream, 
  fhir_objects, fhir_xhtml, fhir_uris, 
  fhir5_base;


const
  FHIR_URI_NONE = '';
  FHIR_URI_ACCOUNT_STATUS = 'http://hl7.org/fhir/account-status';
  FHIR_URI_ACTION_CARDINALITY_BEHAVIOR = 'http://hl7.org/fhir/action-cardinality-behavior';
  FHIR_URI_ACTION_CONDITION_KIND = 'http://hl7.org/fhir/action-condition-kind';
  FHIR_URI_ACTION_GROUPING_BEHAVIOR = 'http://hl7.org/fhir/action-grouping-behavior';
  FHIR_URI_ACTION_PARTICIPANT_TYPE = 'http://hl7.org/fhir/action-participant-type';
  FHIR_URI_ACTION_PRECHECK_BEHAVIOR = 'http://hl7.org/fhir/action-precheck-behavior';
  FHIR_URI_ACTION_RELATIONSHIP_TYPE = 'http://hl7.org/fhir/action-relationship-type';
  FHIR_URI_ACTION_REQUIRED_BEHAVIOR = 'http://hl7.org/fhir/action-required-behavior';
  FHIR_URI_ACTION_SELECTION_BEHAVIOR = 'http://hl7.org/fhir/action-selection-behavior';
  FHIR_URI_ADDRESS_TYPE = 'http://hl7.org/fhir/address-type';
  FHIR_URI_ADDRESS_USE = 'http://hl7.org/fhir/address-use';
  FHIR_URI_ADMINISTRATIVE_GENDER = 'http://hl7.org/fhir/administrative-gender';
  FHIR_URI_ADVERSE_EVENT_ACTUALITY = 'http://hl7.org/fhir/adverse-event-actuality';
  FHIR_URI_EVENT_STATUS = 'http://hl7.org/fhir/event-status';
  FHIR_URI_RESOURCE_AGGREGATION_MODE = 'http://hl7.org/fhir/resource-aggregation-mode';
  FHIR_URI_FHIR_TYPES = 'http://hl7.org/fhir/fhir-types';
  FHIR_URI_ALLERGY_INTOLERANCE_CATEGORY = 'http://hl7.org/fhir/allergy-intolerance-category';
  FHIR_URI_ALLERGY_INTOLERANCE_CRITICALITY = 'http://hl7.org/fhir/allergy-intolerance-criticality';
  FHIR_URI_REACTION_EVENT_SEVERITY = 'http://hl7.org/fhir/reaction-event-severity';
  FHIR_URI_ALLERGY_INTOLERANCE_TYPE = 'http://hl7.org/fhir/allergy-intolerance-type';
  FHIR_URI_APPOINTMENTSTATUS = 'http://hl7.org/fhir/appointmentstatus';
  FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION = 'http://hl7.org/fhir/artifactassessment-disposition';
  FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE = 'http://hl7.org/fhir/artifactassessment-information-type';
  FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS = 'http://hl7.org/fhir/artifactassessment-workflow-status';
  FHIR_URI_ASSERT_DIRECTION_CODES = 'http://hl7.org/fhir/assert-direction-codes';
  FHIR_URI_ASSERT_OPERATOR_CODES = 'http://hl7.org/fhir/assert-operator-codes';
  FHIR_URI_ASSERT_RESPONSE_CODE_TYPES = 'http://hl7.org/fhir/assert-response-code-types';
  FHIR_URI_AUDIT_EVENT_ACTION = 'http://hl7.org/fhir/audit-event-action';
  FHIR_URI_AUDIT_EVENT_SEVERITY = 'http://hl7.org/fhir/audit-event-severity';
  FHIR_URI_BINDING_STRENGTH = 'http://hl7.org/fhir/binding-strength';
  FHIR_URI_BUNDLE_TYPE = 'http://hl7.org/fhir/bundle-type';
  FHIR_URI_CAPABILITY_STATEMENT_KIND = 'http://hl7.org/fhir/capability-statement-kind';
  FHIR_URI_CARE_PLAN_ACTIVITY_STATUS = 'http://hl7.org/fhir/care-plan-activity-status';
  FHIR_URI_REQUEST_INTENT = 'http://hl7.org/fhir/request-intent';
  FHIR_URI_CARE_TEAM_STATUS = 'http://hl7.org/fhir/care-team-status';
  FHIR_URI_CHARACTERISTIC_COMBINATION = 'http://hl7.org/fhir/characteristic-combination';
  FHIR_URI_CHARGEITEM_STATUS = 'http://hl7.org/fhir/chargeitem-status';
  FHIR_URI_CLAIM_OUTCOME = 'http://hl7.org/fhir/claim-outcome';
  FHIR_URI_CLINICAL_USE_DEFINITION_TYPE = 'http://hl7.org/fhir/clinical-use-definition-type';
  FHIR_URI_CODE_SEARCH_SUPPORT = 'http://hl7.org/fhir/code-search-support';
  FHIR_URI_CODESYSTEM_CONTENT_MODE = 'http://hl7.org/fhir/codesystem-content-mode';
  FHIR_URI_CODESYSTEM_HIERARCHY_MEANING = 'http://hl7.org/fhir/codesystem-hierarchy-meaning';
  FHIR_URI_COMPARTMENT_TYPE = 'http://hl7.org/fhir/compartment-type';
  FHIR_URI_COMPOSITION_STATUS = 'http://hl7.org/fhir/composition-status';
  FHIR_URI_CONCEPTMAP_UNMAPPED_MODE = 'http://hl7.org/fhir/conceptmap-unmapped-mode';
  FHIR_URI_CONCEPT_MAP_RELATIONSHIP = 'http://hl7.org/fhir/concept-map-relationship';
  FHIR_URI_CONCEPT_PROPERTY_TYPE = 'http://hl7.org/fhir/concept-property-type';
  FHIR_URI_CONDITION_PRECONDITION_TYPE = 'http://hl7.org/fhir/condition-precondition-type';
  FHIR_URI_CONDITION_QUESTIONNAIRE_PURPOSE = 'http://hl7.org/fhir/condition-questionnaire-purpose';
  FHIR_URI_CONDITIONAL_DELETE_STATUS = 'http://hl7.org/fhir/conditional-delete-status';
  FHIR_URI_CONDITIONAL_READ_STATUS = 'http://hl7.org/fhir/conditional-read-status';
  FHIR_URI_CONFORMANCE_EXPECTATION = 'http://hl7.org/fhir/conformance-expectation';
  FHIR_URI_CONSENT_DATA_MEANING = 'http://hl7.org/fhir/consent-data-meaning';
  FHIR_URI_CONSENT_PROVISION_TYPE = 'http://hl7.org/fhir/consent-provision-type';
  FHIR_URI_CONSENT_STATE_CODES = 'http://hl7.org/fhir/consent-state-codes';
  FHIR_URI_CONSTRAINT_SEVERITY = 'http://hl7.org/fhir/constraint-severity';
  FHIR_URI_CONTACT_POINT_SYSTEM = 'http://hl7.org/fhir/contact-point-system';
  FHIR_URI_CONTACT_POINT_USE = 'http://hl7.org/fhir/contact-point-use';
  FHIR_URI_CONTRACT_PUBLICATIONSTATUS = 'http://hl7.org/fhir/contract-publicationstatus';
  FHIR_URI_CONTRACT_STATUS = 'http://hl7.org/fhir/contract-status';
  FHIR_URI_CONTRIBUTOR_TYPE = 'http://hl7.org/fhir/contributor-type';
  FHIR_URI_SUBSCRIPTIONTOPIC_CR_BEHAVIOR = 'http://hl7.org/fhir/subscriptiontopic-cr-behavior';
  FHIR_URI_DAYS_OF_WEEK = 'http://hl7.org/fhir/days-of-week';
  FHIR_URI_DETECTEDISSUE_SEVERITY = 'http://hl7.org/fhir/detectedissue-severity';
  FHIR_URI_OBSERVATION_STATUS = 'http://hl7.org/fhir/observation-status';
  FHIR_URI_DETECTEDISSUE_STATUS = 'http://hl7.org/fhir/detectedissue-status';
  FHIR_URI_DEVICE_CORRECTIVEACTIONSCOPE = 'http://hl7.org/fhir/device-correctiveactionscope';
  FHIR_URI_DEVICEDEFINITION_REGULATORY_IDENTIFIER_TYPE = 'http://hl7.org/fhir/devicedefinition-regulatory-identifier-type';
  FHIR_URI_DEVICEDISPENSE_STATUS = 'http://terminology.hl7.org/CodeSystem/devicedispense-status';
  FHIR_URI_METRIC_CALIBRATION_STATE = 'http://hl7.org/fhir/metric-calibration-state';
  FHIR_URI_METRIC_CALIBRATION_TYPE = 'http://hl7.org/fhir/metric-calibration-type';
  FHIR_URI_METRIC_CATEGORY = 'http://hl7.org/fhir/metric-category';
  FHIR_URI_METRIC_COLOR = 'http://hl7.org/fhir/metric-color';
  FHIR_URI_METRIC_OPERATIONAL_STATUS = 'http://hl7.org/fhir/metric-operational-status';
  FHIR_URI_DEVICE_NAMETYPE = 'http://hl7.org/fhir/device-nametype';
  FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI = 'http://terminology.hl7.org/CodeSystem/device-productidentifierinudi';
  FHIR_URI_DEVICEUSAGE_STATUS = 'http://hl7.org/fhir/deviceusage-status';
  FHIR_URI_DIAGNOSTIC_REPORT_STATUS = 'http://hl7.org/fhir/diagnostic-report-status';
  FHIR_URI_DISCRIMINATOR_TYPE = 'http://hl7.org/fhir/discriminator-type';
  FHIR_URI_DOCUMENT_MODE = 'http://hl7.org/fhir/document-mode';
  FHIR_URI_DOCUMENT_REFERENCE_STATUS = 'http://hl7.org/fhir/document-reference-status';
  FHIR_URI_ELIGIBILITY_OUTCOME = 'http://hl7.org/fhir/eligibility-outcome';
  FHIR_URI_ELIGIBILITYREQUEST_PURPOSE = 'http://hl7.org/fhir/eligibilityrequest-purpose';
  FHIR_URI_ELIGIBILITYRESPONSE_PURPOSE = 'http://hl7.org/fhir/eligibilityresponse-purpose';
  FHIR_URI_QUESTIONNAIRE_ENABLE_BEHAVIOR = 'http://hl7.org/fhir/questionnaire-enable-behavior';
  FHIR_URI_ENCOUNTER_LOCATION_STATUS = 'http://hl7.org/fhir/encounter-location-status';
  FHIR_URI_ENCOUNTER_STATUS = 'http://hl7.org/fhir/encounter-status';
  FHIR_URI_ENDPOINT_STATUS = 'http://hl7.org/fhir/endpoint-status';
  FHIR_URI_ENROLLMENT_OUTCOME = 'http://hl7.org/fhir/enrollment-outcome';
  FHIR_URI_EPISODE_OF_CARE_STATUS = 'http://hl7.org/fhir/episode-of-care-status';
  FHIR_URI_EVENT_CAPABILITY_MODE = 'http://hl7.org/fhir/event-capability-mode';
  FHIR_URI_EVENT_TIMING = 'http://hl7.org/fhir/event-timing';
  FHIR_URI_V3_TIMINGEVENT = 'http://terminology.hl7.org/CodeSystem/v3-TimingEvent';
  FHIR_URI_VARIABLE_HANDLING = 'http://hl7.org/fhir/variable-handling';
  FHIR_URI_EXAMPLESCENARIO_ACTOR_TYPE = 'http://hl7.org/fhir/examplescenario-actor-type';
  FHIR_URI_EXPLANATIONOFBENEFIT_STATUS = 'http://hl7.org/fhir/explanationofbenefit-status';
  FHIR_URI_EXTENSION_CONTEXT_TYPE = 'http://hl7.org/fhir/extension-context-type';
  FHIR_URI_DEVICE_STATUS = 'http://hl7.org/fhir/device-status';
  FHIR_URI_SUBSTANCE_STATUS = 'http://hl7.org/fhir/substance-status';
  FHIR_URI_FHIR_VERSION = 'http://hl7.org/fhir/FHIR-version';
  FHIR_URI_HISTORY_STATUS = 'http://hl7.org/fhir/history-status';
  FHIR_URI_FILTER_OPERATOR = 'http://hl7.org/fhir/filter-operator';
  FHIR_URI_FM_STATUS = 'http://hl7.org/fhir/fm-status';
  FHIR_URI_FLAG_STATUS = 'http://hl7.org/fhir/flag-status';
  FHIR_URI_FORMULARYITEM_STATUS = 'http://hl7.org/fhir/CodeSystem/formularyitem-status';
  FHIR_URI_GOAL_STATUS = 'http://hl7.org/fhir/goal-status';
  FHIR_URI_GRAPH_COMPARTMENT_RULE = 'http://hl7.org/fhir/graph-compartment-rule';
  FHIR_URI_GRAPH_COMPARTMENT_USE = 'http://hl7.org/fhir/graph-compartment-use';
  FHIR_URI_GROUP_MEMBERSHIP_BASIS = 'http://hl7.org/fhir/group-membership-basis';
  FHIR_URI_GROUP_TYPE = 'http://hl7.org/fhir/group-type';
  FHIR_URI_GUIDANCE_RESPONSE_STATUS = 'http://hl7.org/fhir/guidance-response-status';
  FHIR_URI_GUIDE_PAGE_GENERATION = 'http://hl7.org/fhir/guide-page-generation';
  FHIR_URI_HTTP_VERB = 'http://hl7.org/fhir/http-verb';
  FHIR_URI_IDENTIFIER_USE = 'http://hl7.org/fhir/identifier-use';
  FHIR_URI_IDENTITY_ASSURANCELEVEL = 'http://hl7.org/fhir/identity-assuranceLevel';
  FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE = 'http://hl7.org/fhir/imagingselection-2dgraphictype';
  FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE = 'http://hl7.org/fhir/imagingselection-3dgraphictype';
  FHIR_URI_IMAGINGSELECTION_STATUS = 'http://hl7.org/fhir/imagingselection-status';
  FHIR_URI_IMAGINGSTUDY_STATUS = 'http://hl7.org/fhir/imagingstudy-status';
  FHIR_URI_MEDICATION_ADMIN_STATUS = 'http://hl7.org/fhir/CodeSystem/medication-admin-status';
  FHIR_URI_INGREDIENT_MANUFACTURER_ROLE = 'http://hl7.org/fhir/ingredient-manufacturer-role';
  FHIR_URI_RESTFUL_INTERACTION = 'http://hl7.org/fhir/restful-interaction';
  FHIR_URI_INVENTORYREPORT_COUNTTYPE = 'http://hl7.org/fhir/inventoryreport-counttype';
  FHIR_URI_INVENTORYREPORT_STATUS = 'http://hl7.org/fhir/inventoryreport-status';
  FHIR_URI_INVOICE_STATUS = 'http://hl7.org/fhir/invoice-status';
  FHIR_URI_ISSUE_SEVERITY = 'http://hl7.org/fhir/issue-severity';
  FHIR_URI_ISSUE_TYPE = 'http://hl7.org/fhir/issue-type';
  FHIR_URI_COVERAGE_KIND = 'http://hl7.org/fhir/coverage-kind';
  FHIR_URI_IANA_LINK_RELATIONS = 'http://hl7.org/fhir/CodeSystem/iana-link-relations';
  FHIR_URI_LINK_TYPE = 'http://hl7.org/fhir/link-type';
  FHIR_URI_LINKAGE_TYPE = 'http://hl7.org/fhir/linkage-type';
  FHIR_URI_LIST_MODE = 'http://hl7.org/fhir/list-mode';
  FHIR_URI_LIST_STATUS = 'http://hl7.org/fhir/list-status';
  FHIR_URI_LOCATION_MODE = 'http://hl7.org/fhir/location-mode';
  FHIR_URI_LOCATION_STATUS = 'http://hl7.org/fhir/location-status';
  FHIR_URI_MEASURE_REPORT_STATUS = 'http://hl7.org/fhir/measure-report-status';
  FHIR_URI_MEASURE_REPORT_TYPE = 'http://hl7.org/fhir/measure-report-type';
  FHIR_URI_MEDICATIONDISPENSE_STATUS = 'http://hl7.org/fhir/CodeSystem/medicationdispense-status';
  FHIR_URI_MEDICATIONKNOWLEDGE_STATUS = 'http://hl7.org/fhir/CodeSystem/medicationknowledge-status';
  FHIR_URI_MEDICATIONREQUEST_INTENT = 'http://hl7.org/fhir/CodeSystem/medicationrequest-intent';
  FHIR_URI_MEDICATION_STATUS = 'http://hl7.org/fhir/CodeSystem/medication-status';
  FHIR_URI_MEDICATION_USAGE_STATUS = 'http://hl7.org/fhir/CodeSystem/medication-usage-status';
  FHIR_URI_MEDICATIONREQUEST_STATUS = 'http://hl7.org/fhir/CodeSystem/medicationrequest-status';
  FHIR_URI_MESSAGE_SIGNIFICANCE_CATEGORY = 'http://hl7.org/fhir/message-significance-category';
  FHIR_URI_MESSAGEHEADER_RESPONSE_REQUEST = 'http://hl7.org/fhir/messageheader-response-request';
  FHIR_URI_NAME_USE = 'http://hl7.org/fhir/name-use';
  FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE = 'http://hl7.org/fhir/namingsystem-identifier-type';
  FHIR_URI_NAMINGSYSTEM_TYPE = 'http://hl7.org/fhir/namingsystem-type';
  FHIR_URI_NARRATIVE_STATUS = 'http://hl7.org/fhir/narrative-status';
  FHIR_URI_NOTE_TYPE = 'http://hl7.org/fhir/note-type';
  FHIR_URI_NUTRITIONPRODUCT_STATUS = 'http://hl7.org/fhir/nutritionproduct-status';
  FHIR_URI_PERMITTED_DATA_TYPE = 'http://hl7.org/fhir/permitted-data-type';
  FHIR_URI_OBSERVATION_RANGE_CATEGORY = 'http://hl7.org/fhir/observation-range-category';
  FHIR_URI_OPERATION_KIND = 'http://hl7.org/fhir/operation-kind';
  FHIR_URI_OPERATION_PARAMETER_SCOPE = 'http://hl7.org/fhir/operation-parameter-scope';
  FHIR_URI_OPERATION_PARAMETER_USE = 'http://hl7.org/fhir/operation-parameter-use';
  FHIR_URI_ORIENTATION_TYPE = 'http://hl7.org/fhir/orientation-type';
  FHIR_URI_PARTICIPATIONSTATUS = 'http://hl7.org/fhir/participationstatus';
  FHIR_URI_PAYMENT_OUTCOME = 'http://hl7.org/fhir/payment-outcome';
  FHIR_URI_PERMISSION_RULE_COMBINING = 'http://hl7.org/fhir/permission-rule-combining';
  FHIR_URI_PERMISSION_STATUS = 'http://hl7.org/fhir/permission-status';
  FHIR_URI_PRICE_COMPONENT_TYPE = 'http://hl7.org/fhir/price-component-type';
  FHIR_URI_PROPERTY_REPRESENTATION = 'http://hl7.org/fhir/property-representation';
  FHIR_URI_PROVENANCE_ENTITY_ROLE = 'http://hl7.org/fhir/provenance-entity-role';
  FHIR_URI_PUBLICATION_STATUS = 'http://hl7.org/fhir/publication-status';
  FHIR_URI_QUANTITY_COMPARATOR = 'http://hl7.org/fhir/quantity-comparator';
  FHIR_URI_QUESTIONNAIRE_ANSWER_CONSTRAINT = 'http://hl7.org/fhir/questionnaire-answer-constraint';
  FHIR_URI_QUESTIONNAIRE_DISABLED_DISPLAY = 'http://hl7.org/fhir/questionnaire-disabled-display';
  FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR = 'http://hl7.org/fhir/questionnaire-enable-operator';
  FHIR_URI_ITEM_TYPE = 'http://hl7.org/fhir/item-type';
  FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS = 'http://hl7.org/fhir/questionnaire-answers-status';
  FHIR_URI_REFERENCE_HANDLING_POLICY = 'http://hl7.org/fhir/reference-handling-policy';
  FHIR_URI_REFERENCE_VERSION_RULES = 'http://hl7.org/fhir/reference-version-rules';
  FHIR_URI_RELATED_ARTIFACT_TYPE = 'http://hl7.org/fhir/related-artifact-type';
  FHIR_URI_RELATED_ARTIFACT_TYPE_EXPANDED = 'http://terminology.hl7.org/CodeSystem/related-artifact-type-expanded';
  FHIR_URI_REPORT_RELATION_TYPE = 'http://hl7.org/fhir/report-relation-type';
  FHIR_URI_REQUEST_PRIORITY = 'http://hl7.org/fhir/request-priority';
  FHIR_URI_REQUEST_STATUS = 'http://hl7.org/fhir/request-status';
  FHIR_URI_VERSIONING_POLICY = 'http://hl7.org/fhir/versioning-policy';
  FHIR_URI_RESPONSE_CODE = 'http://hl7.org/fhir/response-code';
  FHIR_URI_RESTFUL_CAPABILITY_MODE = 'http://hl7.org/fhir/restful-capability-mode';
  FHIR_URI_SPDX_LICENSE = 'http://hl7.org/fhir/spdx-license';
  FHIR_URI_SEARCH_COMPARATOR = 'http://hl7.org/fhir/search-comparator';
  FHIR_URI_SEARCH_ENTRY_MODE = 'http://hl7.org/fhir/search-entry-mode';
  FHIR_URI_SEARCH_MODIFIER_CODE = 'http://hl7.org/fhir/search-modifier-code';
  FHIR_URI_SEARCH_PARAM_TYPE = 'http://hl7.org/fhir/search-param-type';
  FHIR_URI_SEARCH_PROCESSINGMODE = 'http://hl7.org/fhir/search-processingmode';
  FHIR_URI_SEQUENCE_TYPE = 'http://hl7.org/fhir/sequence-type';
  FHIR_URI_RESOURCE_SLICING_RULES = 'http://hl7.org/fhir/resource-slicing-rules';
  FHIR_URI_SLOTSTATUS = 'http://hl7.org/fhir/slotstatus';
  FHIR_URI_SORT_DIRECTION = 'http://hl7.org/fhir/sort-direction';
  FHIR_URI_SPECIMEN_COMBINED = 'http://hl7.org/fhir/specimen-combined';
  FHIR_URI_SPECIMEN_CONTAINED_PREFERENCE = 'http://hl7.org/fhir/specimen-contained-preference';
  FHIR_URI_SPECIMEN_STATUS = 'http://hl7.org/fhir/specimen-status';
  FHIR_URI_VERIFICATIONRESULT_STATUS = 'http://hl7.org/fhir/CodeSystem/verificationresult-status';
  FHIR_URI_STRAND_TYPE = 'http://hl7.org/fhir/strand-type';
  FHIR_URI_STRUCTURE_DEFINITION_KIND = 'http://hl7.org/fhir/structure-definition-kind';
  FHIR_URI_MAP_GROUP_TYPE_MODE = 'http://hl7.org/fhir/map-group-type-mode';
  FHIR_URI_MAP_INPUT_MODE = 'http://hl7.org/fhir/map-input-mode';
  FHIR_URI_MAP_MODEL_MODE = 'http://hl7.org/fhir/map-model-mode';
  FHIR_URI_MAP_SOURCE_LIST_MODE = 'http://hl7.org/fhir/map-source-list-mode';
  FHIR_URI_MAP_TARGET_LIST_MODE = 'http://hl7.org/fhir/map-target-list-mode';
  FHIR_URI_MAP_TRANSFORM = 'http://hl7.org/fhir/map-transform';
  FHIR_URI_SUBMIT_DATA_UPDATE_TYPE = 'http://hl7.org/fhir/CodeSystem/submit-data-update-type';
  FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE = 'http://hl7.org/fhir/subscription-notification-type';
  FHIR_URI_SUBSCRIPTION_PAYLOAD_CONTENT = 'http://hl7.org/fhir/subscription-payload-content';
  FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER = 'http://terminology.hl7.org/CodeSystem/subscription-search-modifier';
  FHIR_URI_SUBSCRIPTION_STATUS = 'http://terminology.hl7.org/CodeSystem/subscription-status';
  FHIR_URI_SUPPLYDELIVERY_STATUS = 'http://hl7.org/fhir/supplydelivery-status';
  FHIR_URI_SUPPLYREQUEST_STATUS = 'http://hl7.org/fhir/supplyrequest-status';
  FHIR_URI_TASK_INTENT = 'http://hl7.org/fhir/task-intent';
  FHIR_URI_TASK_STATUS = 'http://hl7.org/fhir/task-status';
  FHIR_URI_REPORT_ACTION_RESULT_CODES = 'http://hl7.org/fhir/report-action-result-codes';
  FHIR_URI_REPORT_PARTICIPANT_TYPE = 'http://hl7.org/fhir/report-participant-type';
  FHIR_URI_REPORT_RESULT_CODES = 'http://hl7.org/fhir/report-result-codes';
  FHIR_URI_REPORT_STATUS_CODES = 'http://hl7.org/fhir/report-status-codes';
  FHIR_URI_HTTP_OPERATIONS = 'http://hl7.org/fhir/http-operations';
  FHIR_URI_TRANSPORT_INTENT = 'http://hl7.org/fhir/transport-intent';
  FHIR_URI_TRANSPORT_STATUS = 'http://hl7.org/fhir/transport-status';
  FHIR_URI_TRIGGER_TYPE = 'http://hl7.org/fhir/trigger-type';
  FHIR_URI_OBSERVATION_TRIGGEREDBYTYPE = 'http://hl7.org/fhir/observation-triggeredbytype';
  FHIR_URI_TYPE_DERIVATION_RULE = 'http://hl7.org/fhir/type-derivation-rule';
  FHIR_URI_UDI_ENTRY_TYPE = 'http://hl7.org/fhir/udi-entry-type';
  FHIR_URI_CLAIM_USE = 'http://hl7.org/fhir/claim-use';
  FHIR_URI_VISION_BASE_CODES = 'http://hl7.org/fhir/vision-base-codes';
  FHIR_URI_VISION_EYE_CODES = 'http://hl7.org/fhir/vision-eye-codes';


type

  // Indicates whether the account is available to be used. (from http://hl7.org/fhir/ValueSet/account-status)
  TFhirAccountStatusEnum = (
    AccountStatusNull, // Value is missing from Instance
    AccountStatusActive,
    AccountStatusInactive,
    AccountStatusEnteredInError,
    AccountStatusOnHold,
    AccountStatusUnknown);
  TFhirAccountStatusEnumList = set of TFhirAccountStatusEnum;


  // Defines behavior for an action or a group for how many times that item may be repeated. (from http://hl7.org/fhir/ValueSet/action-cardinality-behavior)
  TFhirActionCardinalityBehaviorEnum = (
    ActionCardinalityBehaviorNull, // Value is missing from Instance
    ActionCardinalityBehaviorSingle,
    ActionCardinalityBehaviorMultiple);
  TFhirActionCardinalityBehaviorEnumList = set of TFhirActionCardinalityBehaviorEnum;


  // Defines the kinds of conditions that can appear on actions. (from http://hl7.org/fhir/ValueSet/action-condition-kind)
  TFhirActionConditionKindEnum = (
    ActionConditionKindNull, // Value is missing from Instance
    ActionConditionKindApplicability,
    ActionConditionKindStart,
    ActionConditionKindStop);
  TFhirActionConditionKindEnumList = set of TFhirActionConditionKindEnum;


  // Defines organization behavior of a group. (from http://hl7.org/fhir/ValueSet/action-grouping-behavior)
  TFhirActionGroupingBehaviorEnum = (
    ActionGroupingBehaviorNull, // Value is missing from Instance
    ActionGroupingBehaviorVisualGroup,
    ActionGroupingBehaviorLogicalGroup,
    ActionGroupingBehaviorSentenceGroup);
  TFhirActionGroupingBehaviorEnumList = set of TFhirActionGroupingBehaviorEnum;


  // The type of participant for the action. (from http://hl7.org/fhir/ValueSet/action-participant-type)
  TFhirActionParticipantTypeEnum = (
    ActionParticipantTypeNull, // Value is missing from Instance
    ActionParticipantTypeCareteam,
    ActionParticipantTypeDevice,
    ActionParticipantTypeGroup,
    ActionParticipantTypeHealthcareservice,
    ActionParticipantTypeLocation,
    ActionParticipantTypeOrganization,
    ActionParticipantTypePatient,
    ActionParticipantTypePractitioner,
    ActionParticipantTypePractitionerrole,
    ActionParticipantTypeRelatedperson);
  TFhirActionParticipantTypeEnumList = set of TFhirActionParticipantTypeEnum;


  // Defines selection frequency behavior for an action or group. (from http://hl7.org/fhir/ValueSet/action-precheck-behavior)
  TFhirActionPrecheckBehaviorEnum = (
    ActionPrecheckBehaviorNull, // Value is missing from Instance
    ActionPrecheckBehaviorYes,
    ActionPrecheckBehaviorNo);
  TFhirActionPrecheckBehaviorEnumList = set of TFhirActionPrecheckBehaviorEnum;


  // Defines the types of relationships between actions. (from http://hl7.org/fhir/ValueSet/action-relationship-type)
  TFhirActionRelationshipTypeEnum = (
    ActionRelationshipTypeNull, // Value is missing from Instance
    ActionRelationshipTypeBeforeStart,
    ActionRelationshipTypeBefore,
    ActionRelationshipTypeBeforeEnd,
    ActionRelationshipTypeConcurrentWithStart,
    ActionRelationshipTypeConcurrent,
    ActionRelationshipTypeConcurrentWithEnd,
    ActionRelationshipTypeAfterStart,
    ActionRelationshipTypeAfter,
    ActionRelationshipTypeAfterEnd);
  TFhirActionRelationshipTypeEnumList = set of TFhirActionRelationshipTypeEnum;


  // Defines expectations around whether an action or action group is required. (from http://hl7.org/fhir/ValueSet/action-required-behavior)
  TFhirActionRequiredBehaviorEnum = (
    ActionRequiredBehaviorNull, // Value is missing from Instance
    ActionRequiredBehaviorMust,
    ActionRequiredBehaviorCould,
    ActionRequiredBehaviorMustUnlessDocumented);
  TFhirActionRequiredBehaviorEnumList = set of TFhirActionRequiredBehaviorEnum;


  // Defines selection behavior of a group. (from http://hl7.org/fhir/ValueSet/action-selection-behavior)
  TFhirActionSelectionBehaviorEnum = (
    ActionSelectionBehaviorNull, // Value is missing from Instance
    ActionSelectionBehaviorAny,
    ActionSelectionBehaviorAll,
    ActionSelectionBehaviorAllOrNone,
    ActionSelectionBehaviorExactlyOne,
    ActionSelectionBehaviorAtMostOne,
    ActionSelectionBehaviorOneOrMore);
  TFhirActionSelectionBehaviorEnumList = set of TFhirActionSelectionBehaviorEnum;


  // The type of an address (physical / postal). (from http://hl7.org/fhir/ValueSet/address-type)
  TFhirAddressTypeEnum = (
    AddressTypeNull, // Value is missing from Instance
    AddressTypePostal,
    AddressTypePhysical,
    AddressTypeBoth);
  TFhirAddressTypeEnumList = set of TFhirAddressTypeEnum;


  // The use of an address. (from http://hl7.org/fhir/ValueSet/address-use)
  TFhirAddressUseEnum = (
    AddressUseNull, // Value is missing from Instance
    AddressUseHome,
    AddressUseWork,
    AddressUseTemp,
    AddressUseOld,
    AddressUseBilling);
  TFhirAddressUseEnumList = set of TFhirAddressUseEnum;


  // The gender of a person used for administrative purposes. (from http://hl7.org/fhir/ValueSet/administrative-gender)
  TFhirAdministrativeGenderEnum = (
    AdministrativeGenderNull, // Value is missing from Instance
    AdministrativeGenderMale,
    AdministrativeGenderFemale,
    AdministrativeGenderOther,
    AdministrativeGenderUnknown);
  TFhirAdministrativeGenderEnumList = set of TFhirAdministrativeGenderEnum;


  // Overall nature of the adverse event, e.g. real or potential. (from http://hl7.org/fhir/ValueSet/adverse-event-actuality)
  TFhirAdverseEventActualityEnum = (
    AdverseEventActualityNull, // Value is missing from Instance
    AdverseEventActualityActual,
    AdverseEventActualityPotential);
  TFhirAdverseEventActualityEnumList = set of TFhirAdverseEventActualityEnum;


  // Codes identifying the lifecycle stage of an adverse event. (from http://hl7.org/fhir/ValueSet/adverse-event-status)
  TFhirAdverseEventStatusEnum = (
    AdverseEventStatusNull, // Value is missing from Instance
    AdverseEventStatusInProgress,
    AdverseEventStatusCompleted,
    AdverseEventStatusEnteredInError,
    AdverseEventStatusUnknown);
  TFhirAdverseEventStatusEnumList = set of TFhirAdverseEventStatusEnum;


  // How resource references can be aggregated. (from http://hl7.org/fhir/ValueSet/resource-aggregation-mode)
  TFhirAggregationModeEnum = (
    AggregationModeNull, // Value is missing from Instance
    AggregationModeContained,
    AggregationModeReferenced,
    AggregationModeBundled);
  TFhirAggregationModeEnumList = set of TFhirAggregationModeEnum;


  // All fhir data types (including abstract resources) (from http://hl7.org/fhir/ValueSet/all-resource-types)
  TFhirAllResourceTypesEnum = (
    AllResourceTypesNull, // Value is missing from Instance
    AllResourceTypesAccount,
    AllResourceTypesActivityDefinition,
    AllResourceTypesActorDefinition,
    AllResourceTypesAdministrableProductDefinition,
    AllResourceTypesAdverseEvent,
    AllResourceTypesAllergyIntolerance,
    AllResourceTypesAppointment,
    AllResourceTypesAppointmentResponse,
    AllResourceTypesArtifactAssessment,
    AllResourceTypesAuditEvent,
    AllResourceTypesBasic,
    AllResourceTypesBinary,
    AllResourceTypesBiologicallyDerivedProduct,
    AllResourceTypesBodyStructure,
    AllResourceTypesBundle,
    AllResourceTypesCanonicalResource,
    AllResourceTypesCapabilityStatement,
    AllResourceTypesCarePlan,
    AllResourceTypesCareTeam,
    AllResourceTypesChargeItem,
    AllResourceTypesChargeItemDefinition,
    AllResourceTypesCitation,
    AllResourceTypesClaim,
    AllResourceTypesClaimResponse,
    AllResourceTypesClinicalImpression,
    AllResourceTypesClinicalUseDefinition,
    AllResourceTypesCodeSystem,
    AllResourceTypesCommunication,
    AllResourceTypesCommunicationRequest,
    AllResourceTypesCompartmentDefinition,
    AllResourceTypesComposition,
    AllResourceTypesConceptMap,
    AllResourceTypesCondition,
    AllResourceTypesConditionDefinition,
    AllResourceTypesConsent,
    AllResourceTypesContract,
    AllResourceTypesCoverage,
    AllResourceTypesCoverageEligibilityRequest,
    AllResourceTypesCoverageEligibilityResponse,
    AllResourceTypesDetectedIssue,
    AllResourceTypesDevice,
    AllResourceTypesDeviceDefinition,
    AllResourceTypesDeviceDispense,
    AllResourceTypesDeviceMetric,
    AllResourceTypesDeviceRequest,
    AllResourceTypesDeviceUsage,
    AllResourceTypesDiagnosticReport,
    AllResourceTypesDocumentManifest,
    AllResourceTypesDocumentReference,
    AllResourceTypesDomainResource,
    AllResourceTypesEncounter,
    AllResourceTypesEndpoint,
    AllResourceTypesEnrollmentRequest,
    AllResourceTypesEnrollmentResponse,
    AllResourceTypesEpisodeOfCare,
    AllResourceTypesEventDefinition,
    AllResourceTypesEvidence,
    AllResourceTypesEvidenceReport,
    AllResourceTypesEvidenceVariable,
    AllResourceTypesExampleScenario,
    AllResourceTypesExplanationOfBenefit,
    AllResourceTypesFamilyMemberHistory,
    AllResourceTypesFlag,
    AllResourceTypesFormularyItem,
    AllResourceTypesGenomicStudy,
    AllResourceTypesGoal,
    AllResourceTypesGraphDefinition,
    AllResourceTypesGroup,
    AllResourceTypesGuidanceResponse,
    AllResourceTypesHealthcareService,
    AllResourceTypesImagingSelection,
    AllResourceTypesImagingStudy,
    AllResourceTypesImmunization,
    AllResourceTypesImmunizationEvaluation,
    AllResourceTypesImmunizationRecommendation,
    AllResourceTypesImplementationGuide,
    AllResourceTypesIngredient,
    AllResourceTypesInsurancePlan,
    AllResourceTypesInventoryReport,
    AllResourceTypesInvoice,
    AllResourceTypesLibrary,
    AllResourceTypesLinkage,
    AllResourceTypesList,
    AllResourceTypesLocation,
    AllResourceTypesManufacturedItemDefinition,
    AllResourceTypesMeasure,
    AllResourceTypesMeasureReport,
    AllResourceTypesMedication,
    AllResourceTypesMedicationAdministration,
    AllResourceTypesMedicationDispense,
    AllResourceTypesMedicationKnowledge,
    AllResourceTypesMedicationRequest,
    AllResourceTypesMedicationUsage,
    AllResourceTypesMedicinalProductDefinition,
    AllResourceTypesMessageDefinition,
    AllResourceTypesMessageHeader,
    AllResourceTypesMetadataResource,
    AllResourceTypesMolecularSequence,
    AllResourceTypesNamingSystem,
    AllResourceTypesNutritionIntake,
    AllResourceTypesNutritionOrder,
    AllResourceTypesNutritionProduct,
    AllResourceTypesObservation,
    AllResourceTypesObservationDefinition,
    AllResourceTypesOperationDefinition,
    AllResourceTypesOperationOutcome,
    AllResourceTypesOrganization,
    AllResourceTypesOrganizationAffiliation,
    AllResourceTypesPackagedProductDefinition,
    AllResourceTypesParameters,
    AllResourceTypesPatient,
    AllResourceTypesPaymentNotice,
    AllResourceTypesPaymentReconciliation,
    AllResourceTypesPermission,
    AllResourceTypesPerson,
    AllResourceTypesPlanDefinition,
    AllResourceTypesPractitioner,
    AllResourceTypesPractitionerRole,
    AllResourceTypesProcedure,
    AllResourceTypesProvenance,
    AllResourceTypesQuestionnaire,
    AllResourceTypesQuestionnaireResponse,
    AllResourceTypesRegulatedAuthorization,
    AllResourceTypesRelatedPerson,
    AllResourceTypesRequestOrchestration,
    AllResourceTypesRequirements,
    AllResourceTypesResearchStudy,
    AllResourceTypesResearchSubject,
    AllResourceTypesResource,
    AllResourceTypesRiskAssessment,
    AllResourceTypesSchedule,
    AllResourceTypesSearchParameter,
    AllResourceTypesServiceRequest,
    AllResourceTypesSlot,
    AllResourceTypesSpecimen,
    AllResourceTypesSpecimenDefinition,
    AllResourceTypesStructureDefinition,
    AllResourceTypesStructureMap,
    AllResourceTypesSubscription,
    AllResourceTypesSubscriptionStatus,
    AllResourceTypesSubscriptionTopic,
    AllResourceTypesSubstance,
    AllResourceTypesSubstanceDefinition,
    AllResourceTypesSubstanceNucleicAcid,
    AllResourceTypesSubstancePolymer,
    AllResourceTypesSubstanceProtein,
    AllResourceTypesSubstanceReferenceInformation,
    AllResourceTypesSubstanceSourceMaterial,
    AllResourceTypesSupplyDelivery,
    AllResourceTypesSupplyRequest,
    AllResourceTypesTask,
    AllResourceTypesTerminologyCapabilities,
    AllResourceTypesTestReport,
    AllResourceTypesTestScript,
    AllResourceTypesTransport,
    AllResourceTypesValueSet,
    AllResourceTypesVerificationResult,
    AllResourceTypesVisionPrescription);
  TFhirAllResourceTypesEnumList = set of TFhirAllResourceTypesEnum;


  // Category of an identified substance associated with allergies or intolerances. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-category)
  TFhirAllergyIntoleranceCategoryEnum = (
    AllergyIntoleranceCategoryNull, // Value is missing from Instance
    AllergyIntoleranceCategoryFood,
    AllergyIntoleranceCategoryMedication,
    AllergyIntoleranceCategoryEnvironment,
    AllergyIntoleranceCategoryBiologic);
  TFhirAllergyIntoleranceCategoryEnumList = set of TFhirAllergyIntoleranceCategoryEnum;


  // Estimate of the potential clinical harm, or seriousness, of a reaction to an identified substance. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-criticality)
  TFhirAllergyIntoleranceCriticalityEnum = (
    AllergyIntoleranceCriticalityNull, // Value is missing from Instance
    AllergyIntoleranceCriticalityLow,
    AllergyIntoleranceCriticalityHigh,
    AllergyIntoleranceCriticalityUnableToAssess);
  TFhirAllergyIntoleranceCriticalityEnumList = set of TFhirAllergyIntoleranceCriticalityEnum;


  // Clinical assessment of the severity of a reaction event as a whole, potentially considering multiple different manifestations. (from http://hl7.org/fhir/ValueSet/reaction-event-severity)
  TFhirAllergyIntoleranceSeverityEnum = (
    AllergyIntoleranceSeverityNull, // Value is missing from Instance
    AllergyIntoleranceSeverityMild,
    AllergyIntoleranceSeverityModerate,
    AllergyIntoleranceSeveritySevere);
  TFhirAllergyIntoleranceSeverityEnumList = set of TFhirAllergyIntoleranceSeverityEnum;


  // Identification of the underlying physiological mechanism for a Reaction Risk. (from http://hl7.org/fhir/ValueSet/allergy-intolerance-type)
  TFhirAllergyIntoleranceTypeEnum = (
    AllergyIntoleranceTypeNull, // Value is missing from Instance
    AllergyIntoleranceTypeAllergy,
    AllergyIntoleranceTypeIntolerance);
  TFhirAllergyIntoleranceTypeEnumList = set of TFhirAllergyIntoleranceTypeEnum;


  // The free/busy status of an appointment. (from http://hl7.org/fhir/ValueSet/appointmentstatus)
  TFhirAppointmentStatusEnum = (
    AppointmentStatusNull, // Value is missing from Instance
    AppointmentStatusProposed,
    AppointmentStatusPending,
    AppointmentStatusBooked,
    AppointmentStatusArrived,
    AppointmentStatusFulfilled,
    AppointmentStatusCancelled,
    AppointmentStatusNoshow,
    AppointmentStatusEnteredInError,
    AppointmentStatusCheckedIn,
    AppointmentStatusWaitlist);
  TFhirAppointmentStatusEnumList = set of TFhirAppointmentStatusEnum;


  // Possible values for the disposition of a comment or change request, typically used for comments and change requests, to indicate the disposition of the responsible party towards the changes suggested by the comment or change request. (from http://hl7.org/fhir/ValueSet/artifactassessment-disposition)
  TFhirArtifactAssessmentDispositionEnum = (
    ArtifactAssessmentDispositionNull, // Value is missing from Instance
    ArtifactAssessmentDispositionUnresolved,
    ArtifactAssessmentDispositionNotPersuasive,
    ArtifactAssessmentDispositionPersuasive,
    ArtifactAssessmentDispositionPersuasiveWithModification,
    ArtifactAssessmentDispositionNotPersuasiveWithModification);
  TFhirArtifactAssessmentDispositionEnumList = set of TFhirArtifactAssessmentDispositionEnum;


  // The type of information contained in a component of an artifact assessment. (from http://hl7.org/fhir/ValueSet/artifactassessment-information-type)
  TFhirArtifactAssessmentInformationTypeEnum = (
    ArtifactAssessmentInformationTypeNull, // Value is missing from Instance
    ArtifactAssessmentInformationTypeComment,
    ArtifactAssessmentInformationTypeClassifier,
    ArtifactAssessmentInformationTypeRating,
    ArtifactAssessmentInformationTypeContainer,
    ArtifactAssessmentInformationTypeResponse,
    ArtifactAssessmentInformationTypeChangeRequest);
  TFhirArtifactAssessmentInformationTypeEnumList = set of TFhirArtifactAssessmentInformationTypeEnum;


  // Possible values for the workflow status of the comment or assessment, typically used to coordinate workflow around the process of accepting and rejecting changes and comments on the artifact. (from http://hl7.org/fhir/ValueSet/artifactassessment-workflow-status)
  TFhirArtifactAssessmentWorkflowStatusEnum = (
    ArtifactAssessmentWorkflowStatusNull, // Value is missing from Instance
    ArtifactAssessmentWorkflowStatusSubmitted,
    ArtifactAssessmentWorkflowStatusTriaged,
    ArtifactAssessmentWorkflowStatusWaitingForInput,
    ArtifactAssessmentWorkflowStatusResolvedNoChange,
    ArtifactAssessmentWorkflowStatusResolvedChangeRequired,
    ArtifactAssessmentWorkflowStatusDeferred,
    ArtifactAssessmentWorkflowStatusDuplicate,
    ArtifactAssessmentWorkflowStatusApplied,
    ArtifactAssessmentWorkflowStatusPublished);
  TFhirArtifactAssessmentWorkflowStatusEnumList = set of TFhirArtifactAssessmentWorkflowStatusEnum;


  // The type of direction to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-direction-codes)
  TFhirAssertionDirectionTypeEnum = (
    AssertionDirectionTypeNull, // Value is missing from Instance
    AssertionDirectionTypeResponse,
    AssertionDirectionTypeRequest);
  TFhirAssertionDirectionTypeEnumList = set of TFhirAssertionDirectionTypeEnum;


  // The type of operator to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-operator-codes)
  TFhirAssertionOperatorTypeEnum = (
    AssertionOperatorTypeNull, // Value is missing from Instance
    AssertionOperatorTypeEquals,
    AssertionOperatorTypeNotEquals,
    AssertionOperatorTypeIn,
    AssertionOperatorTypeNotIn,
    AssertionOperatorTypeGreaterThan,
    AssertionOperatorTypeLessThan,
    AssertionOperatorTypeEmpty,
    AssertionOperatorTypeNotEmpty,
    AssertionOperatorTypeContains,
    AssertionOperatorTypeNotContains,
    AssertionOperatorTypeEval);
  TFhirAssertionOperatorTypeEnumList = set of TFhirAssertionOperatorTypeEnum;


  // The type of response code to use for assertion. (from http://hl7.org/fhir/ValueSet/assert-response-code-types)
  TFhirAssertionResponseTypesEnum = (
    AssertionResponseTypesNull, // Value is missing from Instance
    AssertionResponseTypesOkay,
    AssertionResponseTypesCreated,
    AssertionResponseTypesNoContent,
    AssertionResponseTypesNotModified,
    AssertionResponseTypesBad,
    AssertionResponseTypesForbidden,
    AssertionResponseTypesNotFound,
    AssertionResponseTypesMethodNotAllowed,
    AssertionResponseTypesConflict,
    AssertionResponseTypesGone,
    AssertionResponseTypesPreconditionFailed,
    AssertionResponseTypesUnprocessable);
  TFhirAssertionResponseTypesEnumList = set of TFhirAssertionResponseTypesEnum;


  // Indicator for type of action performed during the event that generated the event. (from http://hl7.org/fhir/ValueSet/audit-event-action)
  TFhirAuditEventActionEnum = (
    AuditEventActionNull, // Value is missing from Instance
    AuditEventActionC,
    AuditEventActionR,
    AuditEventActionU,
    AuditEventActionD,
    AuditEventActionE);
  TFhirAuditEventActionEnumList = set of TFhirAuditEventActionEnum;


  // The severity of the audit entry. (from http://hl7.org/fhir/ValueSet/audit-event-severity)
  TFhirAuditEventSeverityEnum = (
    AuditEventSeverityNull, // Value is missing from Instance
    AuditEventSeverityEmergency,
    AuditEventSeverityAlert,
    AuditEventSeverityCritical,
    AuditEventSeverityError,
    AuditEventSeverityWarning,
    AuditEventSeverityNotice,
    AuditEventSeverityInformational,
    AuditEventSeverityDebug);
  TFhirAuditEventSeverityEnumList = set of TFhirAuditEventSeverityEnum;


  // Indication of the degree of conformance expectations associated with a binding. (from http://hl7.org/fhir/ValueSet/binding-strength)
  TFhirBindingStrengthEnum = (
    BindingStrengthNull, // Value is missing from Instance
    BindingStrengthRequired,
    BindingStrengthExtensible,
    BindingStrengthPreferred,
    BindingStrengthExample);
  TFhirBindingStrengthEnumList = set of TFhirBindingStrengthEnum;


  // Indicates the purpose of a bundle - how it is intended to be used. (from http://hl7.org/fhir/ValueSet/bundle-type)
  TFhirBundleTypeEnum = (
    BundleTypeNull, // Value is missing from Instance
    BundleTypeDocument,
    BundleTypeMessage,
    BundleTypeTransaction,
    BundleTypeTransactionResponse,
    BundleTypeBatch,
    BundleTypeBatchResponse,
    BundleTypeHistory,
    BundleTypeSearchset,
    BundleTypeCollection,
    BundleTypeSubscriptionNotification);
  TFhirBundleTypeEnumList = set of TFhirBundleTypeEnum;


  // How a capability statement is intended to be used. (from http://hl7.org/fhir/ValueSet/capability-statement-kind)
  TFhirCapabilityStatementKindEnum = (
    CapabilityStatementKindNull, // Value is missing from Instance
    CapabilityStatementKindInstance,
    CapabilityStatementKindCapability,
    CapabilityStatementKindRequirements);
  TFhirCapabilityStatementKindEnumList = set of TFhirCapabilityStatementKindEnum;


  // Resource types defined as part of FHIR that can be represented as in-line definitions of a care plan activity. (from http://hl7.org/fhir/ValueSet/care-plan-activity-kind)
  TFhirCarePlanActivityKindEnum = (
    CarePlanActivityKindNull, // Value is missing from Instance
    CarePlanActivityKindAppointment,
    CarePlanActivityKindCommunicationRequest,
    CarePlanActivityKindDeviceRequest,
    CarePlanActivityKindMedicationRequest,
    CarePlanActivityKindNutritionOrder,
    CarePlanActivityKindTask,
    CarePlanActivityKindServiceRequest,
    CarePlanActivityKindVisionPrescription);
  TFhirCarePlanActivityKindEnumList = set of TFhirCarePlanActivityKindEnum;


  // Codes that reflect the current state of a care plan activity within its overall life cycle. (from http://hl7.org/fhir/ValueSet/care-plan-activity-status)
  TFhirCarePlanActivityStatusEnum = (
    CarePlanActivityStatusNull, // Value is missing from Instance
    CarePlanActivityStatusNotStarted,
    CarePlanActivityStatusScheduled,
    CarePlanActivityStatusInProgress,
    CarePlanActivityStatusOnHold,
    CarePlanActivityStatusCompleted,
    CarePlanActivityStatusCancelled,
    CarePlanActivityStatusStopped,
    CarePlanActivityStatusUnknown,
    CarePlanActivityStatusEnteredInError);
  TFhirCarePlanActivityStatusEnumList = set of TFhirCarePlanActivityStatusEnum;


  // Codes indicating the degree of authority/intentionality associated with a care plan. (from http://hl7.org/fhir/ValueSet/care-plan-intent)
  TFhirCarePlanIntentEnum = (
    CarePlanIntentNull, // Value is missing from Instance
    CarePlanIntentProposal,
    CarePlanIntentPlan,
    CarePlanIntentOrder,
    CarePlanIntentOption,
    CarePlanIntentDirective);
  TFhirCarePlanIntentEnumList = set of TFhirCarePlanIntentEnum;


  // Indicates the status of the care team. (from http://hl7.org/fhir/ValueSet/care-team-status)
  TFhirCareTeamStatusEnum = (
    CareTeamStatusNull, // Value is missing from Instance
    CareTeamStatusProposed,
    CareTeamStatusActive,
    CareTeamStatusSuspended,
    CareTeamStatusInactive,
    CareTeamStatusEnteredInError);
  TFhirCareTeamStatusEnumList = set of TFhirCareTeamStatusEnum;


  // Logical grouping of characteristics. (from http://hl7.org/fhir/ValueSet/characteristic-combination)
  TFhirCharacteristicCombinationEnum = (
    CharacteristicCombinationNull, // Value is missing from Instance
    CharacteristicCombinationAllOf,
    CharacteristicCombinationAnyOf,
    CharacteristicCombinationAtLeast,
    CharacteristicCombinationAtMost,
    CharacteristicCombinationStatistical,
    CharacteristicCombinationNetEffect,
    CharacteristicCombinationDataset);
  TFhirCharacteristicCombinationEnumList = set of TFhirCharacteristicCombinationEnum;


  // Codes identifying the lifecycle stage of a ChargeItem. (from http://hl7.org/fhir/ValueSet/chargeitem-status)
  TFhirChargeItemStatusEnum = (
    ChargeItemStatusNull, // Value is missing from Instance
    ChargeItemStatusPlanned,
    ChargeItemStatusBillable,
    ChargeItemStatusNotBillable,
    ChargeItemStatusAborted,
    ChargeItemStatusBilled,
    ChargeItemStatusEnteredInError,
    ChargeItemStatusUnknown);
  TFhirChargeItemStatusEnumList = set of TFhirChargeItemStatusEnum;


  // This value set includes Claim Processing Outcome codes. (from http://hl7.org/fhir/ValueSet/claim-outcome)
  TFhirClaimProcessingCodesEnum = (
    ClaimProcessingCodesNull, // Value is missing from Instance
    ClaimProcessingCodesQueued,
    ClaimProcessingCodesComplete,
    ClaimProcessingCodesError,
    ClaimProcessingCodesPartial);
  TFhirClaimProcessingCodesEnumList = set of TFhirClaimProcessingCodesEnum;


  // Overall defining type of this clinical use definition. (from http://hl7.org/fhir/ValueSet/clinical-use-definition-type)
  TFhirClinicalUseDefinitionTypeEnum = (
    ClinicalUseDefinitionTypeNull, // Value is missing from Instance
    ClinicalUseDefinitionTypeIndication,
    ClinicalUseDefinitionTypeContraindication,
    ClinicalUseDefinitionTypeInteraction,
    ClinicalUseDefinitionTypeUndesirableEffect,
    ClinicalUseDefinitionTypeWarning);
  TFhirClinicalUseDefinitionTypeEnumList = set of TFhirClinicalUseDefinitionTypeEnum;


  // The degree to which the server supports the code search parameter on ValueSet, if it is supported. (from http://hl7.org/fhir/ValueSet/code-search-support)
  TFhirCodeSearchSupportEnum = (
    CodeSearchSupportNull, // Value is missing from Instance
    CodeSearchSupportInCompose,
    CodeSearchSupportInExpansion,
    CodeSearchSupportInComposeOrExpansion);
  TFhirCodeSearchSupportEnumList = set of TFhirCodeSearchSupportEnum;


  // The extent of the content of the code system (the concepts and codes it defines) are represented in a code system resource. (from http://hl7.org/fhir/ValueSet/codesystem-content-mode)
  TFhirCodeSystemContentModeEnum = (
    CodeSystemContentModeNull, // Value is missing from Instance
    CodeSystemContentModeNotPresent,
    CodeSystemContentModeExample,
    CodeSystemContentModeFragment,
    CodeSystemContentModeComplete,
    CodeSystemContentModeSupplement);
  TFhirCodeSystemContentModeEnumList = set of TFhirCodeSystemContentModeEnum;


  // The meaning of the hierarchy of concepts in a code system. (from http://hl7.org/fhir/ValueSet/codesystem-hierarchy-meaning)
  TFhirCodeSystemHierarchyMeaningEnum = (
    CodeSystemHierarchyMeaningNull, // Value is missing from Instance
    CodeSystemHierarchyMeaningGroupedBy,
    CodeSystemHierarchyMeaningIsA,
    CodeSystemHierarchyMeaningPartOf,
    CodeSystemHierarchyMeaningClassifiedWith);
  TFhirCodeSystemHierarchyMeaningEnumList = set of TFhirCodeSystemHierarchyMeaningEnum;


  // This value set includes common codes from BCP-47 (http://tools.ietf.org/html/bcp47) (from http://hl7.org/fhir/ValueSet/languages)
  TFhirCommonLanguagesEnum = (
    CommonLanguagesNull, // Value is missing from Instance
    CommonLanguagesAr,
    CommonLanguagesBn,
    CommonLanguagesCs,
    CommonLanguagesDa,
    CommonLanguagesDe,
    CommonLanguagesDeAT,
    CommonLanguagesDeCH,
    CommonLanguagesDeDE,
    CommonLanguagesEl,
    CommonLanguagesEn,
    CommonLanguagesEnAU,
    CommonLanguagesEnCA,
    CommonLanguagesEnGB,
    CommonLanguagesEnIN,
    CommonLanguagesEnNZ,
    CommonLanguagesEnSG,
    CommonLanguagesEnUS,
    CommonLanguagesEs,
    CommonLanguagesEsAR,
    CommonLanguagesEsES,
    CommonLanguagesEsUY,
    CommonLanguagesFi,
    CommonLanguagesFr,
    CommonLanguagesFrBE,
    CommonLanguagesFrCH,
    CommonLanguagesFrFR,
    CommonLanguagesFrCA,
    CommonLanguagesFy,
    CommonLanguagesFyNL,
    CommonLanguagesHi,
    CommonLanguagesHr,
    CommonLanguagesIt,
    CommonLanguagesItCH,
    CommonLanguagesItIT,
    CommonLanguagesJa,
    CommonLanguagesKo,
    CommonLanguagesNl,
    CommonLanguagesNlBE,
    CommonLanguagesNlNL,
    CommonLanguagesNo,
    CommonLanguagesNoNO,
    CommonLanguagesPa,
    CommonLanguagesPl,
    CommonLanguagesPt,
    CommonLanguagesPtBR,
    CommonLanguagesRu,
    CommonLanguagesRuRU,
    CommonLanguagesSr,
    CommonLanguagesSrRS,
    CommonLanguagesSv,
    CommonLanguagesSvSE,
    CommonLanguagesTe,
    CommonLanguagesZh,
    CommonLanguagesZhCN,
    CommonLanguagesZhHK,
    CommonLanguagesZhSG,
    CommonLanguagesZhTW);
  TFhirCommonLanguagesEnumList = set of TFhirCommonLanguagesEnum;


  // Which type a compartment definition describes. (from http://hl7.org/fhir/ValueSet/compartment-type)
  TFhirCompartmentTypeEnum = (
    CompartmentTypeNull, // Value is missing from Instance
    CompartmentTypePatient,
    CompartmentTypeEncounter,
    CompartmentTypeRelatedPerson,
    CompartmentTypePractitioner,
    CompartmentTypeDevice);
  TFhirCompartmentTypeEnumList = set of TFhirCompartmentTypeEnum;


  // The workflow/clinical status of the composition. (from http://hl7.org/fhir/ValueSet/composition-status)
  TFhirCompositionStatusEnum = (
    CompositionStatusNull, // Value is missing from Instance
    CompositionStatusRegistered,
    CompositionStatusPartial,
    CompositionStatusPreliminary,
    CompositionStatusFinal,
    CompositionStatusAmended,
    CompositionStatusCorrected,
    CompositionStatusAppended,
    CompositionStatusCancelled,
    CompositionStatusEnteredInError,
    CompositionStatusDeprecated,
    CompositionStatusUnknown);
  TFhirCompositionStatusEnumList = set of TFhirCompositionStatusEnum;


  // Defines which action to take if there is no match in the group. (from http://hl7.org/fhir/ValueSet/conceptmap-unmapped-mode)
  TFhirConceptMapGroupUnmappedModeEnum = (
    ConceptMapGroupUnmappedModeNull, // Value is missing from Instance
    ConceptMapGroupUnmappedModeUseSourceCode,
    ConceptMapGroupUnmappedModeFixed,
    ConceptMapGroupUnmappedModeOtherMap);
  TFhirConceptMapGroupUnmappedModeEnumList = set of TFhirConceptMapGroupUnmappedModeEnum;


  // The relationship between concepts. (from http://hl7.org/fhir/ValueSet/concept-map-relationship)
  TFhirConceptMapRelationshipEnum = (
    ConceptMapRelationshipNull, // Value is missing from Instance
    ConceptMapRelationshipRelatedTo,
    ConceptMapRelationshipEquivalent,
    ConceptMapRelationshipSourceIsNarrowerThanTarget,
    ConceptMapRelationshipSourceIsBroaderThanTarget,
    ConceptMapRelationshipNotRelatedTo);
  TFhirConceptMapRelationshipEnumList = set of TFhirConceptMapRelationshipEnum;


  // The type of a property value. (from http://hl7.org/fhir/ValueSet/concept-property-type)
  TFhirConceptPropertyTypeEnum = (
    ConceptPropertyTypeNull, // Value is missing from Instance
    ConceptPropertyTypeCode,
    ConceptPropertyTypeCoding,
    ConceptPropertyTypeString,
    ConceptPropertyTypeInteger,
    ConceptPropertyTypeBoolean,
    ConceptPropertyTypeDateTime,
    ConceptPropertyTypeDecimal);
  TFhirConceptPropertyTypeEnumList = set of TFhirConceptPropertyTypeEnum;


  // Kind of precondition for the condition. (from http://hl7.org/fhir/ValueSet/condition-precondition-type)
  TFhirConditionPreconditionTypeEnum = (
    ConditionPreconditionTypeNull, // Value is missing from Instance
    ConditionPreconditionTypeSensitive,
    ConditionPreconditionTypeSpecific);
  TFhirConditionPreconditionTypeEnumList = set of TFhirConditionPreconditionTypeEnum;


  // The use of a questionnaire. (from http://hl7.org/fhir/ValueSet/condition-questionnaire-purpose)
  TFhirConditionQuestionnairePurposeEnum = (
    ConditionQuestionnairePurposeNull, // Value is missing from Instance
    ConditionQuestionnairePurposePreadmit,
    ConditionQuestionnairePurposeDiffDiagnosis,
    ConditionQuestionnairePurposeOutcome);
  TFhirConditionQuestionnairePurposeEnumList = set of TFhirConditionQuestionnairePurposeEnum;


  // A code that indicates how the server supports conditional delete. (from http://hl7.org/fhir/ValueSet/conditional-delete-status)
  TFhirConditionalDeleteStatusEnum = (
    ConditionalDeleteStatusNull, // Value is missing from Instance
    ConditionalDeleteStatusNotSupported,
    ConditionalDeleteStatusSingle,
    ConditionalDeleteStatusMultiple);
  TFhirConditionalDeleteStatusEnumList = set of TFhirConditionalDeleteStatusEnum;


  // A code that indicates how the server supports conditional read. (from http://hl7.org/fhir/ValueSet/conditional-read-status)
  TFhirConditionalReadStatusEnum = (
    ConditionalReadStatusNull, // Value is missing from Instance
    ConditionalReadStatusNotSupported,
    ConditionalReadStatusModifiedSince,
    ConditionalReadStatusNotMatch,
    ConditionalReadStatusFullSupport);
  TFhirConditionalReadStatusEnumList = set of TFhirConditionalReadStatusEnum;


  // Description Needed Here (from http://hl7.org/fhir/ValueSet/conformance-expectation)
  TFhirConformanceExpectationEnum = (
    ConformanceExpectationNull, // Value is missing from Instance
    ConformanceExpectationSHALL,
    ConformanceExpectationSHOULD,
    ConformanceExpectationMAY,
    ConformanceExpectationSHOULDNOT);
  TFhirConformanceExpectationEnumList = set of TFhirConformanceExpectationEnum;


  // How a resource reference is interpreted when testing consent restrictions. (from http://hl7.org/fhir/ValueSet/consent-data-meaning)
  TFhirConsentDataMeaningEnum = (
    ConsentDataMeaningNull, // Value is missing from Instance
    ConsentDataMeaningInstance,
    ConsentDataMeaningRelated,
    ConsentDataMeaningDependents,
    ConsentDataMeaningAuthoredby);
  TFhirConsentDataMeaningEnumList = set of TFhirConsentDataMeaningEnum;


  // How a rule statement is applied, such as adding additional consent or removing consent. (from http://hl7.org/fhir/ValueSet/consent-provision-type)
  TFhirConsentProvisionTypeEnum = (
    ConsentProvisionTypeNull, // Value is missing from Instance
    ConsentProvisionTypeDeny,
    ConsentProvisionTypePermit);
  TFhirConsentProvisionTypeEnumList = set of TFhirConsentProvisionTypeEnum;


  // Indicates the state of the consent. (from http://hl7.org/fhir/ValueSet/consent-state-codes)
  TFhirConsentStateEnum = (
    ConsentStateNull, // Value is missing from Instance
    ConsentStateDraft,
    ConsentStateActive,
    ConsentStateInactive,
    ConsentStateNotDone,
    ConsentStateEnteredInError,
    ConsentStateUnknown);
  TFhirConsentStateEnumList = set of TFhirConsentStateEnum;


  // SHALL applications comply with this constraint? (from http://hl7.org/fhir/ValueSet/constraint-severity)
  TFhirConstraintSeverityEnum = (
    ConstraintSeverityNull, // Value is missing from Instance
    ConstraintSeverityError,
    ConstraintSeverityWarning);
  TFhirConstraintSeverityEnumList = set of TFhirConstraintSeverityEnum;


  // Telecommunications form for contact point. (from http://hl7.org/fhir/ValueSet/contact-point-system)
  TFhirContactPointSystemEnum = (
    ContactPointSystemNull, // Value is missing from Instance
    ContactPointSystemPhone,
    ContactPointSystemFax,
    ContactPointSystemEmail,
    ContactPointSystemPager,
    ContactPointSystemUrl,
    ContactPointSystemSms,
    ContactPointSystemOther);
  TFhirContactPointSystemEnumList = set of TFhirContactPointSystemEnum;


  // Use of contact point. (from http://hl7.org/fhir/ValueSet/contact-point-use)
  TFhirContactPointUseEnum = (
    ContactPointUseNull, // Value is missing from Instance
    ContactPointUseHome,
    ContactPointUseWork,
    ContactPointUseTemp,
    ContactPointUseOld,
    ContactPointUseMobile);
  TFhirContactPointUseEnumList = set of TFhirContactPointUseEnum;


  // This value set contract specific codes for status. (from http://hl7.org/fhir/ValueSet/contract-publicationstatus)
  TFhirContractResourcePublicationStatusCodesEnum = (
    ContractResourcePublicationStatusCodesNull, // Value is missing from Instance
    ContractResourcePublicationStatusCodesAmended,
    ContractResourcePublicationStatusCodesAppended,
    ContractResourcePublicationStatusCodesCancelled,
    ContractResourcePublicationStatusCodesDisputed,
    ContractResourcePublicationStatusCodesEnteredInError,
    ContractResourcePublicationStatusCodesExecutable,
    ContractResourcePublicationStatusCodesExecuted,
    ContractResourcePublicationStatusCodesNegotiable,
    ContractResourcePublicationStatusCodesOffered,
    ContractResourcePublicationStatusCodesPolicy,
    ContractResourcePublicationStatusCodesRejected,
    ContractResourcePublicationStatusCodesRenewed,
    ContractResourcePublicationStatusCodesRevoked,
    ContractResourcePublicationStatusCodesResolved,
    ContractResourcePublicationStatusCodesTerminated);
  TFhirContractResourcePublicationStatusCodesEnumList = set of TFhirContractResourcePublicationStatusCodesEnum;


  // This value set contract specific codes for status. (from http://hl7.org/fhir/ValueSet/contract-status)
  TFhirContractResourceStatusCodesEnum = (
    ContractResourceStatusCodesNull, // Value is missing from Instance
    ContractResourceStatusCodesAmended,
    ContractResourceStatusCodesAppended,
    ContractResourceStatusCodesCancelled,
    ContractResourceStatusCodesDisputed,
    ContractResourceStatusCodesEnteredInError,
    ContractResourceStatusCodesExecutable,
    ContractResourceStatusCodesExecuted,
    ContractResourceStatusCodesNegotiable,
    ContractResourceStatusCodesOffered,
    ContractResourceStatusCodesPolicy,
    ContractResourceStatusCodesRejected,
    ContractResourceStatusCodesRenewed,
    ContractResourceStatusCodesRevoked,
    ContractResourceStatusCodesResolved,
    ContractResourceStatusCodesTerminated);
  TFhirContractResourceStatusCodesEnumList = set of TFhirContractResourceStatusCodesEnum;


  // The type of contributor. (from http://hl7.org/fhir/ValueSet/contributor-type)
  TFhirContributorTypeEnum = (
    ContributorTypeNull, // Value is missing from Instance
    ContributorTypeAuthor,
    ContributorTypeEditor,
    ContributorTypeReviewer,
    ContributorTypeEndorser);
  TFhirContributorTypeEnumList = set of TFhirContributorTypeEnum;


  // Behavior a server can exhibit when a criteria state does not exist (e.g., state prior to a create or after a delete). (from http://hl7.org/fhir/ValueSet/subscriptiontopic-cr-behavior)
  TFhirCriteriaNotExistsBehaviorEnum = (
    CriteriaNotExistsBehaviorNull, // Value is missing from Instance
    CriteriaNotExistsBehaviorTestPasses,
    CriteriaNotExistsBehaviorTestFails);
  TFhirCriteriaNotExistsBehaviorEnumList = set of TFhirCriteriaNotExistsBehaviorEnum;


  // The days of the week. (from http://hl7.org/fhir/ValueSet/days-of-week)
  TFhirDaysOfWeekEnum = (
    DaysOfWeekNull, // Value is missing from Instance
    DaysOfWeekMon,
    DaysOfWeekTue,
    DaysOfWeekWed,
    DaysOfWeekThu,
    DaysOfWeekFri,
    DaysOfWeekSat,
    DaysOfWeekSun);
  TFhirDaysOfWeekEnumList = set of TFhirDaysOfWeekEnum;


  // Indicates the potential degree of impact of the identified issue on the patient. (from http://hl7.org/fhir/ValueSet/detectedissue-severity)
  TFhirDetectedIssueSeverityEnum = (
    DetectedIssueSeverityNull, // Value is missing from Instance
    DetectedIssueSeverityHigh,
    DetectedIssueSeverityModerate,
    DetectedIssueSeverityLow);
  TFhirDetectedIssueSeverityEnumList = set of TFhirDetectedIssueSeverityEnum;


  // Indicates the status of a detected issue (from http://hl7.org/fhir/ValueSet/detectedissue-status)
  TFhirDetectedIssueStatusEnum = (
    DetectedIssueStatusNull, // Value is missing from Instance
    DetectedIssueStatusPreliminary,
    DetectedIssueStatusFinal,
    DetectedIssueStatusEnteredInError,
    DetectedIssueStatusMitigated);
  TFhirDetectedIssueStatusEnumList = set of TFhirDetectedIssueStatusEnum;


  // Device - Corrective action scope (from http://hl7.org/fhir/ValueSet/device-correctiveactionscope)
  TFhirDeviceCorrectiveActionScopeEnum = (
    DeviceCorrectiveActionScopeNull, // Value is missing from Instance
    DeviceCorrectiveActionScopeModel,
    DeviceCorrectiveActionScopeLotNumbers,
    DeviceCorrectiveActionScopeSerialNumbers);
  TFhirDeviceCorrectiveActionScopeEnumList = set of TFhirDeviceCorrectiveActionScopeEnum;


  // Regulatory Identifier type (from http://hl7.org/fhir/ValueSet/devicedefinition-regulatory-identifier-type)
  TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum = (
    DeviceDefinitionRegulatoryIdentifierTypeNull, // Value is missing from Instance
    DeviceDefinitionRegulatoryIdentifierTypeBasic,
    DeviceDefinitionRegulatoryIdentifierTypeMaster,
    DeviceDefinitionRegulatoryIdentifierTypeLicense);
  TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList = set of TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum;


  // DeviceDispense Status Codes (from http://hl7.org/fhir/ValueSet/devicedispense-status)
  TFhirDeviceDispenseStatusCodesEnum = (
    DeviceDispenseStatusCodesNull, // Value is missing from Instance
    DeviceDispenseStatusCodesPreparation,
    DeviceDispenseStatusCodesInProgress,
    DeviceDispenseStatusCodesCancelled,
    DeviceDispenseStatusCodesOnHold,
    DeviceDispenseStatusCodesCompleted,
    DeviceDispenseStatusCodesEnteredInError,
    DeviceDispenseStatusCodesStopped,
    DeviceDispenseStatusCodesDeclined,
    DeviceDispenseStatusCodesUnknown);
  TFhirDeviceDispenseStatusCodesEnumList = set of TFhirDeviceDispenseStatusCodesEnum;


  // Describes the state of a metric calibration. (from http://hl7.org/fhir/ValueSet/metric-calibration-state)
  TFhirDeviceMetricCalibrationStateEnum = (
    DeviceMetricCalibrationStateNull, // Value is missing from Instance
    DeviceMetricCalibrationStateNotCalibrated,
    DeviceMetricCalibrationStateCalibrationRequired,
    DeviceMetricCalibrationStateCalibrated,
    DeviceMetricCalibrationStateUnspecified);
  TFhirDeviceMetricCalibrationStateEnumList = set of TFhirDeviceMetricCalibrationStateEnum;


  // Describes the type of a metric calibration. (from http://hl7.org/fhir/ValueSet/metric-calibration-type)
  TFhirDeviceMetricCalibrationTypeEnum = (
    DeviceMetricCalibrationTypeNull, // Value is missing from Instance
    DeviceMetricCalibrationTypeUnspecified,
    DeviceMetricCalibrationTypeOffset,
    DeviceMetricCalibrationTypeGain,
    DeviceMetricCalibrationTypeTwoPoint);
  TFhirDeviceMetricCalibrationTypeEnumList = set of TFhirDeviceMetricCalibrationTypeEnum;


  // Describes the category of the metric. (from http://hl7.org/fhir/ValueSet/metric-category)
  TFhirDeviceMetricCategoryEnum = (
    DeviceMetricCategoryNull, // Value is missing from Instance
    DeviceMetricCategoryMeasurement,
    DeviceMetricCategorySetting,
    DeviceMetricCategoryCalculation,
    DeviceMetricCategoryUnspecified);
  TFhirDeviceMetricCategoryEnumList = set of TFhirDeviceMetricCategoryEnum;


  // Describes the typical color of representation. (from http://hl7.org/fhir/ValueSet/metric-color)
  TFhirDeviceMetricColorEnum = (
    DeviceMetricColorNull, // Value is missing from Instance
    DeviceMetricColorBlack,
    DeviceMetricColorRed,
    DeviceMetricColorGreen,
    DeviceMetricColorYellow,
    DeviceMetricColorBlue,
    DeviceMetricColorMagenta,
    DeviceMetricColorCyan,
    DeviceMetricColorWhite);
  TFhirDeviceMetricColorEnumList = set of TFhirDeviceMetricColorEnum;


  // Describes the operational status of the DeviceMetric. (from http://hl7.org/fhir/ValueSet/metric-operational-status)
  TFhirDeviceMetricOperationalStatusEnum = (
    DeviceMetricOperationalStatusNull, // Value is missing from Instance
    DeviceMetricOperationalStatusOn,
    DeviceMetricOperationalStatusOff,
    DeviceMetricOperationalStatusStandby,
    DeviceMetricOperationalStatusEnteredInError);
  TFhirDeviceMetricOperationalStatusEnumList = set of TFhirDeviceMetricOperationalStatusEnum;


  // The type of name the device is referred by. (from http://hl7.org/fhir/ValueSet/device-nametype)
  TFhirDeviceNameTypeEnum = (
    DeviceNameTypeNull, // Value is missing from Instance
    DeviceNameTypeRegisteredName,
    DeviceNameTypeUserFriendlyName,
    DeviceNameTypePatientReportedName);
  TFhirDeviceNameTypeEnumList = set of TFhirDeviceNameTypeEnum;


  // Device Production Identifier in UDI (from http://hl7.org/fhir/ValueSet/device-productidentifierinudi)
  TFhirDeviceProductionIdentifierInUDIEnum = (
    DeviceProductionIdentifierInUDINull, // Value is missing from Instance
    DeviceProductionIdentifierInUDILotNumber,
    DeviceProductionIdentifierInUDIManufacturedDate,
    DeviceProductionIdentifierInUDISerialNumber,
    DeviceProductionIdentifierInUDIExpirationDate,
    DeviceProductionIdentifierInUDIBiologicalSource,
    DeviceProductionIdentifierInUDISoftwareVersion);
  TFhirDeviceProductionIdentifierInUDIEnumList = set of TFhirDeviceProductionIdentifierInUDIEnum;


  // A coded concept indicating the current status of the Device Usage. (from http://hl7.org/fhir/ValueSet/deviceusage-status)
  TFhirDeviceUsageStatusEnum = (
    DeviceUsageStatusNull, // Value is missing from Instance
    DeviceUsageStatusActive,
    DeviceUsageStatusCompleted,
    DeviceUsageStatusNotDone,
    DeviceUsageStatusEnteredInError,
    DeviceUsageStatusIntended,
    DeviceUsageStatusStopped,
    DeviceUsageStatusOnHold);
  TFhirDeviceUsageStatusEnumList = set of TFhirDeviceUsageStatusEnum;


  // The status of the diagnostic report. (from http://hl7.org/fhir/ValueSet/diagnostic-report-status)
  TFhirDiagnosticReportStatusEnum = (
    DiagnosticReportStatusNull, // Value is missing from Instance
    DiagnosticReportStatusRegistered,
    DiagnosticReportStatusPartial,
    DiagnosticReportStatusPreliminary,
    DiagnosticReportStatusFinal,
    DiagnosticReportStatusAmended,
    DiagnosticReportStatusCorrected,
    DiagnosticReportStatusAppended,
    DiagnosticReportStatusCancelled,
    DiagnosticReportStatusEnteredInError,
    DiagnosticReportStatusUnknown);
  TFhirDiagnosticReportStatusEnumList = set of TFhirDiagnosticReportStatusEnum;


  // How an element value is interpreted when discrimination is evaluated. (from http://hl7.org/fhir/ValueSet/discriminator-type)
  TFhirDiscriminatorTypeEnum = (
    DiscriminatorTypeNull, // Value is missing from Instance
    DiscriminatorTypeValue,
    DiscriminatorTypeExists,
    DiscriminatorTypePattern,
    DiscriminatorTypeType,
    DiscriminatorTypeProfile,
    DiscriminatorTypePosition);
  TFhirDiscriminatorTypeEnumList = set of TFhirDiscriminatorTypeEnum;


  // Whether the application produces or consumes documents. (from http://hl7.org/fhir/ValueSet/document-mode)
  TFhirDocumentModeEnum = (
    DocumentModeNull, // Value is missing from Instance
    DocumentModeProducer,
    DocumentModeConsumer);
  TFhirDocumentModeEnumList = set of TFhirDocumentModeEnum;


  // The status of the document reference. (from http://hl7.org/fhir/ValueSet/document-reference-status)
  TFhirDocumentReferenceStatusEnum = (
    DocumentReferenceStatusNull, // Value is missing from Instance
    DocumentReferenceStatusCurrent,
    DocumentReferenceStatusSuperseded,
    DocumentReferenceStatusEnteredInError);
  TFhirDocumentReferenceStatusEnumList = set of TFhirDocumentReferenceStatusEnum;


  // The outcome of the processing. (from http://hl7.org/fhir/ValueSet/eligibility-outcome)
  TFhirEligibilityOutcomeEnum = (
    EligibilityOutcomeNull, // Value is missing from Instance
    EligibilityOutcomeQueued,
    EligibilityOutcomeComplete,
    EligibilityOutcomeError,
    EligibilityOutcomePartial);
  TFhirEligibilityOutcomeEnumList = set of TFhirEligibilityOutcomeEnum;


  // A code specifying the types of information being requested. (from http://hl7.org/fhir/ValueSet/eligibilityrequest-purpose)
  TFhirEligibilityRequestPurposeEnum = (
    EligibilityRequestPurposeNull, // Value is missing from Instance
    EligibilityRequestPurposeAuthRequirements,
    EligibilityRequestPurposeBenefits,
    EligibilityRequestPurposeDiscovery,
    EligibilityRequestPurposeValidation);
  TFhirEligibilityRequestPurposeEnumList = set of TFhirEligibilityRequestPurposeEnum;


  // A code specifying the types of information being requested. (from http://hl7.org/fhir/ValueSet/eligibilityresponse-purpose)
  TFhirEligibilityResponsePurposeEnum = (
    EligibilityResponsePurposeNull, // Value is missing from Instance
    EligibilityResponsePurposeAuthRequirements,
    EligibilityResponsePurposeBenefits,
    EligibilityResponsePurposeDiscovery,
    EligibilityResponsePurposeValidation);
  TFhirEligibilityResponsePurposeEnumList = set of TFhirEligibilityResponsePurposeEnum;


  // Controls how multiple enableWhen values are interpreted -  whether all or any must be true. (from http://hl7.org/fhir/ValueSet/questionnaire-enable-behavior)
  TFhirEnableWhenBehaviorEnum = (
    EnableWhenBehaviorNull, // Value is missing from Instance
    EnableWhenBehaviorAll,
    EnableWhenBehaviorAny);
  TFhirEnableWhenBehaviorEnumList = set of TFhirEnableWhenBehaviorEnum;


  // The status of the location. (from http://hl7.org/fhir/ValueSet/encounter-location-status)
  TFhirEncounterLocationStatusEnum = (
    EncounterLocationStatusNull, // Value is missing from Instance
    EncounterLocationStatusPlanned,
    EncounterLocationStatusActive,
    EncounterLocationStatusReserved,
    EncounterLocationStatusCompleted);
  TFhirEncounterLocationStatusEnumList = set of TFhirEncounterLocationStatusEnum;


  // Current state of the encounter. (from http://hl7.org/fhir/ValueSet/encounter-status)
  TFhirEncounterStatusEnum = (
    EncounterStatusNull, // Value is missing from Instance
    EncounterStatusPlanned,
    EncounterStatusInProgress,
    EncounterStatusOnhold,
    EncounterStatusDischarged,
    EncounterStatusCompleted,
    EncounterStatusCancelled,
    EncounterStatusDiscontinued,
    EncounterStatusEnteredInError,
    EncounterStatusUnknown);
  TFhirEncounterStatusEnumList = set of TFhirEncounterStatusEnum;


  // The status of the endpoint. (from http://hl7.org/fhir/ValueSet/endpoint-status)
  TFhirEndpointStatusEnum = (
    EndpointStatusNull, // Value is missing from Instance
    EndpointStatusActive,
    EndpointStatusSuspended,
    EndpointStatusError,
    EndpointStatusOff,
    EndpointStatusEnteredInError);
  TFhirEndpointStatusEnumList = set of TFhirEndpointStatusEnum;


  // The outcome of the processing. (from http://hl7.org/fhir/ValueSet/enrollment-outcome)
  TFhirEnrollmentOutcomeEnum = (
    EnrollmentOutcomeNull, // Value is missing from Instance
    EnrollmentOutcomeQueued,
    EnrollmentOutcomeComplete,
    EnrollmentOutcomeError,
    EnrollmentOutcomePartial);
  TFhirEnrollmentOutcomeEnumList = set of TFhirEnrollmentOutcomeEnum;


  // The status of the episode of care. (from http://hl7.org/fhir/ValueSet/episode-of-care-status)
  TFhirEpisodeOfCareStatusEnum = (
    EpisodeOfCareStatusNull, // Value is missing from Instance
    EpisodeOfCareStatusPlanned,
    EpisodeOfCareStatusWaitlist,
    EpisodeOfCareStatusActive,
    EpisodeOfCareStatusOnhold,
    EpisodeOfCareStatusFinished,
    EpisodeOfCareStatusCancelled,
    EpisodeOfCareStatusEnteredInError);
  TFhirEpisodeOfCareStatusEnumList = set of TFhirEpisodeOfCareStatusEnum;


  // The mode of a message capability statement. (from http://hl7.org/fhir/ValueSet/event-capability-mode)
  TFhirEventCapabilityModeEnum = (
    EventCapabilityModeNull, // Value is missing from Instance
    EventCapabilityModeSender,
    EventCapabilityModeReceiver);
  TFhirEventCapabilityModeEnumList = set of TFhirEventCapabilityModeEnum;


  // Codes identifying the lifecycle stage of an event. (from http://hl7.org/fhir/ValueSet/event-status)
  TFhirEventStatusEnum = (
    EventStatusNull, // Value is missing from Instance
    EventStatusPreparation,
    EventStatusInProgress,
    EventStatusNotDone,
    EventStatusOnHold,
    EventStatusStopped,
    EventStatusCompleted,
    EventStatusEnteredInError,
    EventStatusUnknown);
  TFhirEventStatusEnumList = set of TFhirEventStatusEnum;


  // Real world event relating to the schedule. (from http://hl7.org/fhir/ValueSet/event-timing)
  TFhirEventTimingEnum = (
    EventTimingNull, // Value is missing from Instance
    EventTimingMORN,
    EventTimingMORNEarly,
    EventTimingMORNLate,
    EventTimingNOON,
    EventTimingAFT,
    EventTimingAFTEarly,
    EventTimingAFTLate,
    EventTimingEVE,
    EventTimingEVEEarly,
    EventTimingEVELate,
    EventTimingNIGHT,
    EventTimingPHS,
    EventTimingIMD,
    EventTimingHS,
    EventTimingWAKE,
    EventTimingC,
    EventTimingCM,
    EventTimingCD,
    EventTimingCV,
    EventTimingAC,
    EventTimingACM,
    EventTimingACD,
    EventTimingACV,
    EventTimingPC,
    EventTimingPCM,
    EventTimingPCD,
    EventTimingPCV);
  TFhirEventTimingEnumList = set of TFhirEventTimingEnum;


  // The handling of the variable in statistical analysis for exposures or outcomes (E.g. Dichotomous, Continuous, Descriptive). (from http://hl7.org/fhir/ValueSet/variable-handling)
  TFhirEvidenceVariableHandlingEnum = (
    EvidenceVariableHandlingNull, // Value is missing from Instance
    EvidenceVariableHandlingContinuous,
    EvidenceVariableHandlingDichotomous,
    EvidenceVariableHandlingOrdinal,
    EvidenceVariableHandlingPolychotomous);
  TFhirEvidenceVariableHandlingEnumList = set of TFhirEvidenceVariableHandlingEnum;


  // The type of actor - system or human. (from http://hl7.org/fhir/ValueSet/examplescenario-actor-type)
  TFhirExampleScenarioActorTypeEnum = (
    ExampleScenarioActorTypeNull, // Value is missing from Instance
    ExampleScenarioActorTypePerson,
    ExampleScenarioActorTypeSystem);
  TFhirExampleScenarioActorTypeEnumList = set of TFhirExampleScenarioActorTypeEnum;


  // A code specifying the state of the resource instance. (from http://hl7.org/fhir/ValueSet/explanationofbenefit-status)
  TFhirExplanationOfBenefitStatusEnum = (
    ExplanationOfBenefitStatusNull, // Value is missing from Instance
    ExplanationOfBenefitStatusActive,
    ExplanationOfBenefitStatusCancelled,
    ExplanationOfBenefitStatusDraft,
    ExplanationOfBenefitStatusEnteredInError);
  TFhirExplanationOfBenefitStatusEnumList = set of TFhirExplanationOfBenefitStatusEnum;


  // How an extension context is interpreted. (from http://hl7.org/fhir/ValueSet/extension-context-type)
  TFhirExtensionContextTypeEnum = (
    ExtensionContextTypeNull, // Value is missing from Instance
    ExtensionContextTypeFhirpath,
    ExtensionContextTypeElement,
    ExtensionContextTypeExtension);
  TFhirExtensionContextTypeEnumList = set of TFhirExtensionContextTypeEnum;


  // The status of the Device record. (from http://hl7.org/fhir/ValueSet/device-status)
  TFhirFHIRDeviceStatusEnum = (
    FHIRDeviceStatusNull, // Value is missing from Instance
    FHIRDeviceStatusActive,
    FHIRDeviceStatusInactive,
    FHIRDeviceStatusEnteredInError);
  TFhirFHIRDeviceStatusEnumList = set of TFhirFHIRDeviceStatusEnum;


  // A code to indicate if the substance is actively used. (from http://hl7.org/fhir/ValueSet/substance-status)
  TFhirFHIRSubstanceStatusEnum = (
    FHIRSubstanceStatusNull, // Value is missing from Instance
    FHIRSubstanceStatusActive,
    FHIRSubstanceStatusInactive,
    FHIRSubstanceStatusEnteredInError);
  TFhirFHIRSubstanceStatusEnumList = set of TFhirFHIRSubstanceStatusEnum;


  // All FHIR types (from http://hl7.org/fhir/ValueSet/fhir-types)
  TFhirFHIRTypesEnum = (
    FHIRTypesNull, // Value is missing from Instance
    FHIRTypesBase,
    FHIRTypesElement,
    FHIRTypesBackboneElement,
    FHIRTypesDataType,
    FHIRTypesAddress,
    FHIRTypesAnnotation,
    FHIRTypesAttachment,
    FHIRTypesAvailability,
    FHIRTypesBackboneType,
    FHIRTypesDosage,
    FHIRTypesElementDefinition,
    FHIRTypesMarketingStatus,
    FHIRTypesPopulation,
    FHIRTypesProductShelfLife,
    FHIRTypesTiming,
    FHIRTypesCodeableConcept,
    FHIRTypesCodeableReference,
    FHIRTypesCoding,
    FHIRTypesContactDetail,
    FHIRTypesContactPoint,
    FHIRTypesContributor,
    FHIRTypesDataRequirement,
    FHIRTypesExpression,
    FHIRTypesExtendedContactDetail,
    FHIRTypesExtension,
    FHIRTypesHumanName,
    FHIRTypesIdentifier,
    FHIRTypesMeta,
    FHIRTypesMonetaryComponent,
    FHIRTypesMoney,
    FHIRTypesNarrative,
    FHIRTypesParameterDefinition,
    FHIRTypesPeriod,
    FHIRTypesPrimitiveType,
    FHIRTypesBase64Binary,
    FHIRTypesBoolean,
    FHIRTypesDate,
    FHIRTypesDateTime,
    FHIRTypesDecimal,
    FHIRTypesInstant,
    FHIRTypesInteger,
    FHIRTypesPositiveInt,
    FHIRTypesUnsignedInt,
    FHIRTypesInteger64,
    FHIRTypesString,
    FHIRTypesCode,
    FHIRTypesId,
    FHIRTypesMarkdown,
    FHIRTypesTime,
    FHIRTypesUri,
    FHIRTypesCanonical,
    FHIRTypesOid,
    FHIRTypesUrl,
    FHIRTypesUuid,
    FHIRTypesQuantity,
    FHIRTypesAge,
    FHIRTypesCount,
    FHIRTypesDistance,
    FHIRTypesDuration,
    FHIRTypesRange,
    FHIRTypesRatio,
    FHIRTypesRatioRange,
    FHIRTypesReference,
    FHIRTypesRelatedArtifact,
    FHIRTypesSampledData,
    FHIRTypesSignature,
    FHIRTypesTriggerDefinition,
    FHIRTypesUsageContext,
    FHIRTypesVirtualServiceDetail,
    FHIRTypesXhtml,
    FHIRTypesResource,
    FHIRTypesBinary,
    FHIRTypesBundle,
    FHIRTypesDomainResource,
    FHIRTypesAccount,
    FHIRTypesActivityDefinition,
    FHIRTypesActorDefinition,
    FHIRTypesAdministrableProductDefinition,
    FHIRTypesAdverseEvent,
    FHIRTypesAllergyIntolerance,
    FHIRTypesAppointment,
    FHIRTypesAppointmentResponse,
    FHIRTypesArtifactAssessment,
    FHIRTypesAuditEvent,
    FHIRTypesBasic,
    FHIRTypesBiologicallyDerivedProduct,
    FHIRTypesBodyStructure,
    FHIRTypesCanonicalResource,
    FHIRTypesCapabilityStatement,
    FHIRTypesCarePlan,
    FHIRTypesCareTeam,
    FHIRTypesChargeItem,
    FHIRTypesChargeItemDefinition,
    FHIRTypesCitation,
    FHIRTypesClaim,
    FHIRTypesClaimResponse,
    FHIRTypesClinicalImpression,
    FHIRTypesClinicalUseDefinition,
    FHIRTypesCodeSystem,
    FHIRTypesCommunication,
    FHIRTypesCommunicationRequest,
    FHIRTypesCompartmentDefinition,
    FHIRTypesComposition,
    FHIRTypesConceptMap,
    FHIRTypesCondition,
    FHIRTypesConditionDefinition,
    FHIRTypesConsent,
    FHIRTypesContract,
    FHIRTypesCoverage,
    FHIRTypesCoverageEligibilityRequest,
    FHIRTypesCoverageEligibilityResponse,
    FHIRTypesDetectedIssue,
    FHIRTypesDevice,
    FHIRTypesDeviceDefinition,
    FHIRTypesDeviceDispense,
    FHIRTypesDeviceMetric,
    FHIRTypesDeviceRequest,
    FHIRTypesDeviceUsage,
    FHIRTypesDiagnosticReport,
    FHIRTypesDocumentManifest,
    FHIRTypesDocumentReference,
    FHIRTypesEncounter,
    FHIRTypesEndpoint,
    FHIRTypesEnrollmentRequest,
    FHIRTypesEnrollmentResponse,
    FHIRTypesEpisodeOfCare,
    FHIRTypesEventDefinition,
    FHIRTypesEvidence,
    FHIRTypesEvidenceReport,
    FHIRTypesEvidenceVariable,
    FHIRTypesExampleScenario,
    FHIRTypesExplanationOfBenefit,
    FHIRTypesFamilyMemberHistory,
    FHIRTypesFlag,
    FHIRTypesFormularyItem,
    FHIRTypesGenomicStudy,
    FHIRTypesGoal,
    FHIRTypesGraphDefinition,
    FHIRTypesGroup,
    FHIRTypesGuidanceResponse,
    FHIRTypesHealthcareService,
    FHIRTypesImagingSelection,
    FHIRTypesImagingStudy,
    FHIRTypesImmunization,
    FHIRTypesImmunizationEvaluation,
    FHIRTypesImmunizationRecommendation,
    FHIRTypesImplementationGuide,
    FHIRTypesIngredient,
    FHIRTypesInsurancePlan,
    FHIRTypesInventoryReport,
    FHIRTypesInvoice,
    FHIRTypesLibrary,
    FHIRTypesLinkage,
    FHIRTypesList,
    FHIRTypesLocation,
    FHIRTypesManufacturedItemDefinition,
    FHIRTypesMeasure,
    FHIRTypesMeasureReport,
    FHIRTypesMedication,
    FHIRTypesMedicationAdministration,
    FHIRTypesMedicationDispense,
    FHIRTypesMedicationKnowledge,
    FHIRTypesMedicationRequest,
    FHIRTypesMedicationUsage,
    FHIRTypesMedicinalProductDefinition,
    FHIRTypesMessageDefinition,
    FHIRTypesMessageHeader,
    FHIRTypesMetadataResource,
    FHIRTypesMolecularSequence,
    FHIRTypesNamingSystem,
    FHIRTypesNutritionIntake,
    FHIRTypesNutritionOrder,
    FHIRTypesNutritionProduct,
    FHIRTypesObservation,
    FHIRTypesObservationDefinition,
    FHIRTypesOperationDefinition,
    FHIRTypesOperationOutcome,
    FHIRTypesOrganization,
    FHIRTypesOrganizationAffiliation,
    FHIRTypesPackagedProductDefinition,
    FHIRTypesPatient,
    FHIRTypesPaymentNotice,
    FHIRTypesPaymentReconciliation,
    FHIRTypesPermission,
    FHIRTypesPerson,
    FHIRTypesPlanDefinition,
    FHIRTypesPractitioner,
    FHIRTypesPractitionerRole,
    FHIRTypesProcedure,
    FHIRTypesProvenance,
    FHIRTypesQuestionnaire,
    FHIRTypesQuestionnaireResponse,
    FHIRTypesRegulatedAuthorization,
    FHIRTypesRelatedPerson,
    FHIRTypesRequestOrchestration,
    FHIRTypesRequirements,
    FHIRTypesResearchStudy,
    FHIRTypesResearchSubject,
    FHIRTypesRiskAssessment,
    FHIRTypesSchedule,
    FHIRTypesSearchParameter,
    FHIRTypesServiceRequest,
    FHIRTypesSlot,
    FHIRTypesSpecimen,
    FHIRTypesSpecimenDefinition,
    FHIRTypesStructureDefinition,
    FHIRTypesStructureMap,
    FHIRTypesSubscription,
    FHIRTypesSubscriptionStatus,
    FHIRTypesSubscriptionTopic,
    FHIRTypesSubstance,
    FHIRTypesSubstanceDefinition,
    FHIRTypesSubstanceNucleicAcid,
    FHIRTypesSubstancePolymer,
    FHIRTypesSubstanceProtein,
    FHIRTypesSubstanceReferenceInformation,
    FHIRTypesSubstanceSourceMaterial,
    FHIRTypesSupplyDelivery,
    FHIRTypesSupplyRequest,
    FHIRTypesTask,
    FHIRTypesTerminologyCapabilities,
    FHIRTypesTestReport,
    FHIRTypesTestScript,
    FHIRTypesTransport,
    FHIRTypesValueSet,
    FHIRTypesVerificationResult,
    FHIRTypesVisionPrescription,
    FHIRTypesParameters);
  TFhirFHIRTypesEnumList = set of TFhirFHIRTypesEnum;


  // All published FHIR Versions. (from http://hl7.org/fhir/ValueSet/FHIR-version)
  TFhirFHIRVersionEnum = (
    FHIRVersionNull, // Value is missing from Instance
    FHIRVersion001,
    FHIRVersion005,
    FHIRVersion006,
    FHIRVersion011,
    FHIRVersion00,
    FHIRVersion0080,
    FHIRVersion0081,
    FHIRVersion0082,
    FHIRVersion04,
    FHIRVersion040,
    FHIRVersion05,
    FHIRVersion050,
    FHIRVersion10,
    FHIRVersion100,
    FHIRVersion101,
    FHIRVersion102,
    FHIRVersion11,
    FHIRVersion110,
    FHIRVersion14,
    FHIRVersion140,
    FHIRVersion16,
    FHIRVersion160,
    FHIRVersion18,
    FHIRVersion180,
    FHIRVersion30,
    FHIRVersion300,
    FHIRVersion301,
    FHIRVersion302,
    FHIRVersion33,
    FHIRVersion330,
    FHIRVersion35,
    FHIRVersion350,
    FHIRVersion40,
    FHIRVersion400,
    FHIRVersion401,
    FHIRVersion41,
    FHIRVersion410,
    FHIRVersion42,
    FHIRVersion420,
    FHIRVersion43,
    FHIRVersion430,
    FHIRVersion44,
    FHIRVersion440,
    FHIRVersion45,
    FHIRVersion450,
    FHIRVersion46,
    FHIRVersion460,
    FHIRVersion50,
    FHIRVersion500,
    FHIRVersion500Cibuild,
    FHIRVersion500Snapshot1,
    FHIRVersion500Snapshot2,
    FHIRVersion500Ballot);
  TFhirFHIRVersionEnumList = set of TFhirFHIRVersionEnum;


  // A code that identifies the status of the family history record. (from http://hl7.org/fhir/ValueSet/history-status)
  TFhirFamilyHistoryStatusEnum = (
    FamilyHistoryStatusNull, // Value is missing from Instance
    FamilyHistoryStatusPartial,
    FamilyHistoryStatusCompleted,
    FamilyHistoryStatusEnteredInError,
    FamilyHistoryStatusHealthUnknown);
  TFhirFamilyHistoryStatusEnumList = set of TFhirFamilyHistoryStatusEnum;


  // The kind of operation to perform as a part of a property based filter. (from http://hl7.org/fhir/ValueSet/filter-operator)
  TFhirFilterOperatorEnum = (
    FilterOperatorNull, // Value is missing from Instance
    FilterOperatorEqual,
    FilterOperatorIsA,
    FilterOperatorDescendentOf,
    FilterOperatorIsNotA,
    FilterOperatorRegex,
    FilterOperatorIn,
    FilterOperatorNotIn,
    FilterOperatorGeneralizes,
    FilterOperatorChildOf,
    FilterOperatorDescendentLeaf,
    FilterOperatorExists);
  TFhirFilterOperatorEnumList = set of TFhirFilterOperatorEnum;


  // This value set includes Status codes. (from http://hl7.org/fhir/ValueSet/fm-status)
  TFhirFinancialResourceStatusCodesEnum = (
    FinancialResourceStatusCodesNull, // Value is missing from Instance
    FinancialResourceStatusCodesActive,
    FinancialResourceStatusCodesCancelled,
    FinancialResourceStatusCodesDraft,
    FinancialResourceStatusCodesEnteredInError);
  TFhirFinancialResourceStatusCodesEnumList = set of TFhirFinancialResourceStatusCodesEnum;


  // Indicates whether this flag is active and needs to be displayed to a user, or whether it is no longer needed or was entered in error. (from http://hl7.org/fhir/ValueSet/flag-status)
  TFhirFlagStatusEnum = (
    FlagStatusNull, // Value is missing from Instance
    FlagStatusActive,
    FlagStatusInactive,
    FlagStatusEnteredInError);
  TFhirFlagStatusEnumList = set of TFhirFlagStatusEnum;


  // FormularyItem Status Codes (from http://hl7.org/fhir/ValueSet/formularyitem-status)
  TFhirFormularyItemStatusCodesEnum = (
    FormularyItemStatusCodesNull, // Value is missing from Instance
    FormularyItemStatusCodesActive,
    FormularyItemStatusCodesEnteredInError,
    FormularyItemStatusCodesInactive);
  TFhirFormularyItemStatusCodesEnumList = set of TFhirFormularyItemStatusCodesEnum;


  // Codes that reflect the current state of a goal and whether the goal is still being targeted. (from http://hl7.org/fhir/ValueSet/goal-status)
  TFhirGoalLifecycleStatusEnum = (
    GoalLifecycleStatusNull, // Value is missing from Instance
    GoalLifecycleStatusProposed,
    GoalLifecycleStatusPlanned,
    GoalLifecycleStatusAccepted,
    GoalLifecycleStatusActive,
    GoalLifecycleStatusOnHold,
    GoalLifecycleStatusCompleted,
    GoalLifecycleStatusCancelled,
    GoalLifecycleStatusEnteredInError,
    GoalLifecycleStatusRejected);
  TFhirGoalLifecycleStatusEnumList = set of TFhirGoalLifecycleStatusEnum;


  // How a compartment must be linked. (from http://hl7.org/fhir/ValueSet/graph-compartment-rule)
  TFhirGraphCompartmentRuleEnum = (
    GraphCompartmentRuleNull, // Value is missing from Instance
    GraphCompartmentRuleIdentical,
    GraphCompartmentRuleMatching,
    GraphCompartmentRuleDifferent,
    GraphCompartmentRuleCustom);
  TFhirGraphCompartmentRuleEnumList = set of TFhirGraphCompartmentRuleEnum;


  // Defines how a compartment rule is used. (from http://hl7.org/fhir/ValueSet/graph-compartment-use)
  TFhirGraphCompartmentUseEnum = (
    GraphCompartmentUseNull, // Value is missing from Instance
    GraphCompartmentUseCondition,
    GraphCompartmentUseRequirement);
  TFhirGraphCompartmentUseEnumList = set of TFhirGraphCompartmentUseEnum;


  // Basis for membership in a group (from http://hl7.org/fhir/ValueSet/group-membership-basis)
  TFhirGroupMembershipBasisEnum = (
    GroupMembershipBasisNull, // Value is missing from Instance
    GroupMembershipBasisDefinitional,
    GroupMembershipBasisEnumerated);
  TFhirGroupMembershipBasisEnumList = set of TFhirGroupMembershipBasisEnum;


  // Types of resources that are part of group. (from http://hl7.org/fhir/ValueSet/group-type)
  TFhirGroupTypeEnum = (
    GroupTypeNull, // Value is missing from Instance
    GroupTypePerson,
    GroupTypeAnimal,
    GroupTypePractitioner,
    GroupTypeDevice,
    GroupTypeCareteam,
    GroupTypeHealthcareservice,
    GroupTypeLocation,
    GroupTypeOrganization,
    GroupTypeRelatedperson,
    GroupTypeSpecimen);
  TFhirGroupTypeEnumList = set of TFhirGroupTypeEnum;


  // The status of a guidance response. (from http://hl7.org/fhir/ValueSet/guidance-response-status)
  TFhirGuidanceResponseStatusEnum = (
    GuidanceResponseStatusNull, // Value is missing from Instance
    GuidanceResponseStatusSuccess,
    GuidanceResponseStatusDataRequested,
    GuidanceResponseStatusDataRequired,
    GuidanceResponseStatusInProgress,
    GuidanceResponseStatusFailure,
    GuidanceResponseStatusEnteredInError);
  TFhirGuidanceResponseStatusEnumList = set of TFhirGuidanceResponseStatusEnum;


  // A code that indicates how the page is generated. (from http://hl7.org/fhir/ValueSet/guide-page-generation)
  TFhirGuidePageGenerationEnum = (
    GuidePageGenerationNull, // Value is missing from Instance
    GuidePageGenerationHtml,
    GuidePageGenerationMarkdown,
    GuidePageGenerationXml,
    GuidePageGenerationGenerated);
  TFhirGuidePageGenerationEnumList = set of TFhirGuidePageGenerationEnum;


  // HTTP verbs (in the HTTP command line). See [HTTP rfc](https://tools.ietf.org/html/rfc7231) for details. (from http://hl7.org/fhir/ValueSet/http-verb)
  TFhirHTTPVerbEnum = (
    HTTPVerbNull, // Value is missing from Instance
    HTTPVerbGET,
    HTTPVerbHEAD,
    HTTPVerbPOST,
    HTTPVerbPUT,
    HTTPVerbDELETE,
    HTTPVerbPATCH);
  TFhirHTTPVerbEnumList = set of TFhirHTTPVerbEnum;


  // Identifies the purpose for this identifier, if known . (from http://hl7.org/fhir/ValueSet/identifier-use)
  TFhirIdentifierUseEnum = (
    IdentifierUseNull, // Value is missing from Instance
    IdentifierUseUsual,
    IdentifierUseOfficial,
    IdentifierUseTemp,
    IdentifierUseSecondary,
    IdentifierUseOld);
  TFhirIdentifierUseEnumList = set of TFhirIdentifierUseEnum;


  // The level of confidence that this link represents the same actual person, based on NIST Authentication Levels. (from http://hl7.org/fhir/ValueSet/identity-assuranceLevel)
  TFhirIdentityAssuranceLevelEnum = (
    IdentityAssuranceLevelNull, // Value is missing from Instance
    IdentityAssuranceLevelLevel1,
    IdentityAssuranceLevelLevel2,
    IdentityAssuranceLevelLevel3,
    IdentityAssuranceLevelLevel4);
  TFhirIdentityAssuranceLevelEnumList = set of TFhirIdentityAssuranceLevelEnum;


  // The type of 2D coordinates describing a 2D image region. (from http://hl7.org/fhir/ValueSet/imagingselection-2dgraphictype)
  TFhirImagingSelection2DGraphicTypeEnum = (
    ImagingSelection2DGraphicTypeNull, // Value is missing from Instance
    ImagingSelection2DGraphicTypePoint,
    ImagingSelection2DGraphicTypePolyline,
    ImagingSelection2DGraphicTypeInterpolated,
    ImagingSelection2DGraphicTypeCircle,
    ImagingSelection2DGraphicTypeEllipse);
  TFhirImagingSelection2DGraphicTypeEnumList = set of TFhirImagingSelection2DGraphicTypeEnum;


  // The type of coordinates describing a 3D image region. (from http://hl7.org/fhir/ValueSet/imagingselection-3dgraphictype)
  TFhirImagingSelection3DGraphicTypeEnum = (
    ImagingSelection3DGraphicTypeNull, // Value is missing from Instance
    ImagingSelection3DGraphicTypePoint,
    ImagingSelection3DGraphicTypeMultipoint,
    ImagingSelection3DGraphicTypePolyline,
    ImagingSelection3DGraphicTypePolygon,
    ImagingSelection3DGraphicTypeEllipse,
    ImagingSelection3DGraphicTypeEllipsoid);
  TFhirImagingSelection3DGraphicTypeEnumList = set of TFhirImagingSelection3DGraphicTypeEnum;


  // The status of the ImagingSelection. (from http://hl7.org/fhir/ValueSet/imagingselection-status)
  TFhirImagingSelectionStatusEnum = (
    ImagingSelectionStatusNull, // Value is missing from Instance
    ImagingSelectionStatusAvailable,
    ImagingSelectionStatusEnteredInError,
    ImagingSelectionStatusUnknown);
  TFhirImagingSelectionStatusEnumList = set of TFhirImagingSelectionStatusEnum;


  // The status of the ImagingStudy. (from http://hl7.org/fhir/ValueSet/imagingstudy-status)
  TFhirImagingStudyStatusEnum = (
    ImagingStudyStatusNull, // Value is missing from Instance
    ImagingStudyStatusRegistered,
    ImagingStudyStatusAvailable,
    ImagingStudyStatusCancelled,
    ImagingStudyStatusEnteredInError,
    ImagingStudyStatusUnknown);
  TFhirImagingStudyStatusEnumList = set of TFhirImagingStudyStatusEnum;


  // The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the evaluation for vaccine administration event. (from http://hl7.org/fhir/ValueSet/immunization-evaluation-status)
  TFhirImmunizationEvaluationStatusCodesEnum = (
    ImmunizationEvaluationStatusCodesNull, // Value is missing from Instance
    ImmunizationEvaluationStatusCodesCompleted,
    ImmunizationEvaluationStatusCodesEnteredInError);
  TFhirImmunizationEvaluationStatusCodesEnumList = set of TFhirImmunizationEvaluationStatusCodesEnum;


  // The value set to instantiate this attribute should be drawn from a terminologically robust code system that consists of or contains concepts to support describing the current status of the administered dose of vaccine. (from http://hl7.org/fhir/ValueSet/immunization-status)
  TFhirImmunizationStatusCodesEnum = (
    ImmunizationStatusCodesNull, // Value is missing from Instance
    ImmunizationStatusCodesCompleted,
    ImmunizationStatusCodesEnteredInError,
    ImmunizationStatusCodesNotDone);
  TFhirImmunizationStatusCodesEnumList = set of TFhirImmunizationStatusCodesEnum;


  // The way in which this manufacturer is associated with the ingredient. For example whether it is a possible one (others allowed), or an exclusive authorized one for this ingredient. Note that this is not the manufacturing process role. (from http://hl7.org/fhir/ValueSet/ingredient-manufacturer-role)
  TFhirIngredientManufacturerRoleEnum = (
    IngredientManufacturerRoleNull, // Value is missing from Instance
    IngredientManufacturerRoleAllowed,
    IngredientManufacturerRolePossible,
    IngredientManufacturerRoleActual);
  TFhirIngredientManufacturerRoleEnumList = set of TFhirIngredientManufacturerRoleEnum;


  // FHIR RESTful interaction codes used for SubscriptionTopic trigger. (from http://hl7.org/fhir/ValueSet/interaction-trigger)
  TFhirInteractionTriggerEnum = (
    InteractionTriggerNull, // Value is missing from Instance
    InteractionTriggerCreate,
    InteractionTriggerUpdate,
    InteractionTriggerDelete);
  TFhirInteractionTriggerEnumList = set of TFhirInteractionTriggerEnum;


  // The type of count. (from http://hl7.org/fhir/ValueSet/inventoryreport-counttype)
  TFhirInventoryCountTypeEnum = (
    InventoryCountTypeNull, // Value is missing from Instance
    InventoryCountTypeSnapshot,
    InventoryCountTypeDifference);
  TFhirInventoryCountTypeEnumList = set of TFhirInventoryCountTypeEnum;


  // The status of the InventoryReport. (from http://hl7.org/fhir/ValueSet/inventoryreport-status)
  TFhirInventoryReportStatusEnum = (
    InventoryReportStatusNull, // Value is missing from Instance
    InventoryReportStatusDraft,
    InventoryReportStatusRequested,
    InventoryReportStatusActive,
    InventoryReportStatusEnteredInError);
  TFhirInventoryReportStatusEnumList = set of TFhirInventoryReportStatusEnum;


  // Codes identifying the lifecycle stage of an Invoice. (from http://hl7.org/fhir/ValueSet/invoice-status)
  TFhirInvoiceStatusEnum = (
    InvoiceStatusNull, // Value is missing from Instance
    InvoiceStatusDraft,
    InvoiceStatusIssued,
    InvoiceStatusBalanced,
    InvoiceStatusCancelled,
    InvoiceStatusEnteredInError);
  TFhirInvoiceStatusEnumList = set of TFhirInvoiceStatusEnum;


  // How the issue affects the success of the action. (from http://hl7.org/fhir/ValueSet/issue-severity)
  TFhirIssueSeverityEnum = (
    IssueSeverityNull, // Value is missing from Instance
    IssueSeverityFatal,
    IssueSeverityError,
    IssueSeverityWarning,
    IssueSeverityInformation);
  TFhirIssueSeverityEnumList = set of TFhirIssueSeverityEnum;


  // A code that describes the type of issue. (from http://hl7.org/fhir/ValueSet/issue-type)
  TFhirIssueTypeEnum = (
    IssueTypeNull, // Value is missing from Instance
    IssueTypeInvalid,
    IssueTypeStructure,
    IssueTypeRequired,
    IssueTypeValue,
    IssueTypeInvariant,
    IssueTypeSecurity,
    IssueTypeLogin,
    IssueTypeUnknown,
    IssueTypeExpired,
    IssueTypeForbidden,
    IssueTypeSuppressed,
    IssueTypeProcessing,
    IssueTypeNotSupported,
    IssueTypeDuplicate,
    IssueTypeMultipleMatches,
    IssueTypeNotFound,
    IssueTypeDeleted,
    IssueTypeTooLong,
    IssueTypeCodeInvalid,
    IssueTypeExtension,
    IssueTypeTooCostly,
    IssueTypeBusinessRule,
    IssueTypeConflict,
    IssueTypeTransient,
    IssueTypeLockError,
    IssueTypeNoStore,
    IssueTypeException,
    IssueTypeTimeout,
    IssueTypeIncomplete,
    IssueTypeThrottled,
    IssueTypeInformational);
  TFhirIssueTypeEnumList = set of TFhirIssueTypeEnum;


  // The kind of coverage: insurance, selfpay or other. (from http://hl7.org/fhir/ValueSet/coverage-kind)
  TFhirKindEnum = (
    KindNull, // Value is missing from Instance
    KindInsurance,
    KindSelfPay,
    KindOther);
  TFhirKindEnumList = set of TFhirKindEnum;


  // Link Relation Types defined at https://www.iana.org/assignments/link-relations/link-relations.xhtml#link-relations-1 (from http://hl7.org/fhir/ValueSet/iana-link-relations)
  TFhirLinkRelationTypesEnum = (
    LinkRelationTypesNull, // Value is missing from Instance
    LinkRelationTypesAbout,
    LinkRelationTypesAcl,
    LinkRelationTypesAlternate,
    LinkRelationTypesAmphtml,
    LinkRelationTypesAppendix,
    LinkRelationTypesAppleTouchIcon,
    LinkRelationTypesAppleTouchStartupImage,
    LinkRelationTypesArchives,
    LinkRelationTypesAuthor,
    LinkRelationTypesBlockedBy,
    LinkRelationTypesBookmark,
    LinkRelationTypesCanonical,
    LinkRelationTypesChapter,
    LinkRelationTypesCiteAs,
    LinkRelationTypesCollection,
    LinkRelationTypesContents,
    LinkRelationTypesConvertedFrom,
    LinkRelationTypesCopyright,
    LinkRelationTypesCreateForm,
    LinkRelationTypesCurrent,
    LinkRelationTypesDescribedby,
    LinkRelationTypesDescribes,
    LinkRelationTypesDisclosure,
    LinkRelationTypesDnsPrefetch,
    LinkRelationTypesDuplicate,
    LinkRelationTypesEdit,
    LinkRelationTypesEditForm,
    LinkRelationTypesEditMedia,
    LinkRelationTypesEnclosure,
    LinkRelationTypesExternal,
    LinkRelationTypesFirst,
    LinkRelationTypesGlossary,
    LinkRelationTypesHelp,
    LinkRelationTypesHosts,
    LinkRelationTypesHub,
    LinkRelationTypesIcon,
    LinkRelationTypesIndex,
    LinkRelationTypesIntervalAfter,
    LinkRelationTypesIntervalBefore,
    LinkRelationTypesIntervalContains,
    LinkRelationTypesIntervalDisjoint,
    LinkRelationTypesIntervalDuring,
    LinkRelationTypesIntervalEquals,
    LinkRelationTypesIntervalFinishedBy,
    LinkRelationTypesIntervalFinishes,
    LinkRelationTypesIntervalIn,
    LinkRelationTypesIntervalMeets,
    LinkRelationTypesIntervalMetBy,
    LinkRelationTypesIntervalOverlappedBy,
    LinkRelationTypesIntervalOverlaps,
    LinkRelationTypesIntervalStartedBy,
    LinkRelationTypesIntervalStarts,
    LinkRelationTypesItem,
    LinkRelationTypesLast,
    LinkRelationTypesLatestVersion,
    LinkRelationTypesLicense,
    LinkRelationTypesLinkset,
    LinkRelationTypesLrdd,
    LinkRelationTypesManifest,
    LinkRelationTypesMaskIcon,
    LinkRelationTypesMediaFeed,
    LinkRelationTypesMemento,
    LinkRelationTypesMicropub,
    LinkRelationTypesModulepreload,
    LinkRelationTypesMonitor,
    LinkRelationTypesMonitorGroup,
    LinkRelationTypesNext,
    LinkRelationTypesNextArchive,
    LinkRelationTypesNofollow,
    LinkRelationTypesNoopener,
    LinkRelationTypesNoreferrer,
    LinkRelationTypesOpener,
    LinkRelationTypesOpenid2LocalId,
    LinkRelationTypesOpenid2Provider,
    LinkRelationTypesOriginal,
    LinkRelationTypesP3Pv1,
    LinkRelationTypesPayment,
    LinkRelationTypesPingback,
    LinkRelationTypesPreconnect,
    LinkRelationTypesPredecessorVersion,
    LinkRelationTypesPrefetch,
    LinkRelationTypesPreload,
    LinkRelationTypesPrerender,
    LinkRelationTypesPrev,
    LinkRelationTypesPreview,
    LinkRelationTypesPrevious,
    LinkRelationTypesPrevArchive,
    LinkRelationTypesPrivacyPolicy,
    LinkRelationTypesProfile,
    LinkRelationTypesPublication,
    LinkRelationTypesRelated,
    LinkRelationTypesRestconf,
    LinkRelationTypesReplies,
    LinkRelationTypesRuleinput,
    LinkRelationTypesSearch,
    LinkRelationTypesSection,
    LinkRelationTypesSelf,
    LinkRelationTypesService,
    LinkRelationTypesServiceDesc,
    LinkRelationTypesServiceDoc,
    LinkRelationTypesServiceMeta,
    LinkRelationTypesSponsored,
    LinkRelationTypesStart,
    LinkRelationTypesStatus,
    LinkRelationTypesStylesheet,
    LinkRelationTypesSubsection,
    LinkRelationTypesSuccessorVersion,
    LinkRelationTypesSunset,
    LinkRelationTypesTag,
    LinkRelationTypesTermsOfService,
    LinkRelationTypesTimegate,
    LinkRelationTypesTimemap,
    LinkRelationTypesType,
    LinkRelationTypesUgc,
    LinkRelationTypesUp,
    LinkRelationTypesVersionHistory,
    LinkRelationTypesVia,
    LinkRelationTypesWebmention,
    LinkRelationTypesWorkingCopy,
    LinkRelationTypesWorkingCopyOf);
  TFhirLinkRelationTypesEnumList = set of TFhirLinkRelationTypesEnum;


  // The type of link between this patient resource and another patient resource. (from http://hl7.org/fhir/ValueSet/link-type)
  TFhirLinkTypeEnum = (
    LinkTypeNull, // Value is missing from Instance
    LinkTypeReplacedBy,
    LinkTypeReplaces,
    LinkTypeRefer,
    LinkTypeSeealso);
  TFhirLinkTypeEnumList = set of TFhirLinkTypeEnum;


  // Used to distinguish different roles a resource can play within a set of linked resources. (from http://hl7.org/fhir/ValueSet/linkage-type)
  TFhirLinkageTypeEnum = (
    LinkageTypeNull, // Value is missing from Instance
    LinkageTypeSource,
    LinkageTypeAlternate,
    LinkageTypeHistorical);
  TFhirLinkageTypeEnumList = set of TFhirLinkageTypeEnum;


  // The processing mode that applies to this list. (from http://hl7.org/fhir/ValueSet/list-mode)
  TFhirListModeEnum = (
    ListModeNull, // Value is missing from Instance
    ListModeWorking,
    ListModeSnapshot,
    ListModeChanges);
  TFhirListModeEnumList = set of TFhirListModeEnum;


  // The current state of the list. (from http://hl7.org/fhir/ValueSet/list-status)
  TFhirListStatusEnum = (
    ListStatusNull, // Value is missing from Instance
    ListStatusCurrent,
    ListStatusRetired,
    ListStatusEnteredInError);
  TFhirListStatusEnumList = set of TFhirListStatusEnum;


  // Indicates whether a resource instance represents a specific location or a class of locations. (from http://hl7.org/fhir/ValueSet/location-mode)
  TFhirLocationModeEnum = (
    LocationModeNull, // Value is missing from Instance
    LocationModeInstance,
    LocationModeKind);
  TFhirLocationModeEnumList = set of TFhirLocationModeEnum;


  // Indicates whether the location is still in use. (from http://hl7.org/fhir/ValueSet/location-status)
  TFhirLocationStatusEnum = (
    LocationStatusNull, // Value is missing from Instance
    LocationStatusActive,
    LocationStatusSuspended,
    LocationStatusInactive);
  TFhirLocationStatusEnumList = set of TFhirLocationStatusEnum;


  // The status of the measure report. (from http://hl7.org/fhir/ValueSet/measure-report-status)
  TFhirMeasureReportStatusEnum = (
    MeasureReportStatusNull, // Value is missing from Instance
    MeasureReportStatusComplete,
    MeasureReportStatusPending,
    MeasureReportStatusError);
  TFhirMeasureReportStatusEnumList = set of TFhirMeasureReportStatusEnum;


  // The type of the measure report. (from http://hl7.org/fhir/ValueSet/measure-report-type)
  TFhirMeasureReportTypeEnum = (
    MeasureReportTypeNull, // Value is missing from Instance
    MeasureReportTypeIndividual,
    MeasureReportTypeSubjectList,
    MeasureReportTypeSummary,
    MeasureReportTypeDataExchange);
  TFhirMeasureReportTypeEnumList = set of TFhirMeasureReportTypeEnum;


  // MedicationAdministration Status Codes (from http://hl7.org/fhir/ValueSet/medication-admin-status)
  TFhirMedicationAdministrationStatusCodesEnum = (
    MedicationAdministrationStatusCodesNull, // Value is missing from Instance
    MedicationAdministrationStatusCodesInProgress,
    MedicationAdministrationStatusCodesNotDone,
    MedicationAdministrationStatusCodesOnHold,
    MedicationAdministrationStatusCodesCompleted,
    MedicationAdministrationStatusCodesEnteredInError,
    MedicationAdministrationStatusCodesStopped,
    MedicationAdministrationStatusCodesUnknown);
  TFhirMedicationAdministrationStatusCodesEnumList = set of TFhirMedicationAdministrationStatusCodesEnum;


  // MedicationDispense Status Codes (from http://hl7.org/fhir/ValueSet/medicationdispense-status)
  TFhirMedicationDispenseStatusCodesEnum = (
    MedicationDispenseStatusCodesNull, // Value is missing from Instance
    MedicationDispenseStatusCodesPreparation,
    MedicationDispenseStatusCodesInProgress,
    MedicationDispenseStatusCodesCancelled,
    MedicationDispenseStatusCodesOnHold,
    MedicationDispenseStatusCodesCompleted,
    MedicationDispenseStatusCodesEnteredInError,
    MedicationDispenseStatusCodesStopped,
    MedicationDispenseStatusCodesDeclined,
    MedicationDispenseStatusCodesUnknown);
  TFhirMedicationDispenseStatusCodesEnumList = set of TFhirMedicationDispenseStatusCodesEnum;


  // MedicationKnowledge Status Codes (from http://hl7.org/fhir/ValueSet/medicationknowledge-status)
  TFhirMedicationKnowledgeStatusCodesEnum = (
    MedicationKnowledgeStatusCodesNull, // Value is missing from Instance
    MedicationKnowledgeStatusCodesActive,
    MedicationKnowledgeStatusCodesEnteredInError,
    MedicationKnowledgeStatusCodesInactive);
  TFhirMedicationKnowledgeStatusCodesEnumList = set of TFhirMedicationKnowledgeStatusCodesEnum;


  // MedicationRequest Intent Codes (from http://hl7.org/fhir/ValueSet/medicationrequest-intent)
  TFhirMedicationRequestIntentEnum = (
    MedicationRequestIntentNull, // Value is missing from Instance
    MedicationRequestIntentProposal,
    MedicationRequestIntentPlan,
    MedicationRequestIntentOrder,
    MedicationRequestIntentOriginalOrder,
    MedicationRequestIntentReflexOrder,
    MedicationRequestIntentFillerOrder,
    MedicationRequestIntentInstanceOrder,
    MedicationRequestIntentOption);
  TFhirMedicationRequestIntentEnumList = set of TFhirMedicationRequestIntentEnum;


  // Medication Status Codes (from http://hl7.org/fhir/ValueSet/medication-status)
  TFhirMedicationStatusCodesEnum = (
    MedicationStatusCodesNull, // Value is missing from Instance
    MedicationStatusCodesActive,
    MedicationStatusCodesInactive,
    MedicationStatusCodesEnteredInError);
  TFhirMedicationStatusCodesEnumList = set of TFhirMedicationStatusCodesEnum;


  // MedicationUsage Status Codes (from http://hl7.org/fhir/ValueSet/medication-usage-status)
  TFhirMedicationUsageStatusCodesEnum = (
    MedicationUsageStatusCodesNull, // Value is missing from Instance
    MedicationUsageStatusCodesRecorded,
    MedicationUsageStatusCodesEnteredInError,
    MedicationUsageStatusCodesDraft);
  TFhirMedicationUsageStatusCodesEnumList = set of TFhirMedicationUsageStatusCodesEnum;


  // MedicationRequest Status Codes (from http://hl7.org/fhir/ValueSet/medicationrequest-status)
  TFhirMedicationrequestStatusEnum = (
    MedicationrequestStatusNull, // Value is missing from Instance
    MedicationrequestStatusActive,
    MedicationrequestStatusOnHold,
    MedicationrequestStatusEnded,
    MedicationrequestStatusStopped,
    MedicationrequestStatusCompleted,
    MedicationrequestStatusCancelled,
    MedicationrequestStatusEnteredInError,
    MedicationrequestStatusDraft,
    MedicationrequestStatusUnknown);
  TFhirMedicationrequestStatusEnumList = set of TFhirMedicationrequestStatusEnum;


  // The impact of the content of a message. (from http://hl7.org/fhir/ValueSet/message-significance-category)
  TFhirMessageSignificanceCategoryEnum = (
    MessageSignificanceCategoryNull, // Value is missing from Instance
    MessageSignificanceCategoryConsequence,
    MessageSignificanceCategoryCurrency,
    MessageSignificanceCategoryNotification);
  TFhirMessageSignificanceCategoryEnumList = set of TFhirMessageSignificanceCategoryEnum;


  // HL7-defined table of codes which identify conditions under which acknowledgments are required to be returned in response to a message. (from http://hl7.org/fhir/ValueSet/messageheader-response-request)
  TFhirMessageheaderResponseRequestEnum = (
    MessageheaderResponseRequestNull, // Value is missing from Instance
    MessageheaderResponseRequestAlways,
    MessageheaderResponseRequestOnError,
    MessageheaderResponseRequestNever,
    MessageheaderResponseRequestOnSuccess);
  TFhirMessageheaderResponseRequestEnumList = set of TFhirMessageheaderResponseRequestEnum;


  // The use of a human name. (from http://hl7.org/fhir/ValueSet/name-use)
  TFhirNameUseEnum = (
    NameUseNull, // Value is missing from Instance
    NameUseUsual,
    NameUseOfficial,
    NameUseTemp,
    NameUseNickname,
    NameUseAnonymous,
    NameUseOld,
    NameUseMaiden);
  TFhirNameUseEnumList = set of TFhirNameUseEnum;


  // Identifies the style of unique identifier used to identify a namespace. (from http://hl7.org/fhir/ValueSet/namingsystem-identifier-type)
  TFhirNamingSystemIdentifierTypeEnum = (
    NamingSystemIdentifierTypeNull, // Value is missing from Instance
    NamingSystemIdentifierTypeOid,
    NamingSystemIdentifierTypeUuid,
    NamingSystemIdentifierTypeUri,
    NamingSystemIdentifierTypeV2csmnemonic,
    NamingSystemIdentifierTypeOther);
  TFhirNamingSystemIdentifierTypeEnumList = set of TFhirNamingSystemIdentifierTypeEnum;


  // Identifies the purpose of the naming system. (from http://hl7.org/fhir/ValueSet/namingsystem-type)
  TFhirNamingSystemTypeEnum = (
    NamingSystemTypeNull, // Value is missing from Instance
    NamingSystemTypeCodesystem,
    NamingSystemTypeIdentifier,
    NamingSystemTypeRoot);
  TFhirNamingSystemTypeEnumList = set of TFhirNamingSystemTypeEnum;


  // The status of a resource narrative. (from http://hl7.org/fhir/ValueSet/narrative-status)
  TFhirNarrativeStatusEnum = (
    NarrativeStatusNull, // Value is missing from Instance
    NarrativeStatusGenerated,
    NarrativeStatusExtensions,
    NarrativeStatusAdditional,
    NarrativeStatusEmpty);
  TFhirNarrativeStatusEnumList = set of TFhirNarrativeStatusEnum;


  // The presentation types of notes. (from http://hl7.org/fhir/ValueSet/note-type)
  TFhirNoteTypeEnum = (
    NoteTypeNull, // Value is missing from Instance
    NoteTypeDisplay,
    NoteTypePrint,
    NoteTypePrintoper);
  TFhirNoteTypeEnumList = set of TFhirNoteTypeEnum;


  // Codes identifying the lifecycle stage of a product. (from http://hl7.org/fhir/ValueSet/nutritionproduct-status)
  TFhirNutritionProductStatusEnum = (
    NutritionProductStatusNull, // Value is missing from Instance
    NutritionProductStatusActive,
    NutritionProductStatusInactive,
    NutritionProductStatusEnteredInError);
  TFhirNutritionProductStatusEnumList = set of TFhirNutritionProductStatusEnum;


  // Permitted data type for observation value. (from http://hl7.org/fhir/ValueSet/permitted-data-type)
  TFhirObservationDataTypeEnum = (
    ObservationDataTypeNull, // Value is missing from Instance
    ObservationDataTypeQuantity,
    ObservationDataTypeCodeableConcept,
    ObservationDataTypeString,
    ObservationDataTypeBoolean,
    ObservationDataTypeInteger,
    ObservationDataTypeRange,
    ObservationDataTypeRatio,
    ObservationDataTypeSampledData,
    ObservationDataTypeTime,
    ObservationDataTypeDateTime,
    ObservationDataTypePeriod);
  TFhirObservationDataTypeEnumList = set of TFhirObservationDataTypeEnum;


  // Codes identifying the category of observation range. (from http://hl7.org/fhir/ValueSet/observation-range-category)
  TFhirObservationRangeCategoryEnum = (
    ObservationRangeCategoryNull, // Value is missing from Instance
    ObservationRangeCategoryReference,
    ObservationRangeCategoryCritical,
    ObservationRangeCategoryAbsolute);
  TFhirObservationRangeCategoryEnumList = set of TFhirObservationRangeCategoryEnum;


  // Codes providing the status of an observation. (from http://hl7.org/fhir/ValueSet/observation-status)
  TFhirObservationStatusEnum = (
    ObservationStatusNull, // Value is missing from Instance
    ObservationStatusRegistered,
    ObservationStatusPreliminary,
    ObservationStatusFinal,
    ObservationStatusAmended,
    ObservationStatusCorrected,
    ObservationStatusCancelled,
    ObservationStatusEnteredInError,
    ObservationStatusUnknown);
  TFhirObservationStatusEnumList = set of TFhirObservationStatusEnum;


  // Whether an operation is a normal operation or a query. (from http://hl7.org/fhir/ValueSet/operation-kind)
  TFhirOperationKindEnum = (
    OperationKindNull, // Value is missing from Instance
    OperationKindOperation,
    OperationKindQuery);
  TFhirOperationKindEnumList = set of TFhirOperationKindEnum;


  // Indicates that a parameter applies when the operation is being invoked at the specified level (from http://hl7.org/fhir/ValueSet/operation-parameter-scope)
  TFhirOperationParameterScopeEnum = (
    OperationParameterScopeNull, // Value is missing from Instance
    OperationParameterScopeInstance,
    OperationParameterScopeType,
    OperationParameterScopeSystem);
  TFhirOperationParameterScopeEnumList = set of TFhirOperationParameterScopeEnum;


  // Whether an operation parameter is an input or an output parameter. (from http://hl7.org/fhir/ValueSet/operation-parameter-use)
  TFhirOperationParameterUseEnum = (
    OperationParameterUseNull, // Value is missing from Instance
    OperationParameterUseIn,
    OperationParameterUseOut);
  TFhirOperationParameterUseEnumList = set of TFhirOperationParameterUseEnum;


  // Type for orientation. (from http://hl7.org/fhir/ValueSet/orientation-type)
  TFhirOrientationTypeEnum = (
    OrientationTypeNull, // Value is missing from Instance
    OrientationTypeSense,
    OrientationTypeAntisense);
  TFhirOrientationTypeEnumList = set of TFhirOrientationTypeEnum;


  // The Participation status of an appointment. (from http://hl7.org/fhir/ValueSet/participationstatus)
  TFhirParticipationStatusEnum = (
    ParticipationStatusNull, // Value is missing from Instance
    ParticipationStatusAccepted,
    ParticipationStatusDeclined,
    ParticipationStatusTentative,
    ParticipationStatusNeedsAction);
  TFhirParticipationStatusEnumList = set of TFhirParticipationStatusEnum;


  // The outcome of the processing. (from http://hl7.org/fhir/ValueSet/payment-outcome)
  TFhirPaymentOutcomeEnum = (
    PaymentOutcomeNull, // Value is missing from Instance
    PaymentOutcomeQueued,
    PaymentOutcomeComplete,
    PaymentOutcomeError,
    PaymentOutcomePartial);
  TFhirPaymentOutcomeEnumList = set of TFhirPaymentOutcomeEnum;


  // Codes identifying rule combining algorithm. (from http://hl7.org/fhir/ValueSet/permission-rule-combining)
  TFhirPermissionRuleCombiningEnum = (
    PermissionRuleCombiningNull, // Value is missing from Instance
    PermissionRuleCombiningDenyOverrides,
    PermissionRuleCombiningPermitOverrides,
    PermissionRuleCombiningOrderedDenyOverrides,
    PermissionRuleCombiningOrderedPermitOverrides,
    PermissionRuleCombiningDenyUnlessPermit,
    PermissionRuleCombiningPermitUnlessDeny);
  TFhirPermissionRuleCombiningEnumList = set of TFhirPermissionRuleCombiningEnum;


  // Codes identifying the lifecycle stage of a product. (from http://hl7.org/fhir/ValueSet/permission-status)
  TFhirPermissionStatusEnum = (
    PermissionStatusNull, // Value is missing from Instance
    PermissionStatusActive,
    PermissionStatusEnteredInError,
    PermissionStatusDraft,
    PermissionStatusRejected);
  TFhirPermissionStatusEnumList = set of TFhirPermissionStatusEnum;


  // Codes indicating the kind of the price component. (from http://hl7.org/fhir/ValueSet/price-component-type)
  TFhirPriceComponentTypeEnum = (
    PriceComponentTypeNull, // Value is missing from Instance
    PriceComponentTypeBase,
    PriceComponentTypeSurcharge,
    PriceComponentTypeDeduction,
    PriceComponentTypeDiscount,
    PriceComponentTypeTax,
    PriceComponentTypeInformational);
  TFhirPriceComponentTypeEnumList = set of TFhirPriceComponentTypeEnum;


  // How a property is represented when serialized. (from http://hl7.org/fhir/ValueSet/property-representation)
  TFhirPropertyRepresentationEnum = (
    PropertyRepresentationNull, // Value is missing from Instance
    PropertyRepresentationXmlAttr,
    PropertyRepresentationXmlText,
    PropertyRepresentationTypeAttr,
    PropertyRepresentationCdaText,
    PropertyRepresentationXhtml);
  TFhirPropertyRepresentationEnumList = set of TFhirPropertyRepresentationEnum;


  // How an entity was used in an activity. (from http://hl7.org/fhir/ValueSet/provenance-entity-role)
  TFhirProvenanceEntityRoleEnum = (
    ProvenanceEntityRoleNull, // Value is missing from Instance
    ProvenanceEntityRoleRevision,
    ProvenanceEntityRoleQuotation,
    ProvenanceEntityRoleSource,
    ProvenanceEntityRoleInstantiates,
    ProvenanceEntityRoleRemoval);
  TFhirProvenanceEntityRoleEnumList = set of TFhirProvenanceEntityRoleEnum;


  // The lifecycle status of an artifact. (from http://hl7.org/fhir/ValueSet/publication-status)
  TFhirPublicationStatusEnum = (
    PublicationStatusNull, // Value is missing from Instance
    PublicationStatusDraft,
    PublicationStatusActive,
    PublicationStatusRetired,
    PublicationStatusUnknown);
  TFhirPublicationStatusEnumList = set of TFhirPublicationStatusEnum;


  // How the Quantity should be understood and represented. (from http://hl7.org/fhir/ValueSet/quantity-comparator)
  TFhirQuantityComparatorEnum = (
    QuantityComparatorNull, // Value is missing from Instance
    QuantityComparatorLessThan,
    QuantityComparatorLessOrEquals,
    QuantityComparatorGreaterOrEquals,
    QuantityComparatorGreaterThan,
    QuantityComparatorAd);
  TFhirQuantityComparatorEnumList = set of TFhirQuantityComparatorEnum;


  // Codes that describe the types of constraints possible on a question item that has a list of permitted answers (from http://hl7.org/fhir/ValueSet/questionnaire-answer-constraint)
  TFhirQuestionnaireAnswerConstraintEnum = (
    QuestionnaireAnswerConstraintNull, // Value is missing from Instance
    QuestionnaireAnswerConstraintOptionsOnly,
    QuestionnaireAnswerConstraintOptionsOrType,
    QuestionnaireAnswerConstraintOptionsOrString);
  TFhirQuestionnaireAnswerConstraintEnumList = set of TFhirQuestionnaireAnswerConstraintEnum;


  // Codes that guide the display of disabled questionnaire items (from http://hl7.org/fhir/ValueSet/questionnaire-disabled-display)
  TFhirQuestionnaireItemDisabledDisplayEnum = (
    QuestionnaireItemDisabledDisplayNull, // Value is missing from Instance
    QuestionnaireItemDisabledDisplayHidden,
    QuestionnaireItemDisabledDisplayProtected);
  TFhirQuestionnaireItemDisabledDisplayEnumList = set of TFhirQuestionnaireItemDisabledDisplayEnum;


  // The criteria by which a question is enabled. (from http://hl7.org/fhir/ValueSet/questionnaire-enable-operator)
  TFhirQuestionnaireItemOperatorEnum = (
    QuestionnaireItemOperatorNull, // Value is missing from Instance
    QuestionnaireItemOperatorExists,
    QuestionnaireItemOperatorEqual,
    QuestionnaireItemOperatorNotEqual,
    QuestionnaireItemOperatorGreaterThan,
    QuestionnaireItemOperatorLessThan,
    QuestionnaireItemOperatorGreaterOrEquals,
    QuestionnaireItemOperatorLessOrEquals);
  TFhirQuestionnaireItemOperatorEnumList = set of TFhirQuestionnaireItemOperatorEnum;


  // Distinguishes groups from questions and display text and indicates data type for questions. (from http://hl7.org/fhir/ValueSet/item-type)
  TFhirQuestionnaireItemTypeEnum = (
    QuestionnaireItemTypeNull, // Value is missing from Instance
    QuestionnaireItemTypeGroup,
    QuestionnaireItemTypeDisplay,
    QuestionnaireItemTypeQuestion,
    QuestionnaireItemTypeBoolean,
    QuestionnaireItemTypeDecimal,
    QuestionnaireItemTypeInteger,
    QuestionnaireItemTypeDate,
    QuestionnaireItemTypeDateTime,
    QuestionnaireItemTypeTime,
    QuestionnaireItemTypeString,
    QuestionnaireItemTypeText,
    QuestionnaireItemTypeUrl,
    QuestionnaireItemTypeCoding,
    QuestionnaireItemTypeAttachment,
    QuestionnaireItemTypeReference,
    QuestionnaireItemTypeQuantity);
  TFhirQuestionnaireItemTypeEnumList = set of TFhirQuestionnaireItemTypeEnum;


  // Lifecycle status of the questionnaire response. (from http://hl7.org/fhir/ValueSet/questionnaire-answers-status)
  TFhirQuestionnaireResponseStatusEnum = (
    QuestionnaireResponseStatusNull, // Value is missing from Instance
    QuestionnaireResponseStatusInProgress,
    QuestionnaireResponseStatusCompleted,
    QuestionnaireResponseStatusAmended,
    QuestionnaireResponseStatusEnteredInError,
    QuestionnaireResponseStatusStopped);
  TFhirQuestionnaireResponseStatusEnumList = set of TFhirQuestionnaireResponseStatusEnum;


  // A set of flags that defines how references are supported. (from http://hl7.org/fhir/ValueSet/reference-handling-policy)
  TFhirReferenceHandlingPolicyEnum = (
    ReferenceHandlingPolicyNull, // Value is missing from Instance
    ReferenceHandlingPolicyLiteral,
    ReferenceHandlingPolicyLogical,
    ReferenceHandlingPolicyResolves,
    ReferenceHandlingPolicyEnforced,
    ReferenceHandlingPolicyLocal);
  TFhirReferenceHandlingPolicyEnumList = set of TFhirReferenceHandlingPolicyEnum;


  // Whether a reference needs to be version specific or version independent, or whether either can be used. (from http://hl7.org/fhir/ValueSet/reference-version-rules)
  TFhirReferenceVersionRulesEnum = (
    ReferenceVersionRulesNull, // Value is missing from Instance
    ReferenceVersionRulesEither,
    ReferenceVersionRulesIndependent,
    ReferenceVersionRulesSpecific);
  TFhirReferenceVersionRulesEnumList = set of TFhirReferenceVersionRulesEnum;


  // The type of relationship to the related artifact. (from http://hl7.org/fhir/ValueSet/related-artifact-type)
  TFhirRelatedArtifactTypeEnum = (
    RelatedArtifactTypeNull, // Value is missing from Instance
    RelatedArtifactTypeDocumentation,
    RelatedArtifactTypeJustification,
    RelatedArtifactTypeCitation,
    RelatedArtifactTypePredecessor,
    RelatedArtifactTypeSuccessor,
    RelatedArtifactTypeDerivedFrom,
    RelatedArtifactTypeDependsOn,
    RelatedArtifactTypeComposedOf,
    RelatedArtifactTypePartOf,
    RelatedArtifactTypeAmends,
    RelatedArtifactTypeAmendedWith,
    RelatedArtifactTypeAppends,
    RelatedArtifactTypeAppendedWith,
    RelatedArtifactTypeCites,
    RelatedArtifactTypeCitedBy,
    RelatedArtifactTypeCommentsOn,
    RelatedArtifactTypeCommentIn,
    RelatedArtifactTypeContains,
    RelatedArtifactTypeContainedIn,
    RelatedArtifactTypeCorrects,
    RelatedArtifactTypeCorrectionIn,
    RelatedArtifactTypeReplaces,
    RelatedArtifactTypeReplacedWith,
    RelatedArtifactTypeRetracts,
    RelatedArtifactTypeRetractedBy,
    RelatedArtifactTypeSigns,
    RelatedArtifactTypeSimilarTo,
    RelatedArtifactTypeSupports,
    RelatedArtifactTypeSupportedWith,
    RelatedArtifactTypeTransforms,
    RelatedArtifactTypeTransformedInto,
    RelatedArtifactTypeTransformedWith,
    RelatedArtifactTypeDocuments,
    RelatedArtifactTypeSpecificationOf,
    RelatedArtifactTypeCreatedWith,
    RelatedArtifactTypeCiteAs);
  TFhirRelatedArtifactTypeEnumList = set of TFhirRelatedArtifactTypeEnum;


  // The type of relationship to the cited artifact. (from http://hl7.org/fhir/ValueSet/related-artifact-type-expanded)
  TFhirRelatedArtifactTypeExpandedEnum = (
    RelatedArtifactTypeExpandedNull, // Value is missing from Instance
    RelatedArtifactTypeExpandedDocumentation,
    RelatedArtifactTypeExpandedJustification,
    RelatedArtifactTypeExpandedCitation,
    RelatedArtifactTypeExpandedPredecessor,
    RelatedArtifactTypeExpandedSuccessor,
    RelatedArtifactTypeExpandedDerivedFrom,
    RelatedArtifactTypeExpandedDependsOn,
    RelatedArtifactTypeExpandedComposedOf,
    RelatedArtifactTypeExpandedPartOf,
    RelatedArtifactTypeExpandedAmends,
    RelatedArtifactTypeExpandedAmendedWith,
    RelatedArtifactTypeExpandedAppends,
    RelatedArtifactTypeExpandedAppendedWith,
    RelatedArtifactTypeExpandedCites,
    RelatedArtifactTypeExpandedCitedBy,
    RelatedArtifactTypeExpandedCommentsOn,
    RelatedArtifactTypeExpandedCommentIn,
    RelatedArtifactTypeExpandedContains,
    RelatedArtifactTypeExpandedContainedIn,
    RelatedArtifactTypeExpandedCorrects,
    RelatedArtifactTypeExpandedCorrectionIn,
    RelatedArtifactTypeExpandedReplaces,
    RelatedArtifactTypeExpandedReplacedWith,
    RelatedArtifactTypeExpandedRetracts,
    RelatedArtifactTypeExpandedRetractedBy,
    RelatedArtifactTypeExpandedSigns,
    RelatedArtifactTypeExpandedSimilarTo,
    RelatedArtifactTypeExpandedSupports,
    RelatedArtifactTypeExpandedSupportedWith,
    RelatedArtifactTypeExpandedTransforms,
    RelatedArtifactTypeExpandedTransformedInto,
    RelatedArtifactTypeExpandedTransformedWith,
    RelatedArtifactTypeExpandedDocuments,
    RelatedArtifactTypeExpandedSpecificationOf,
    RelatedArtifactTypeExpandedCreatedWith,
    RelatedArtifactTypeExpandedCiteAs,
    RelatedArtifactTypeExpandedReprint,
    RelatedArtifactTypeExpandedReprintOf);
  TFhirRelatedArtifactTypeExpandedEnumList = set of TFhirRelatedArtifactTypeExpandedEnum;


  // The type of relationship between reports. (from http://hl7.org/fhir/ValueSet/report-relation-type)
  TFhirReportRelationshipTypeEnum = (
    ReportRelationshipTypeNull, // Value is missing from Instance
    ReportRelationshipTypeReplaces,
    ReportRelationshipTypeAmends,
    ReportRelationshipTypeAppends,
    ReportRelationshipTypeTransforms,
    ReportRelationshipTypeReplacedWith,
    ReportRelationshipTypeAmendedWith,
    ReportRelationshipTypeAppendedWith,
    ReportRelationshipTypeTransformedWith);
  TFhirReportRelationshipTypeEnumList = set of TFhirReportRelationshipTypeEnum;


  // Codes indicating the degree of authority/intentionality associated with a request. (from http://hl7.org/fhir/ValueSet/request-intent)
  TFhirRequestIntentEnum = (
    RequestIntentNull, // Value is missing from Instance
    RequestIntentProposal,
    RequestIntentPlan,
    RequestIntentDirective,
    RequestIntentOrder,
    RequestIntentOriginalOrder,
    RequestIntentReflexOrder,
    RequestIntentFillerOrder,
    RequestIntentInstanceOrder,
    RequestIntentOption);
  TFhirRequestIntentEnumList = set of TFhirRequestIntentEnum;


  // Identifies the level of importance to be assigned to actioning the request. (from http://hl7.org/fhir/ValueSet/request-priority)
  TFhirRequestPriorityEnum = (
    RequestPriorityNull, // Value is missing from Instance
    RequestPriorityRoutine,
    RequestPriorityUrgent,
    RequestPriorityAsap,
    RequestPriorityStat);
  TFhirRequestPriorityEnumList = set of TFhirRequestPriorityEnum;


  // All Resource Types that represent request resources (from http://hl7.org/fhir/ValueSet/request-resource-types)
  TFhirRequestResourceTypesEnum = (
    RequestResourceTypesNull, // Value is missing from Instance
    RequestResourceTypesAppointment,
    RequestResourceTypesAppointmentResponse,
    RequestResourceTypesCarePlan,
    RequestResourceTypesClaim,
    RequestResourceTypesCommunicationRequest,
    RequestResourceTypesContract,
    RequestResourceTypesCoverageEligibilityRequest,
    RequestResourceTypesDeviceRequest,
    RequestResourceTypesEnrollmentRequest,
    RequestResourceTypesImmunizationRecommendation,
    RequestResourceTypesMedicationRequest,
    RequestResourceTypesNutritionOrder,
    RequestResourceTypesRequestOrchestration,
    RequestResourceTypesServiceRequest,
    RequestResourceTypesSupplyRequest,
    RequestResourceTypesVisionPrescription);
  TFhirRequestResourceTypesEnumList = set of TFhirRequestResourceTypesEnum;


  // Codes identifying the lifecycle stage of a request. (from http://hl7.org/fhir/ValueSet/request-status)
  TFhirRequestStatusEnum = (
    RequestStatusNull, // Value is missing from Instance
    RequestStatusDraft,
    RequestStatusActive,
    RequestStatusOnHold,
    RequestStatusRevoked,
    RequestStatusCompleted,
    RequestStatusEnteredInError,
    RequestStatusUnknown);
  TFhirRequestStatusEnumList = set of TFhirRequestStatusEnum;


  // All fhir data types (from http://hl7.org/fhir/ValueSet/resource-types)
  TFhirResourceTypesEnum = (
    ResourceTypesNull, // Value is missing from Instance
    ResourceTypesAccount,
    ResourceTypesActivityDefinition,
    ResourceTypesActorDefinition,
    ResourceTypesAdministrableProductDefinition,
    ResourceTypesAdverseEvent,
    ResourceTypesAllergyIntolerance,
    ResourceTypesAppointment,
    ResourceTypesAppointmentResponse,
    ResourceTypesArtifactAssessment,
    ResourceTypesAuditEvent,
    ResourceTypesBasic,
    ResourceTypesBinary,
    ResourceTypesBiologicallyDerivedProduct,
    ResourceTypesBodyStructure,
    ResourceTypesBundle,
    ResourceTypesCapabilityStatement,
    ResourceTypesCarePlan,
    ResourceTypesCareTeam,
    ResourceTypesChargeItem,
    ResourceTypesChargeItemDefinition,
    ResourceTypesCitation,
    ResourceTypesClaim,
    ResourceTypesClaimResponse,
    ResourceTypesClinicalImpression,
    ResourceTypesClinicalUseDefinition,
    ResourceTypesCodeSystem,
    ResourceTypesCommunication,
    ResourceTypesCommunicationRequest,
    ResourceTypesCompartmentDefinition,
    ResourceTypesComposition,
    ResourceTypesConceptMap,
    ResourceTypesCondition,
    ResourceTypesConditionDefinition,
    ResourceTypesConsent,
    ResourceTypesContract,
    ResourceTypesCoverage,
    ResourceTypesCoverageEligibilityRequest,
    ResourceTypesCoverageEligibilityResponse,
    ResourceTypesDetectedIssue,
    ResourceTypesDevice,
    ResourceTypesDeviceDefinition,
    ResourceTypesDeviceDispense,
    ResourceTypesDeviceMetric,
    ResourceTypesDeviceRequest,
    ResourceTypesDeviceUsage,
    ResourceTypesDiagnosticReport,
    ResourceTypesDocumentManifest,
    ResourceTypesDocumentReference,
    ResourceTypesEncounter,
    ResourceTypesEndpoint,
    ResourceTypesEnrollmentRequest,
    ResourceTypesEnrollmentResponse,
    ResourceTypesEpisodeOfCare,
    ResourceTypesEventDefinition,
    ResourceTypesEvidence,
    ResourceTypesEvidenceReport,
    ResourceTypesEvidenceVariable,
    ResourceTypesExampleScenario,
    ResourceTypesExplanationOfBenefit,
    ResourceTypesFamilyMemberHistory,
    ResourceTypesFlag,
    ResourceTypesFormularyItem,
    ResourceTypesGenomicStudy,
    ResourceTypesGoal,
    ResourceTypesGraphDefinition,
    ResourceTypesGroup,
    ResourceTypesGuidanceResponse,
    ResourceTypesHealthcareService,
    ResourceTypesImagingSelection,
    ResourceTypesImagingStudy,
    ResourceTypesImmunization,
    ResourceTypesImmunizationEvaluation,
    ResourceTypesImmunizationRecommendation,
    ResourceTypesImplementationGuide,
    ResourceTypesIngredient,
    ResourceTypesInsurancePlan,
    ResourceTypesInventoryReport,
    ResourceTypesInvoice,
    ResourceTypesLibrary,
    ResourceTypesLinkage,
    ResourceTypesList,
    ResourceTypesLocation,
    ResourceTypesManufacturedItemDefinition,
    ResourceTypesMeasure,
    ResourceTypesMeasureReport,
    ResourceTypesMedication,
    ResourceTypesMedicationAdministration,
    ResourceTypesMedicationDispense,
    ResourceTypesMedicationKnowledge,
    ResourceTypesMedicationRequest,
    ResourceTypesMedicationUsage,
    ResourceTypesMedicinalProductDefinition,
    ResourceTypesMessageDefinition,
    ResourceTypesMessageHeader,
    ResourceTypesMolecularSequence,
    ResourceTypesNamingSystem,
    ResourceTypesNutritionIntake,
    ResourceTypesNutritionOrder,
    ResourceTypesNutritionProduct,
    ResourceTypesObservation,
    ResourceTypesObservationDefinition,
    ResourceTypesOperationDefinition,
    ResourceTypesOperationOutcome,
    ResourceTypesOrganization,
    ResourceTypesOrganizationAffiliation,
    ResourceTypesPackagedProductDefinition,
    ResourceTypesParameters,
    ResourceTypesPatient,
    ResourceTypesPaymentNotice,
    ResourceTypesPaymentReconciliation,
    ResourceTypesPermission,
    ResourceTypesPerson,
    ResourceTypesPlanDefinition,
    ResourceTypesPractitioner,
    ResourceTypesPractitionerRole,
    ResourceTypesProcedure,
    ResourceTypesProvenance,
    ResourceTypesQuestionnaire,
    ResourceTypesQuestionnaireResponse,
    ResourceTypesRegulatedAuthorization,
    ResourceTypesRelatedPerson,
    ResourceTypesRequestOrchestration,
    ResourceTypesRequirements,
    ResourceTypesResearchStudy,
    ResourceTypesResearchSubject,
    ResourceTypesRiskAssessment,
    ResourceTypesSchedule,
    ResourceTypesSearchParameter,
    ResourceTypesServiceRequest,
    ResourceTypesSlot,
    ResourceTypesSpecimen,
    ResourceTypesSpecimenDefinition,
    ResourceTypesStructureDefinition,
    ResourceTypesStructureMap,
    ResourceTypesSubscription,
    ResourceTypesSubscriptionStatus,
    ResourceTypesSubscriptionTopic,
    ResourceTypesSubstance,
    ResourceTypesSubstanceDefinition,
    ResourceTypesSubstanceNucleicAcid,
    ResourceTypesSubstancePolymer,
    ResourceTypesSubstanceProtein,
    ResourceTypesSubstanceReferenceInformation,
    ResourceTypesSubstanceSourceMaterial,
    ResourceTypesSupplyDelivery,
    ResourceTypesSupplyRequest,
    ResourceTypesTask,
    ResourceTypesTerminologyCapabilities,
    ResourceTypesTestReport,
    ResourceTypesTestScript,
    ResourceTypesTransport,
    ResourceTypesValueSet,
    ResourceTypesVerificationResult,
    ResourceTypesVisionPrescription);
  TFhirResourceTypesEnumList = set of TFhirResourceTypesEnum;


  // How the system supports versioning for a resource. (from http://hl7.org/fhir/ValueSet/versioning-policy)
  TFhirResourceVersionPolicyEnum = (
    ResourceVersionPolicyNull, // Value is missing from Instance
    ResourceVersionPolicyNoVersion,
    ResourceVersionPolicyVersioned,
    ResourceVersionPolicyVersionedUpdate);
  TFhirResourceVersionPolicyEnumList = set of TFhirResourceVersionPolicyEnum;


  // The kind of response to a message. (from http://hl7.org/fhir/ValueSet/response-code)
  TFhirResponseTypeEnum = (
    ResponseTypeNull, // Value is missing from Instance
    ResponseTypeOk,
    ResponseTypeTransientError,
    ResponseTypeFatalError);
  TFhirResponseTypeEnumList = set of TFhirResponseTypeEnum;


  // The mode of a RESTful capability statement. (from http://hl7.org/fhir/ValueSet/restful-capability-mode)
  TFhirRestfulCapabilityModeEnum = (
    RestfulCapabilityModeNull, // Value is missing from Instance
    RestfulCapabilityModeClient,
    RestfulCapabilityModeServer);
  TFhirRestfulCapabilityModeEnumList = set of TFhirRestfulCapabilityModeEnum;


  // The license that applies to an Implementation Guide (using an SPDX license Identifiers, or 'not-open-source'). The binding is required but new SPDX license Identifiers are allowed to be used (https://spdx.org/licenses/). (from http://hl7.org/fhir/ValueSet/spdx-license)
  TFhirSPDXLicenseEnum = (
    SPDXLicenseNull, // Value is missing from Instance
    SPDXLicenseNotOpenSource,
    SPDXLicense0BSD,
    SPDXLicenseAAL,
    SPDXLicenseAbstyles,
    SPDXLicenseAdobe2006,
    SPDXLicenseAdobeGlyph,
    SPDXLicenseADSL,
    SPDXLicenseAFL11,
    SPDXLicenseAFL12,
    SPDXLicenseAFL20,
    SPDXLicenseAFL21,
    SPDXLicenseAFL30,
    SPDXLicenseAfmparse,
    SPDXLicenseAGPL10Only,
    SPDXLicenseAGPL10OrLater,
    SPDXLicenseAGPL30Only,
    SPDXLicenseAGPL30OrLater,
    SPDXLicenseAladdin,
    SPDXLicenseAMDPLPA,
    SPDXLicenseAML,
    SPDXLicenseAMPAS,
    SPDXLicenseANTLRPD,
    SPDXLicenseApache10,
    SPDXLicenseApache11,
    SPDXLicenseApache20,
    SPDXLicenseAPAFML,
    SPDXLicenseAPL10,
    SPDXLicenseAPSL10,
    SPDXLicenseAPSL11,
    SPDXLicenseAPSL12,
    SPDXLicenseAPSL20,
    SPDXLicenseArtistic10Cl8,
    SPDXLicenseArtistic10Perl,
    SPDXLicenseArtistic10,
    SPDXLicenseArtistic20,
    SPDXLicenseBahyph,
    SPDXLicenseBarr,
    SPDXLicenseBeerware,
    SPDXLicenseBitTorrent10,
    SPDXLicenseBitTorrent11,
    SPDXLicenseBorceux,
    SPDXLicenseBSD1Clause,
    SPDXLicenseBSD2ClauseFreeBSD,
    SPDXLicenseBSD2ClauseNetBSD,
    SPDXLicenseBSD2ClausePatent,
    SPDXLicenseBSD2Clause,
    SPDXLicenseBSD3ClauseAttribution,
    SPDXLicenseBSD3ClauseClear,
    SPDXLicenseBSD3ClauseLBNL,
    SPDXLicenseBSD3ClauseNoNuclearLicense2014,
    SPDXLicenseBSD3ClauseNoNuclearLicense,
    SPDXLicenseBSD3ClauseNoNuclearWarranty,
    SPDXLicenseBSD3Clause,
    SPDXLicenseBSD4ClauseUC,
    SPDXLicenseBSD4Clause,
    SPDXLicenseBSDProtection,
    SPDXLicenseBSDSourceCode,
    SPDXLicenseBSL10,
    SPDXLicenseBzip2105,
    SPDXLicenseBzip2106,
    SPDXLicenseCaldera,
    SPDXLicenseCATOSL11,
    SPDXLicenseCCBY10,
    SPDXLicenseCCBY20,
    SPDXLicenseCCBY25,
    SPDXLicenseCCBY30,
    SPDXLicenseCCBY40,
    SPDXLicenseCCBYNC10,
    SPDXLicenseCCBYNC20,
    SPDXLicenseCCBYNC25,
    SPDXLicenseCCBYNC30,
    SPDXLicenseCCBYNC40,
    SPDXLicenseCCBYNCND10,
    SPDXLicenseCCBYNCND20,
    SPDXLicenseCCBYNCND25,
    SPDXLicenseCCBYNCND30,
    SPDXLicenseCCBYNCND40,
    SPDXLicenseCCBYNCSA10,
    SPDXLicenseCCBYNCSA20,
    SPDXLicenseCCBYNCSA25,
    SPDXLicenseCCBYNCSA30,
    SPDXLicenseCCBYNCSA40,
    SPDXLicenseCCBYND10,
    SPDXLicenseCCBYND20,
    SPDXLicenseCCBYND25,
    SPDXLicenseCCBYND30,
    SPDXLicenseCCBYND40,
    SPDXLicenseCCBYSA10,
    SPDXLicenseCCBYSA20,
    SPDXLicenseCCBYSA25,
    SPDXLicenseCCBYSA30,
    SPDXLicenseCCBYSA40,
    SPDXLicenseCC010,
    SPDXLicenseCDDL10,
    SPDXLicenseCDDL11,
    SPDXLicenseCDLAPermissive10,
    SPDXLicenseCDLASharing10,
    SPDXLicenseCECILL10,
    SPDXLicenseCECILL11,
    SPDXLicenseCECILL20,
    SPDXLicenseCECILL21,
    SPDXLicenseCECILLB,
    SPDXLicenseCECILLC,
    SPDXLicenseClArtistic,
    SPDXLicenseCNRIJython,
    SPDXLicenseCNRIPythonGPLCompatible,
    SPDXLicenseCNRIPython,
    SPDXLicenseCondor11,
    SPDXLicenseCPAL10,
    SPDXLicenseCPL10,
    SPDXLicenseCPOL102,
    SPDXLicenseCrossword,
    SPDXLicenseCrystalStacker,
    SPDXLicenseCUAOPL10,
    SPDXLicenseCube,
    SPDXLicenseCurl,
    SPDXLicenseDFSL10,
    SPDXLicenseDiffmark,
    SPDXLicenseDOC,
    SPDXLicenseDotseqn,
    SPDXLicenseDSDP,
    SPDXLicenseDvipdfm,
    SPDXLicenseECL10,
    SPDXLicenseECL20,
    SPDXLicenseEFL10,
    SPDXLicenseEFL20,
    SPDXLicenseEGenix,
    SPDXLicenseEntessa,
    SPDXLicenseEPL10,
    SPDXLicenseEPL20,
    SPDXLicenseErlPL11,
    SPDXLicenseEUDatagrid,
    SPDXLicenseEUPL10,
    SPDXLicenseEUPL11,
    SPDXLicenseEUPL12,
    SPDXLicenseEurosym,
    SPDXLicenseFair,
    SPDXLicenseFrameworx10,
    SPDXLicenseFreeImage,
    SPDXLicenseFSFAP,
    SPDXLicenseFSFUL,
    SPDXLicenseFSFULLR,
    SPDXLicenseFTL,
    SPDXLicenseGFDL11Only,
    SPDXLicenseGFDL11OrLater,
    SPDXLicenseGFDL12Only,
    SPDXLicenseGFDL12OrLater,
    SPDXLicenseGFDL13Only,
    SPDXLicenseGFDL13OrLater,
    SPDXLicenseGiftware,
    SPDXLicenseGL2PS,
    SPDXLicenseGlide,
    SPDXLicenseGlulxe,
    SPDXLicenseGnuplot,
    SPDXLicenseGPL10Only,
    SPDXLicenseGPL10OrLater,
    SPDXLicenseGPL20Only,
    SPDXLicenseGPL20OrLater,
    SPDXLicenseGPL30Only,
    SPDXLicenseGPL30OrLater,
    SPDXLicenseGSOAP13b,
    SPDXLicenseHaskellReport,
    SPDXLicenseHPND,
    SPDXLicenseIBMPibs,
    SPDXLicenseICU,
    SPDXLicenseIJG,
    SPDXLicenseImageMagick,
    SPDXLicenseIMatix,
    SPDXLicenseImlib2,
    SPDXLicenseInfoZIP,
    SPDXLicenseIntelACPI,
    SPDXLicenseIntel,
    SPDXLicenseInterbase10,
    SPDXLicenseIPA,
    SPDXLicenseIPL10,
    SPDXLicenseISC,
    SPDXLicenseJasPer20,
    SPDXLicenseJSON,
    SPDXLicenseLAL12,
    SPDXLicenseLAL13,
    SPDXLicenseLatex2e,
    SPDXLicenseLeptonica,
    SPDXLicenseLGPL20Only,
    SPDXLicenseLGPL20OrLater,
    SPDXLicenseLGPL21Only,
    SPDXLicenseLGPL21OrLater,
    SPDXLicenseLGPL30Only,
    SPDXLicenseLGPL30OrLater,
    SPDXLicenseLGPLLR,
    SPDXLicenseLibpng,
    SPDXLicenseLibtiff,
    SPDXLicenseLiLiQP11,
    SPDXLicenseLiLiQR11,
    SPDXLicenseLiLiQRplus11,
    SPDXLicenseLinuxOpenIB,
    SPDXLicenseLPL10,
    SPDXLicenseLPL102,
    SPDXLicenseLPPL10,
    SPDXLicenseLPPL11,
    SPDXLicenseLPPL12,
    SPDXLicenseLPPL13a,
    SPDXLicenseLPPL13c,
    SPDXLicenseMakeIndex,
    SPDXLicenseMirOS,
    SPDXLicenseMIT0,
    SPDXLicenseMITAdvertising,
    SPDXLicenseMITCMU,
    SPDXLicenseMITEnna,
    SPDXLicenseMITFeh,
    SPDXLicenseMIT,
    SPDXLicenseMITNFA,
    SPDXLicenseMotosoto,
    SPDXLicenseMpich2,
    SPDXLicenseMPL10,
    SPDXLicenseMPL11,
    SPDXLicenseMPL20NoCopyleftException,
    SPDXLicenseMPL20,
    SPDXLicenseMSPL,
    SPDXLicenseMSRL,
    SPDXLicenseMTLL,
    SPDXLicenseMultics,
    SPDXLicenseMup,
    SPDXLicenseNASA13,
    SPDXLicenseNaumen,
    SPDXLicenseNBPL10,
    SPDXLicenseNCSA,
    SPDXLicenseNetSNMP,
    SPDXLicenseNetCDF,
    SPDXLicenseNewsletr,
    SPDXLicenseNGPL,
    SPDXLicenseNLOD10,
    SPDXLicenseNLPL,
    SPDXLicenseNokia,
    SPDXLicenseNOSL,
    SPDXLicenseNoweb,
    SPDXLicenseNPL10,
    SPDXLicenseNPL11,
    SPDXLicenseNPOSL30,
    SPDXLicenseNRL,
    SPDXLicenseNTP,
    SPDXLicenseOCCTPL,
    SPDXLicenseOCLC20,
    SPDXLicenseODbL10,
    SPDXLicenseOFL10,
    SPDXLicenseOFL11,
    SPDXLicenseOGTSL,
    SPDXLicenseOLDAP11,
    SPDXLicenseOLDAP12,
    SPDXLicenseOLDAP13,
    SPDXLicenseOLDAP14,
    SPDXLicenseOLDAP201,
    SPDXLicenseOLDAP20,
    SPDXLicenseOLDAP21,
    SPDXLicenseOLDAP221,
    SPDXLicenseOLDAP222,
    SPDXLicenseOLDAP22,
    SPDXLicenseOLDAP23,
    SPDXLicenseOLDAP24,
    SPDXLicenseOLDAP25,
    SPDXLicenseOLDAP26,
    SPDXLicenseOLDAP27,
    SPDXLicenseOLDAP28,
    SPDXLicenseOML,
    SPDXLicenseOpenSSL,
    SPDXLicenseOPL10,
    SPDXLicenseOSETPL21,
    SPDXLicenseOSL10,
    SPDXLicenseOSL11,
    SPDXLicenseOSL20,
    SPDXLicenseOSL21,
    SPDXLicenseOSL30,
    SPDXLicensePDDL10,
    SPDXLicensePHP30,
    SPDXLicensePHP301,
    SPDXLicensePlexus,
    SPDXLicensePostgreSQL,
    SPDXLicensePsfrag,
    SPDXLicensePsutils,
    SPDXLicensePython20,
    SPDXLicenseQhull,
    SPDXLicenseQPL10,
    SPDXLicenseRdisc,
    SPDXLicenseRHeCos11,
    SPDXLicenseRPL11,
    SPDXLicenseRPL15,
    SPDXLicenseRPSL10,
    SPDXLicenseRSAMD,
    SPDXLicenseRSCPL,
    SPDXLicenseRuby,
    SPDXLicenseSAXPD,
    SPDXLicenseSaxpath,
    SPDXLicenseSCEA,
    SPDXLicenseSendmail,
    SPDXLicenseSGIB10,
    SPDXLicenseSGIB11,
    SPDXLicenseSGIB20,
    SPDXLicenseSimPL20,
    SPDXLicenseSISSL12,
    SPDXLicenseSISSL,
    SPDXLicenseSleepycat,
    SPDXLicenseSMLNJ,
    SPDXLicenseSMPPL,
    SPDXLicenseSNIA,
    SPDXLicenseSpencer86,
    SPDXLicenseSpencer94,
    SPDXLicenseSpencer99,
    SPDXLicenseSPL10,
    SPDXLicenseSugarCRM113,
    SPDXLicenseSWL,
    SPDXLicenseTCL,
    SPDXLicenseTCPWrappers,
    SPDXLicenseTMate,
    SPDXLicenseTORQUE11,
    SPDXLicenseTOSL,
    SPDXLicenseUnicodeDFS2015,
    SPDXLicenseUnicodeDFS2016,
    SPDXLicenseUnicodeTOU,
    SPDXLicenseUnlicense,
    SPDXLicenseUPL10,
    SPDXLicenseVim,
    SPDXLicenseVOSTROM,
    SPDXLicenseVSL10,
    SPDXLicenseW3C19980720,
    SPDXLicenseW3C20150513,
    SPDXLicenseW3C,
    SPDXLicenseWatcom10,
    SPDXLicenseWsuipa,
    SPDXLicenseWTFPL,
    SPDXLicenseX11,
    SPDXLicenseXerox,
    SPDXLicenseXFree8611,
    SPDXLicenseXinetd,
    SPDXLicenseXnet,
    SPDXLicenseXpp,
    SPDXLicenseXSkat,
    SPDXLicenseYPL10,
    SPDXLicenseYPL11,
    SPDXLicenseZed,
    SPDXLicenseZend20,
    SPDXLicenseZimbra13,
    SPDXLicenseZimbra14,
    SPDXLicenseZlibAcknowledgement,
    SPDXLicenseZlib,
    SPDXLicenseZPL11,
    SPDXLicenseZPL20,
    SPDXLicenseZPL21);
  // What Search Comparator Codes are supported in search. (from http://hl7.org/fhir/ValueSet/search-comparator)
  TFhirSearchComparatorEnum = (
    SearchComparatorNull, // Value is missing from Instance
    SearchComparatorEq,
    SearchComparatorNe,
    SearchComparatorGt,
    SearchComparatorLt,
    SearchComparatorGe,
    SearchComparatorLe,
    SearchComparatorSa,
    SearchComparatorEb,
    SearchComparatorAp);
  TFhirSearchComparatorEnumList = set of TFhirSearchComparatorEnum;


  // Why an entry is in the result set - whether it's included as a match or because of an _include requirement, or to convey information or warning information about the search process. (from http://hl7.org/fhir/ValueSet/search-entry-mode)
  TFhirSearchEntryModeEnum = (
    SearchEntryModeNull, // Value is missing from Instance
    SearchEntryModeMatch,
    SearchEntryModeInclude,
    SearchEntryModeOutcome);
  TFhirSearchEntryModeEnumList = set of TFhirSearchEntryModeEnum;


  // A supported modifier for a search parameter. (from http://hl7.org/fhir/ValueSet/search-modifier-code)
  TFhirSearchModifierCodeEnum = (
    SearchModifierCodeNull, // Value is missing from Instance
    SearchModifierCodeMissing,
    SearchModifierCodeExact,
    SearchModifierCodeContains,
    SearchModifierCodeNot,
    SearchModifierCodeText,
    SearchModifierCodeIn,
    SearchModifierCodeNotIn,
    SearchModifierCodeBelow,
    SearchModifierCodeAbove,
    SearchModifierCodeType,
    SearchModifierCodeIdentifier,
    SearchModifierCodeOfType,
    SearchModifierCodeCodeText,
    SearchModifierCodeTextAdvanced,
    SearchModifierCodeIterate);
  TFhirSearchModifierCodeEnumList = set of TFhirSearchModifierCodeEnum;


  // Data types allowed to be used for search parameters. (from http://hl7.org/fhir/ValueSet/search-param-type)
  TFhirSearchParamTypeEnum = (
    SearchParamTypeNull, // Value is missing from Instance
    SearchParamTypeNumber,
    SearchParamTypeDate,
    SearchParamTypeString,
    SearchParamTypeToken,
    SearchParamTypeReference,
    SearchParamTypeComposite,
    SearchParamTypeQuantity,
    SearchParamTypeUri,
    SearchParamTypeSpecial);
  TFhirSearchParamTypeEnumList = set of TFhirSearchParamTypeEnum;


  // How a search parameter relates to the set of elements returned by evaluating its expression query. (from http://hl7.org/fhir/ValueSet/search-processingmode)
  TFhirSearchProcessingModeTypeEnum = (
    SearchProcessingModeTypeNull, // Value is missing from Instance
    SearchProcessingModeTypeNormal,
    SearchProcessingModeTypePhonetic,
    SearchProcessingModeTypeOther);
  TFhirSearchProcessingModeTypeEnumList = set of TFhirSearchProcessingModeTypeEnum;


  // Type if a sequence -- DNA, RNA, or amino acid sequence. (from http://hl7.org/fhir/ValueSet/sequence-type)
  TFhirSequenceTypeEnum = (
    SequenceTypeNull, // Value is missing from Instance
    SequenceTypeAa,
    SequenceTypeDna,
    SequenceTypeRna);
  TFhirSequenceTypeEnumList = set of TFhirSequenceTypeEnum;


  // How slices are interpreted when evaluating an instance. (from http://hl7.org/fhir/ValueSet/resource-slicing-rules)
  TFhirSlicingRulesEnum = (
    SlicingRulesNull, // Value is missing from Instance
    SlicingRulesClosed,
    SlicingRulesOpen,
    SlicingRulesOpenAtEnd);
  TFhirSlicingRulesEnumList = set of TFhirSlicingRulesEnum;


  // The free/busy status of the slot. (from http://hl7.org/fhir/ValueSet/slotstatus)
  TFhirSlotStatusEnum = (
    SlotStatusNull, // Value is missing from Instance
    SlotStatusBusy,
    SlotStatusFree,
    SlotStatusBusyUnavailable,
    SlotStatusBusyTentative,
    SlotStatusEnteredInError);
  TFhirSlotStatusEnumList = set of TFhirSlotStatusEnum;


  // The possible sort directions, ascending or descending. (from http://hl7.org/fhir/ValueSet/sort-direction)
  TFhirSortDirectionEnum = (
    SortDirectionNull, // Value is missing from Instance
    SortDirectionAscending,
    SortDirectionDescending);
  TFhirSortDirectionEnumList = set of TFhirSortDirectionEnum;


  // Codes providing the combined status of a specimen. (from http://hl7.org/fhir/ValueSet/specimen-combined)
  TFhirSpecimenCombinedEnum = (
    SpecimenCombinedNull, // Value is missing from Instance
    SpecimenCombinedGrouped,
    SpecimenCombinedPooled);
  TFhirSpecimenCombinedEnumList = set of TFhirSpecimenCombinedEnum;


  // Degree of preference of a type of conditioned specimen. (from http://hl7.org/fhir/ValueSet/specimen-contained-preference)
  TFhirSpecimenContainedPreferenceEnum = (
    SpecimenContainedPreferenceNull, // Value is missing from Instance
    SpecimenContainedPreferencePreferred,
    SpecimenContainedPreferenceAlternate);
  TFhirSpecimenContainedPreferenceEnumList = set of TFhirSpecimenContainedPreferenceEnum;


  // Codes providing the status/availability of a specimen. (from http://hl7.org/fhir/ValueSet/specimen-status)
  TFhirSpecimenStatusEnum = (
    SpecimenStatusNull, // Value is missing from Instance
    SpecimenStatusAvailable,
    SpecimenStatusUnavailable,
    SpecimenStatusUnsatisfactory,
    SpecimenStatusEnteredInError);
  TFhirSpecimenStatusEnumList = set of TFhirSpecimenStatusEnum;


  // The validation status of the target (from http://hl7.org/fhir/ValueSet/verificationresult-status)
  TFhirStatusEnum = (
    StatusNull, // Value is missing from Instance
    StatusAttested,
    StatusValidated,
    StatusInProcess,
    StatusReqRevalid,
    StatusValFail,
    StatusRevalFail);
  TFhirStatusEnumList = set of TFhirStatusEnum;


  // Type for strand. (from http://hl7.org/fhir/ValueSet/strand-type)
  TFhirStrandTypeEnum = (
    StrandTypeNull, // Value is missing from Instance
    StrandTypeWatson,
    StrandTypeCrick);
  TFhirStrandTypeEnumList = set of TFhirStrandTypeEnum;


  // Defines the type of structure that a definition is describing. (from http://hl7.org/fhir/ValueSet/structure-definition-kind)
  TFhirStructureDefinitionKindEnum = (
    StructureDefinitionKindNull, // Value is missing from Instance
    StructureDefinitionKindPrimitiveType,
    StructureDefinitionKindComplexType,
    StructureDefinitionKindResource,
    StructureDefinitionKindLogical);
  TFhirStructureDefinitionKindEnumList = set of TFhirStructureDefinitionKindEnum;


  // If this is the default rule set to apply for the source type, or this combination of types. (from http://hl7.org/fhir/ValueSet/map-group-type-mode)
  TFhirStructureMapGroupTypeModeEnum = (
    StructureMapGroupTypeModeNull, // Value is missing from Instance
    StructureMapGroupTypeModeTypes,
    StructureMapGroupTypeModeTypeAndTypes);
  TFhirStructureMapGroupTypeModeEnumList = set of TFhirStructureMapGroupTypeModeEnum;


  // Mode for this instance of data. (from http://hl7.org/fhir/ValueSet/map-input-mode)
  TFhirStructureMapInputModeEnum = (
    StructureMapInputModeNull, // Value is missing from Instance
    StructureMapInputModeSource,
    StructureMapInputModeTarget);
  TFhirStructureMapInputModeEnumList = set of TFhirStructureMapInputModeEnum;


  // How the referenced structure is used in this mapping. (from http://hl7.org/fhir/ValueSet/map-model-mode)
  TFhirStructureMapModelModeEnum = (
    StructureMapModelModeNull, // Value is missing from Instance
    StructureMapModelModeSource,
    StructureMapModelModeQueried,
    StructureMapModelModeTarget,
    StructureMapModelModeProduced);
  TFhirStructureMapModelModeEnumList = set of TFhirStructureMapModelModeEnum;


  // If field is a list, how to manage the source. (from http://hl7.org/fhir/ValueSet/map-source-list-mode)
  TFhirStructureMapSourceListModeEnum = (
    StructureMapSourceListModeNull, // Value is missing from Instance
    StructureMapSourceListModeFirst,
    StructureMapSourceListModeNotFirst,
    StructureMapSourceListModeLast,
    StructureMapSourceListModeNotLast,
    StructureMapSourceListModeOnlyOne);
  TFhirStructureMapSourceListModeEnumList = set of TFhirStructureMapSourceListModeEnum;


  // If field is a list, how to manage the production. (from http://hl7.org/fhir/ValueSet/map-target-list-mode)
  TFhirStructureMapTargetListModeEnum = (
    StructureMapTargetListModeNull, // Value is missing from Instance
    StructureMapTargetListModeFirst,
    StructureMapTargetListModeShare,
    StructureMapTargetListModeLast,
    StructureMapTargetListModeCollate);
  TFhirStructureMapTargetListModeEnumList = set of TFhirStructureMapTargetListModeEnum;


  // How data is copied/created. (from http://hl7.org/fhir/ValueSet/map-transform)
  TFhirStructureMapTransformEnum = (
    StructureMapTransformNull, // Value is missing from Instance
    StructureMapTransformCreate,
    StructureMapTransformCopy,
    StructureMapTransformTruncate,
    StructureMapTransformEscape,
    StructureMapTransformCast,
    StructureMapTransformAppend,
    StructureMapTransformTranslate,
    StructureMapTransformReference,
    StructureMapTransformDateOp,
    StructureMapTransformUuid,
    StructureMapTransformPointer,
    StructureMapTransformEvaluate,
    StructureMapTransformCc,
    StructureMapTransformC,
    StructureMapTransformQty,
    StructureMapTransformId,
    StructureMapTransformCp);
  TFhirStructureMapTransformEnumList = set of TFhirStructureMapTransformEnum;


  // Concepts for how a measure report consumer and receiver coordinate data exchange updates. The choices are snapshot or incremental updates (from http://hl7.org/fhir/ValueSet/submit-data-update-type)
  TFhirSubmitDataUpdateTypeEnum = (
    SubmitDataUpdateTypeNull, // Value is missing from Instance
    SubmitDataUpdateTypeIncremental,
    SubmitDataUpdateTypeSnapshot);
  TFhirSubmitDataUpdateTypeEnumList = set of TFhirSubmitDataUpdateTypeEnum;


  // The type of notification represented by the status message. (from http://hl7.org/fhir/ValueSet/subscription-notification-type)
  TFhirSubscriptionNotificationTypeEnum = (
    SubscriptionNotificationTypeNull, // Value is missing from Instance
    SubscriptionNotificationTypeHandshake,
    SubscriptionNotificationTypeHeartbeat,
    SubscriptionNotificationTypeEventNotification,
    SubscriptionNotificationTypeQueryStatus,
    SubscriptionNotificationTypeQueryEvent);
  TFhirSubscriptionNotificationTypeEnumList = set of TFhirSubscriptionNotificationTypeEnum;


  // Codes to represent how much resource content to send in the notification payload. (from http://hl7.org/fhir/ValueSet/subscription-payload-content)
  TFhirSubscriptionPayloadContentEnum = (
    SubscriptionPayloadContentNull, // Value is missing from Instance
    SubscriptionPayloadContentEmpty,
    SubscriptionPayloadContentIdOnly,
    SubscriptionPayloadContentFullResource);
  TFhirSubscriptionPayloadContentEnumList = set of TFhirSubscriptionPayloadContentEnum;


  // FHIR search modifiers allowed for use in Subscriptions and SubscriptionTopics. (from http://hl7.org/fhir/ValueSet/subscription-search-modifier)
  TFhirSubscriptionSearchModifierEnum = (
    SubscriptionSearchModifierNull, // Value is missing from Instance
    SubscriptionSearchModifierEqual,
    SubscriptionSearchModifierEq,
    SubscriptionSearchModifierNe,
    SubscriptionSearchModifierGt,
    SubscriptionSearchModifierLt,
    SubscriptionSearchModifierGe,
    SubscriptionSearchModifierLe,
    SubscriptionSearchModifierSa,
    SubscriptionSearchModifierEb,
    SubscriptionSearchModifierAp,
    SubscriptionSearchModifierAbove,
    SubscriptionSearchModifierBelow,
    SubscriptionSearchModifierIn,
    SubscriptionSearchModifierNotIn,
    SubscriptionSearchModifierOfType);
  TFhirSubscriptionSearchModifierEnumList = set of TFhirSubscriptionSearchModifierEnum;


  // State values for FHIR Subscriptions. (from http://hl7.org/fhir/ValueSet/subscription-status)
  TFhirSubscriptionStatusCodesEnum = (
    SubscriptionStatusCodesNull, // Value is missing from Instance
    SubscriptionStatusCodesRequested,
    SubscriptionStatusCodesActive,
    SubscriptionStatusCodesError,
    SubscriptionStatusCodesOff,
    SubscriptionStatusCodesEnteredInError);
  TFhirSubscriptionStatusCodesEnumList = set of TFhirSubscriptionStatusCodesEnum;


  // Status of the supply delivery. (from http://hl7.org/fhir/ValueSet/supplydelivery-status)
  TFhirSupplyDeliveryStatusEnum = (
    SupplyDeliveryStatusNull, // Value is missing from Instance
    SupplyDeliveryStatusInProgress,
    SupplyDeliveryStatusCompleted,
    SupplyDeliveryStatusAbandoned,
    SupplyDeliveryStatusEnteredInError);
  TFhirSupplyDeliveryStatusEnumList = set of TFhirSupplyDeliveryStatusEnum;


  // Status of the supply request. (from http://hl7.org/fhir/ValueSet/supplyrequest-status)
  TFhirSupplyRequestStatusEnum = (
    SupplyRequestStatusNull, // Value is missing from Instance
    SupplyRequestStatusDraft,
    SupplyRequestStatusActive,
    SupplyRequestStatusSuspended,
    SupplyRequestStatusCancelled,
    SupplyRequestStatusCompleted,
    SupplyRequestStatusEnteredInError,
    SupplyRequestStatusUnknown);
  TFhirSupplyRequestStatusEnumList = set of TFhirSupplyRequestStatusEnum;


  // Operations supported by REST at the system level. (from http://hl7.org/fhir/ValueSet/system-restful-interaction)
  TFhirSystemRestfulInteractionEnum = (
    SystemRestfulInteractionNull, // Value is missing from Instance
    SystemRestfulInteractionTransaction,
    SystemRestfulInteractionBatch,
    SystemRestfulInteractionSearchSystem,
    SystemRestfulInteractionHistorySystem);
  TFhirSystemRestfulInteractionEnumList = set of TFhirSystemRestfulInteractionEnum;


  // Distinguishes whether the task is a proposal, plan or full order. (from http://hl7.org/fhir/ValueSet/task-intent)
  TFhirTaskIntentEnum = (
    TaskIntentNull, // Value is missing from Instance
    TaskIntentUnknown,
    TaskIntentProposal,
    TaskIntentPlan,
    TaskIntentOrder,
    TaskIntentOriginalOrder,
    TaskIntentReflexOrder,
    TaskIntentFillerOrder,
    TaskIntentInstanceOrder,
    TaskIntentOption);
  TFhirTaskIntentEnumList = set of TFhirTaskIntentEnum;


  // The current status of the task. (from http://hl7.org/fhir/ValueSet/task-status)
  TFhirTaskStatusEnum = (
    TaskStatusNull, // Value is missing from Instance
    TaskStatusDraft,
    TaskStatusRequested,
    TaskStatusReceived,
    TaskStatusAccepted,
    TaskStatusRejected,
    TaskStatusReady,
    TaskStatusCancelled,
    TaskStatusInProgress,
    TaskStatusOnHold,
    TaskStatusFailed,
    TaskStatusCompleted,
    TaskStatusEnteredInError);
  TFhirTaskStatusEnumList = set of TFhirTaskStatusEnum;


  // The results of executing an action. (from http://hl7.org/fhir/ValueSet/report-action-result-codes)
  TFhirTestReportActionResultEnum = (
    TestReportActionResultNull, // Value is missing from Instance
    TestReportActionResultPass,
    TestReportActionResultSkip,
    TestReportActionResultFail,
    TestReportActionResultWarning,
    TestReportActionResultError);
  TFhirTestReportActionResultEnumList = set of TFhirTestReportActionResultEnum;


  // The type of participant. (from http://hl7.org/fhir/ValueSet/report-participant-type)
  TFhirTestReportParticipantTypeEnum = (
    TestReportParticipantTypeNull, // Value is missing from Instance
    TestReportParticipantTypeTestEngine,
    TestReportParticipantTypeClient,
    TestReportParticipantTypeServer);
  TFhirTestReportParticipantTypeEnumList = set of TFhirTestReportParticipantTypeEnum;


  // The reported execution result. (from http://hl7.org/fhir/ValueSet/report-result-codes)
  TFhirTestReportResultEnum = (
    TestReportResultNull, // Value is missing from Instance
    TestReportResultPass,
    TestReportResultFail,
    TestReportResultPending);
  TFhirTestReportResultEnumList = set of TFhirTestReportResultEnum;


  // The current status of the test report. (from http://hl7.org/fhir/ValueSet/report-status-codes)
  TFhirTestReportStatusEnum = (
    TestReportStatusNull, // Value is missing from Instance
    TestReportStatusCompleted,
    TestReportStatusInProgress,
    TestReportStatusWaiting,
    TestReportStatusStopped,
    TestReportStatusEnteredInError);
  TFhirTestReportStatusEnumList = set of TFhirTestReportStatusEnum;


  // The allowable request method or HTTP operation codes. (from http://hl7.org/fhir/ValueSet/http-operations)
  TFhirTestScriptRequestMethodCodeEnum = (
    TestScriptRequestMethodCodeNull, // Value is missing from Instance
    TestScriptRequestMethodCodeDelete,
    TestScriptRequestMethodCodeGet,
    TestScriptRequestMethodCodeOptions,
    TestScriptRequestMethodCodePatch,
    TestScriptRequestMethodCodePost,
    TestScriptRequestMethodCodePut,
    TestScriptRequestMethodCodeHead);
  TFhirTestScriptRequestMethodCodeEnumList = set of TFhirTestScriptRequestMethodCodeEnum;


  // Distinguishes whether the transport is a proposal, plan or full order. (from http://hl7.org/fhir/ValueSet/transport-intent)
  TFhirTransportIntentEnum = (
    TransportIntentNull, // Value is missing from Instance
    TransportIntentUnknown,
    TransportIntentProposal,
    TransportIntentPlan,
    TransportIntentOrder,
    TransportIntentOriginalOrder,
    TransportIntentReflexOrder,
    TransportIntentFillerOrder,
    TransportIntentInstanceOrder,
    TransportIntentOption);
  TFhirTransportIntentEnumList = set of TFhirTransportIntentEnum;


  // Status of the transport (from http://hl7.org/fhir/ValueSet/transport-status)
  TFhirTransportStatusEnum = (
    TransportStatusNull, // Value is missing from Instance
    TransportStatusInProgress,
    TransportStatusCompleted,
    TransportStatusAbandoned,
    TransportStatusCancelled,
    TransportStatusPlanned,
    TransportStatusEnteredInError);
  TFhirTransportStatusEnumList = set of TFhirTransportStatusEnum;


  // The type of trigger. (from http://hl7.org/fhir/ValueSet/trigger-type)
  TFhirTriggerTypeEnum = (
    TriggerTypeNull, // Value is missing from Instance
    TriggerTypeNamedEvent,
    TriggerTypePeriodic,
    TriggerTypeDataChanged,
    TriggerTypeDataAdded,
    TriggerTypeDataModified,
    TriggerTypeDataRemoved,
    TriggerTypeDataAccessed,
    TriggerTypeDataAccessEnded);
  TFhirTriggerTypeEnumList = set of TFhirTriggerTypeEnum;


  // Codes providing the type of triggeredBy observation. (from http://hl7.org/fhir/ValueSet/observation-triggeredbytype)
  TFhirTriggeredBytypeEnum = (
    TriggeredBytypeNull, // Value is missing from Instance
    TriggeredBytypeReflex,
    TriggeredBytypeRepeat,
    TriggeredBytypeReRun);
  TFhirTriggeredBytypeEnumList = set of TFhirTriggeredBytypeEnum;


  // How a type relates to its baseDefinition. (from http://hl7.org/fhir/ValueSet/type-derivation-rule)
  TFhirTypeDerivationRuleEnum = (
    TypeDerivationRuleNull, // Value is missing from Instance
    TypeDerivationRuleSpecialization,
    TypeDerivationRuleConstraint);
  TFhirTypeDerivationRuleEnumList = set of TFhirTypeDerivationRuleEnum;


  // Operations supported by REST at the type or instance level. (from http://hl7.org/fhir/ValueSet/type-restful-interaction)
  TFhirTypeRestfulInteractionEnum = (
    TypeRestfulInteractionNull, // Value is missing from Instance
    TypeRestfulInteractionRead,
    TypeRestfulInteractionVread,
    TypeRestfulInteractionUpdate,
    TypeRestfulInteractionPatch,
    TypeRestfulInteractionDelete,
    TypeRestfulInteractionHistoryInstance,
    TypeRestfulInteractionHistoryType,
    TypeRestfulInteractionCreate,
    TypeRestfulInteractionSearchType);
  TFhirTypeRestfulInteractionEnumList = set of TFhirTypeRestfulInteractionEnum;


  // Codes to identify how UDI data was entered. (from http://hl7.org/fhir/ValueSet/udi-entry-type)
  TFhirUDIEntryTypeEnum = (
    UDIEntryTypeNull, // Value is missing from Instance
    UDIEntryTypeBarcode,
    UDIEntryTypeRfid,
    UDIEntryTypeManual,
    UDIEntryTypeCard,
    UDIEntryTypeSelfReported,
    UDIEntryTypeElectronicTransmission,
    UDIEntryTypeUnknown);
  TFhirUDIEntryTypeEnumList = set of TFhirUDIEntryTypeEnum;


  // A unit of time (units from UCUM). (from http://hl7.org/fhir/ValueSet/units-of-time)
  TFhirUnitsOfTimeEnum = (
    UnitsOfTimeNull, // Value is missing from Instance
    UnitsOfTimeS,
    UnitsOfTimeMin,
    UnitsOfTimeH,
    UnitsOfTimeD,
    UnitsOfTimeWk,
    UnitsOfTimeMo,
    UnitsOfTimeA);
  TFhirUnitsOfTimeEnumList = set of TFhirUnitsOfTimeEnum;


  // The purpose of the Claim: predetermination, preauthorization, claim. (from http://hl7.org/fhir/ValueSet/claim-use)
  TFhirUseEnum = (
    UseNull, // Value is missing from Instance
    UseClaim,
    UsePreauthorization,
    UsePredetermination);
  TFhirUseEnumList = set of TFhirUseEnum;


  // The type of comparator operator to use (from http://hl7.org/fhir/ValueSet/value-filter-comparator)
  TFhirValueFilterComparatorEnum = (
    ValueFilterComparatorNull, // Value is missing from Instance
    ValueFilterComparatorEq,
    ValueFilterComparatorGt,
    ValueFilterComparatorLt,
    ValueFilterComparatorGe,
    ValueFilterComparatorLe,
    ValueFilterComparatorSa,
    ValueFilterComparatorEb);
  TFhirValueFilterComparatorEnumList = set of TFhirValueFilterComparatorEnum;


  // A coded concept listing the base codes. (from http://hl7.org/fhir/ValueSet/vision-base-codes)
  TFhirVisionBaseEnum = (
    VisionBaseNull, // Value is missing from Instance
    VisionBaseUp,
    VisionBaseDown,
    VisionBaseIn,
    VisionBaseOut);
  TFhirVisionBaseEnumList = set of TFhirVisionBaseEnum;


  // A coded concept listing the eye codes. (from http://hl7.org/fhir/ValueSet/vision-eye-codes)
  TFhirVisionEyesEnum = (
    VisionEyesNull, // Value is missing from Instance
    VisionEyesRight,
    VisionEyesLeft);
  TFhirVisionEyesEnumList = set of TFhirVisionEyesEnum;




const
  CODES_TFhirAccountStatusEnum : Array[TFhirAccountStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error', 'on-hold', 'unknown');
  SYSTEMS_TFhirAccountStatusEnum : Array[TFhirAccountStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACCOUNT_STATUS, FHIR_URI_ACCOUNT_STATUS, FHIR_URI_ACCOUNT_STATUS, FHIR_URI_ACCOUNT_STATUS, FHIR_URI_ACCOUNT_STATUS);
  CODES_TFhirActionCardinalityBehaviorEnum : Array[TFhirActionCardinalityBehaviorEnum] of String = ('', 'single', 'multiple');
  SYSTEMS_TFhirActionCardinalityBehaviorEnum : Array[TFhirActionCardinalityBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_CARDINALITY_BEHAVIOR, FHIR_URI_ACTION_CARDINALITY_BEHAVIOR);
  CODES_TFhirActionConditionKindEnum : Array[TFhirActionConditionKindEnum] of String = ('', 'applicability', 'start', 'stop');
  SYSTEMS_TFhirActionConditionKindEnum : Array[TFhirActionConditionKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_CONDITION_KIND, FHIR_URI_ACTION_CONDITION_KIND, FHIR_URI_ACTION_CONDITION_KIND);
  CODES_TFhirActionGroupingBehaviorEnum : Array[TFhirActionGroupingBehaviorEnum] of String = ('', 'visual-group', 'logical-group', 'sentence-group');
  SYSTEMS_TFhirActionGroupingBehaviorEnum : Array[TFhirActionGroupingBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_GROUPING_BEHAVIOR, FHIR_URI_ACTION_GROUPING_BEHAVIOR, FHIR_URI_ACTION_GROUPING_BEHAVIOR);
  CODES_TFhirActionParticipantTypeEnum : Array[TFhirActionParticipantTypeEnum] of String = ('', 'careteam', 'device', 'group', 'healthcareservice', 'location', 'organization', 'patient', 'practitioner', 'practitionerrole', 'relatedperson');
  SYSTEMS_TFhirActionParticipantTypeEnum : Array[TFhirActionParticipantTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE, FHIR_URI_ACTION_PARTICIPANT_TYPE);
  CODES_TFhirActionPrecheckBehaviorEnum : Array[TFhirActionPrecheckBehaviorEnum] of String = ('', 'yes', 'no');
  SYSTEMS_TFhirActionPrecheckBehaviorEnum : Array[TFhirActionPrecheckBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_PRECHECK_BEHAVIOR, FHIR_URI_ACTION_PRECHECK_BEHAVIOR);
  CODES_TFhirActionRelationshipTypeEnum : Array[TFhirActionRelationshipTypeEnum] of String = ('', 'before-start', 'before', 'before-end', 'concurrent-with-start', 'concurrent', 'concurrent-with-end', 'after-start', 'after', 'after-end');
  SYSTEMS_TFhirActionRelationshipTypeEnum : Array[TFhirActionRelationshipTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE, FHIR_URI_ACTION_RELATIONSHIP_TYPE);
  CODES_TFhirActionRequiredBehaviorEnum : Array[TFhirActionRequiredBehaviorEnum] of String = ('', 'must', 'could', 'must-unless-documented');
  SYSTEMS_TFhirActionRequiredBehaviorEnum : Array[TFhirActionRequiredBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_REQUIRED_BEHAVIOR, FHIR_URI_ACTION_REQUIRED_BEHAVIOR, FHIR_URI_ACTION_REQUIRED_BEHAVIOR);
  CODES_TFhirActionSelectionBehaviorEnum : Array[TFhirActionSelectionBehaviorEnum] of String = ('', 'any', 'all', 'all-or-none', 'exactly-one', 'at-most-one', 'one-or-more');
  SYSTEMS_TFhirActionSelectionBehaviorEnum : Array[TFhirActionSelectionBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_ACTION_SELECTION_BEHAVIOR, FHIR_URI_ACTION_SELECTION_BEHAVIOR, FHIR_URI_ACTION_SELECTION_BEHAVIOR, FHIR_URI_ACTION_SELECTION_BEHAVIOR, FHIR_URI_ACTION_SELECTION_BEHAVIOR, FHIR_URI_ACTION_SELECTION_BEHAVIOR);
  CODES_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = ('', 'postal', 'physical', 'both');
  SYSTEMS_TFhirAddressTypeEnum : Array[TFhirAddressTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ADDRESS_TYPE, FHIR_URI_ADDRESS_TYPE, FHIR_URI_ADDRESS_TYPE);
  CODES_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = ('', 'home', 'work', 'temp', 'old', 'billing');
  SYSTEMS_TFhirAddressUseEnum : Array[TFhirAddressUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_ADDRESS_USE, FHIR_URI_ADDRESS_USE, FHIR_URI_ADDRESS_USE, FHIR_URI_ADDRESS_USE, FHIR_URI_ADDRESS_USE);
  CODES_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = ('', 'male', 'female', 'other', 'unknown');
  SYSTEMS_TFhirAdministrativeGenderEnum : Array[TFhirAdministrativeGenderEnum] of String = (FHIR_URI_NONE, FHIR_URI_ADMINISTRATIVE_GENDER, FHIR_URI_ADMINISTRATIVE_GENDER, FHIR_URI_ADMINISTRATIVE_GENDER, FHIR_URI_ADMINISTRATIVE_GENDER);
  CODES_TFhirAdverseEventActualityEnum : Array[TFhirAdverseEventActualityEnum] of String = ('', 'actual', 'potential');
  SYSTEMS_TFhirAdverseEventActualityEnum : Array[TFhirAdverseEventActualityEnum] of String = (FHIR_URI_NONE, FHIR_URI_ADVERSE_EVENT_ACTUALITY, FHIR_URI_ADVERSE_EVENT_ACTUALITY);
  CODES_TFhirAdverseEventStatusEnum : Array[TFhirAdverseEventStatusEnum] of String = ('', 'in-progress', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirAdverseEventStatusEnum : Array[TFhirAdverseEventStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS);
  CODES_TFhirAggregationModeEnum : Array[TFhirAggregationModeEnum] of String = ('', 'contained', 'referenced', 'bundled');
  SYSTEMS_TFhirAggregationModeEnum : Array[TFhirAggregationModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESOURCE_AGGREGATION_MODE, FHIR_URI_RESOURCE_AGGREGATION_MODE, FHIR_URI_RESOURCE_AGGREGATION_MODE);
  CODES_TFhirAllResourceTypesEnum : Array[TFhirAllResourceTypesEnum] of String = ('', 'Account', 'ActivityDefinition', 'ActorDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'ArtifactAssessment', 'AuditEvent', 'Basic', 'Binary', 'BiologicallyDerivedProduct', 'BodyStructure', 'Bundle', 'CanonicalResource', 'CapabilityStatement', 'CarePlan', 'CareTeam', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseDefinition', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceDispense', 'DeviceMetric', 'DeviceRequest', 'DeviceUsage', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'DomainResource', 'Encounter', 'Endpoint',
       'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'EventDefinition', 'Evidence', 'EvidenceReport', 'EvidenceVariable', 'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'FormularyItem', 'GenomicStudy', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingSelection', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'InventoryReport', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MetadataResource', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome',
       'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition', 'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Permission', 'Person', 'PlanDefinition', 'Practitioner', 'PractitionerRole', 'Procedure', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestOrchestration', 'Requirements', 'ResearchStudy', 'ResearchSubject', 'Resource', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'Transport', 'ValueSet', 'VerificationResult', 'VisionPrescription');
  SYSTEMS_TFhirAllResourceTypesEnum : Array[TFhirAllResourceTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES);
  CODES_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = ('', 'food', 'medication', 'environment', 'biologic');
  SYSTEMS_TFhirAllergyIntoleranceCategoryEnum : Array[TFhirAllergyIntoleranceCategoryEnum] of String = (FHIR_URI_NONE, FHIR_URI_ALLERGY_INTOLERANCE_CATEGORY, FHIR_URI_ALLERGY_INTOLERANCE_CATEGORY, FHIR_URI_ALLERGY_INTOLERANCE_CATEGORY, FHIR_URI_ALLERGY_INTOLERANCE_CATEGORY);
  CODES_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = ('', 'low', 'high', 'unable-to-assess');
  SYSTEMS_TFhirAllergyIntoleranceCriticalityEnum : Array[TFhirAllergyIntoleranceCriticalityEnum] of String = (FHIR_URI_NONE, FHIR_URI_ALLERGY_INTOLERANCE_CRITICALITY, FHIR_URI_ALLERGY_INTOLERANCE_CRITICALITY, FHIR_URI_ALLERGY_INTOLERANCE_CRITICALITY);
  CODES_TFhirAllergyIntoleranceSeverityEnum : Array[TFhirAllergyIntoleranceSeverityEnum] of String = ('', 'mild', 'moderate', 'severe');
  SYSTEMS_TFhirAllergyIntoleranceSeverityEnum : Array[TFhirAllergyIntoleranceSeverityEnum] of String = (FHIR_URI_NONE, FHIR_URI_REACTION_EVENT_SEVERITY, FHIR_URI_REACTION_EVENT_SEVERITY, FHIR_URI_REACTION_EVENT_SEVERITY);
  CODES_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = ('', 'allergy', 'intolerance');
  SYSTEMS_TFhirAllergyIntoleranceTypeEnum : Array[TFhirAllergyIntoleranceTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ALLERGY_INTOLERANCE_TYPE, FHIR_URI_ALLERGY_INTOLERANCE_TYPE);
  CODES_TFhirAppointmentStatusEnum : Array[TFhirAppointmentStatusEnum] of String = ('', 'proposed', 'pending', 'booked', 'arrived', 'fulfilled', 'cancelled', 'noshow', 'entered-in-error', 'checked-in', 'waitlist');
  SYSTEMS_TFhirAppointmentStatusEnum : Array[TFhirAppointmentStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS, FHIR_URI_APPOINTMENTSTATUS);
  CODES_TFhirArtifactAssessmentDispositionEnum : Array[TFhirArtifactAssessmentDispositionEnum] of String = ('', 'unresolved', 'not-persuasive', 'persuasive', 'persuasive-with-modification', 'not-persuasive-with-modification');
  SYSTEMS_TFhirArtifactAssessmentDispositionEnum : Array[TFhirArtifactAssessmentDispositionEnum] of String = (FHIR_URI_NONE, FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION, FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION, FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION, FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION, FHIR_URI_ARTIFACTASSESSMENT_DISPOSITION);
  CODES_TFhirArtifactAssessmentInformationTypeEnum : Array[TFhirArtifactAssessmentInformationTypeEnum] of String = ('', 'comment', 'classifier', 'rating', 'container', 'response', 'change-request');
  SYSTEMS_TFhirArtifactAssessmentInformationTypeEnum : Array[TFhirArtifactAssessmentInformationTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE, FHIR_URI_ARTIFACTASSESSMENT_INFORMATION_TYPE);
  CODES_TFhirArtifactAssessmentWorkflowStatusEnum : Array[TFhirArtifactAssessmentWorkflowStatusEnum] of String = ('', 'submitted', 'triaged', 'waiting-for-input', 'resolved-no-change', 'resolved-change-required', 'deferred', 'duplicate', 'applied', 'published');
  SYSTEMS_TFhirArtifactAssessmentWorkflowStatusEnum : Array[TFhirArtifactAssessmentWorkflowStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS, FHIR_URI_ARTIFACTASSESSMENT_WORKFLOW_STATUS);
  CODES_TFhirAssertionDirectionTypeEnum : Array[TFhirAssertionDirectionTypeEnum] of String = ('', 'response', 'request');
  SYSTEMS_TFhirAssertionDirectionTypeEnum : Array[TFhirAssertionDirectionTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ASSERT_DIRECTION_CODES, FHIR_URI_ASSERT_DIRECTION_CODES);
  CODES_TFhirAssertionOperatorTypeEnum : Array[TFhirAssertionOperatorTypeEnum] of String = ('', 'equals', 'notEquals', 'in', 'notIn', 'greaterThan', 'lessThan', 'empty', 'notEmpty', 'contains', 'notContains', 'eval');
  SYSTEMS_TFhirAssertionOperatorTypeEnum : Array[TFhirAssertionOperatorTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES, FHIR_URI_ASSERT_OPERATOR_CODES);
  CODES_TFhirAssertionResponseTypesEnum : Array[TFhirAssertionResponseTypesEnum] of String = ('', 'okay', 'created', 'noContent', 'notModified', 'bad', 'forbidden', 'notFound', 'methodNotAllowed', 'conflict', 'gone', 'preconditionFailed', 'unprocessable');
  SYSTEMS_TFhirAssertionResponseTypesEnum : Array[TFhirAssertionResponseTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES, FHIR_URI_ASSERT_RESPONSE_CODE_TYPES);
  CODES_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = ('', 'C', 'R', 'U', 'D', 'E');
  SYSTEMS_TFhirAuditEventActionEnum : Array[TFhirAuditEventActionEnum] of String = (FHIR_URI_NONE, FHIR_URI_AUDIT_EVENT_ACTION, FHIR_URI_AUDIT_EVENT_ACTION, FHIR_URI_AUDIT_EVENT_ACTION, FHIR_URI_AUDIT_EVENT_ACTION, FHIR_URI_AUDIT_EVENT_ACTION);
  CODES_TFhirAuditEventSeverityEnum : Array[TFhirAuditEventSeverityEnum] of String = ('', 'emergency', 'alert', 'critical', 'error', 'warning', 'notice', 'informational', 'debug');
  SYSTEMS_TFhirAuditEventSeverityEnum : Array[TFhirAuditEventSeverityEnum] of String = (FHIR_URI_NONE, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY, FHIR_URI_AUDIT_EVENT_SEVERITY);
  CODES_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = ('', 'required', 'extensible', 'preferred', 'example');
  SYSTEMS_TFhirBindingStrengthEnum : Array[TFhirBindingStrengthEnum] of String = (FHIR_URI_NONE, FHIR_URI_BINDING_STRENGTH, FHIR_URI_BINDING_STRENGTH, FHIR_URI_BINDING_STRENGTH, FHIR_URI_BINDING_STRENGTH);
  CODES_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = ('', 'document', 'message', 'transaction', 'transaction-response', 'batch', 'batch-response', 'history', 'searchset', 'collection', 'subscription-notification');
  SYSTEMS_TFhirBundleTypeEnum : Array[TFhirBundleTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE, FHIR_URI_BUNDLE_TYPE);
  CODES_TFhirCapabilityStatementKindEnum : Array[TFhirCapabilityStatementKindEnum] of String = ('', 'instance', 'capability', 'requirements');
  SYSTEMS_TFhirCapabilityStatementKindEnum : Array[TFhirCapabilityStatementKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_CAPABILITY_STATEMENT_KIND, FHIR_URI_CAPABILITY_STATEMENT_KIND, FHIR_URI_CAPABILITY_STATEMENT_KIND);
  CODES_TFhirCarePlanActivityKindEnum : Array[TFhirCarePlanActivityKindEnum] of String = ('', 'Appointment', 'CommunicationRequest', 'DeviceRequest', 'MedicationRequest', 'NutritionOrder', 'Task', 'ServiceRequest', 'VisionPrescription');
  SYSTEMS_TFhirCarePlanActivityKindEnum : Array[TFhirCarePlanActivityKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES);
  CODES_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = ('', 'not-started', 'scheduled', 'in-progress', 'on-hold', 'completed', 'cancelled', 'stopped', 'unknown', 'entered-in-error');
  SYSTEMS_TFhirCarePlanActivityStatusEnum : Array[TFhirCarePlanActivityStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS, FHIR_URI_CARE_PLAN_ACTIVITY_STATUS);
  CODES_TFhirCarePlanIntentEnum : Array[TFhirCarePlanIntentEnum] of String = ('', 'proposal', 'plan', 'order', 'option', 'directive');
  SYSTEMS_TFhirCarePlanIntentEnum : Array[TFhirCarePlanIntentEnum] of String = (FHIR_URI_NONE, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT);
  CODES_TFhirCareTeamStatusEnum : Array[TFhirCareTeamStatusEnum] of String = ('', 'proposed', 'active', 'suspended', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirCareTeamStatusEnum : Array[TFhirCareTeamStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_CARE_TEAM_STATUS, FHIR_URI_CARE_TEAM_STATUS, FHIR_URI_CARE_TEAM_STATUS, FHIR_URI_CARE_TEAM_STATUS, FHIR_URI_CARE_TEAM_STATUS);
  CODES_TFhirCharacteristicCombinationEnum : Array[TFhirCharacteristicCombinationEnum] of String = ('', 'all-of', 'any-of', 'at-least', 'at-most', 'statistical', 'net-effect', 'dataset');
  SYSTEMS_TFhirCharacteristicCombinationEnum : Array[TFhirCharacteristicCombinationEnum] of String = (FHIR_URI_NONE, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION, FHIR_URI_CHARACTERISTIC_COMBINATION);
  CODES_TFhirChargeItemStatusEnum : Array[TFhirChargeItemStatusEnum] of String = ('', 'planned', 'billable', 'not-billable', 'aborted', 'billed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirChargeItemStatusEnum : Array[TFhirChargeItemStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS, FHIR_URI_CHARGEITEM_STATUS);
  CODES_TFhirClaimProcessingCodesEnum : Array[TFhirClaimProcessingCodesEnum] of String = ('', 'queued', 'complete', 'error', 'partial');
  SYSTEMS_TFhirClaimProcessingCodesEnum : Array[TFhirClaimProcessingCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_CLAIM_OUTCOME, FHIR_URI_CLAIM_OUTCOME, FHIR_URI_CLAIM_OUTCOME, FHIR_URI_CLAIM_OUTCOME);
  CODES_TFhirClinicalUseDefinitionTypeEnum : Array[TFhirClinicalUseDefinitionTypeEnum] of String = ('', 'indication', 'contraindication', 'interaction', 'undesirable-effect', 'warning');
  SYSTEMS_TFhirClinicalUseDefinitionTypeEnum : Array[TFhirClinicalUseDefinitionTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CLINICAL_USE_DEFINITION_TYPE, FHIR_URI_CLINICAL_USE_DEFINITION_TYPE, FHIR_URI_CLINICAL_USE_DEFINITION_TYPE, FHIR_URI_CLINICAL_USE_DEFINITION_TYPE, FHIR_URI_CLINICAL_USE_DEFINITION_TYPE);
  CODES_TFhirCodeSearchSupportEnum : Array[TFhirCodeSearchSupportEnum] of String = ('', 'in-compose', 'in-expansion', 'in-compose-or-expansion');
  SYSTEMS_TFhirCodeSearchSupportEnum : Array[TFhirCodeSearchSupportEnum] of String = (FHIR_URI_NONE, FHIR_URI_CODE_SEARCH_SUPPORT, FHIR_URI_CODE_SEARCH_SUPPORT, FHIR_URI_CODE_SEARCH_SUPPORT);
  CODES_TFhirCodeSystemContentModeEnum : Array[TFhirCodeSystemContentModeEnum] of String = ('', 'not-present', 'example', 'fragment', 'complete', 'supplement');
  SYSTEMS_TFhirCodeSystemContentModeEnum : Array[TFhirCodeSystemContentModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CODESYSTEM_CONTENT_MODE, FHIR_URI_CODESYSTEM_CONTENT_MODE, FHIR_URI_CODESYSTEM_CONTENT_MODE, FHIR_URI_CODESYSTEM_CONTENT_MODE, FHIR_URI_CODESYSTEM_CONTENT_MODE);
  CODES_TFhirCodeSystemHierarchyMeaningEnum : Array[TFhirCodeSystemHierarchyMeaningEnum] of String = ('', 'grouped-by', 'is-a', 'part-of', 'classified-with');
  SYSTEMS_TFhirCodeSystemHierarchyMeaningEnum : Array[TFhirCodeSystemHierarchyMeaningEnum] of String = (FHIR_URI_NONE, FHIR_URI_CODESYSTEM_HIERARCHY_MEANING, FHIR_URI_CODESYSTEM_HIERARCHY_MEANING, FHIR_URI_CODESYSTEM_HIERARCHY_MEANING, FHIR_URI_CODESYSTEM_HIERARCHY_MEANING);
  CODES_TFhirCommonLanguagesEnum : Array[TFhirCommonLanguagesEnum] of String = ('', 'ar', 'bn', 'cs', 'da', 'de', 'de-AT', 'de-CH', 'de-DE', 'el', 'en', 'en-AU', 'en-CA', 'en-GB', 'en-IN', 'en-NZ', 'en-SG', 'en-US', 'es', 'es-AR', 'es-ES', 'es-UY', 'fi', 'fr', 'fr-BE', 'fr-CH', 'fr-FR', 'fr-CA', 'fy', 'fy-NL', 'hi', 'hr', 'it', 'it-CH', 'it-IT', 'ja', 'ko', 'nl', 'nl-BE', 'nl-NL', 'no', 'no-NO', 'pa', 'pl', 'pt', 'pt-BR', 'ru', 'ru-RU', 'sr', 'sr-RS', 'sv', 'sv-SE', 'te', 'zh', 'zh-CN', 'zh-HK', 'zh-SG', 'zh-TW');
  SYSTEMS_TFhirCommonLanguagesEnum : Array[TFhirCommonLanguagesEnum] of String = (FHIR_URI_NONE, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47, URI_BCP47);
  CODES_TFhirCompartmentTypeEnum : Array[TFhirCompartmentTypeEnum] of String = ('', 'Patient', 'Encounter', 'RelatedPerson', 'Practitioner', 'Device');
  SYSTEMS_TFhirCompartmentTypeEnum : Array[TFhirCompartmentTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_COMPARTMENT_TYPE, FHIR_URI_COMPARTMENT_TYPE, FHIR_URI_COMPARTMENT_TYPE, FHIR_URI_COMPARTMENT_TYPE, FHIR_URI_COMPARTMENT_TYPE);
  CODES_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = ('', 'registered', 'partial', 'preliminary', 'final', 'amended', 'corrected', 'appended', 'cancelled', 'entered-in-error', 'deprecated', 'unknown');
  SYSTEMS_TFhirCompositionStatusEnum : Array[TFhirCompositionStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS, FHIR_URI_COMPOSITION_STATUS);
  CODES_TFhirConceptMapGroupUnmappedModeEnum : Array[TFhirConceptMapGroupUnmappedModeEnum] of String = ('', 'use-source-code', 'fixed', 'other-map');
  SYSTEMS_TFhirConceptMapGroupUnmappedModeEnum : Array[TFhirConceptMapGroupUnmappedModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONCEPTMAP_UNMAPPED_MODE, FHIR_URI_CONCEPTMAP_UNMAPPED_MODE, FHIR_URI_CONCEPTMAP_UNMAPPED_MODE);
  CODES_TFhirConceptMapRelationshipEnum : Array[TFhirConceptMapRelationshipEnum] of String = ('', 'related-to', 'equivalent', 'source-is-narrower-than-target', 'source-is-broader-than-target', 'not-related-to');
  SYSTEMS_TFhirConceptMapRelationshipEnum : Array[TFhirConceptMapRelationshipEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONCEPT_MAP_RELATIONSHIP, FHIR_URI_CONCEPT_MAP_RELATIONSHIP, FHIR_URI_CONCEPT_MAP_RELATIONSHIP, FHIR_URI_CONCEPT_MAP_RELATIONSHIP, FHIR_URI_CONCEPT_MAP_RELATIONSHIP);
  CODES_TFhirConceptPropertyTypeEnum : Array[TFhirConceptPropertyTypeEnum] of String = ('', 'code', 'Coding', 'string', 'integer', 'boolean', 'dateTime', 'decimal');
  SYSTEMS_TFhirConceptPropertyTypeEnum : Array[TFhirConceptPropertyTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE, FHIR_URI_CONCEPT_PROPERTY_TYPE);
  CODES_TFhirConditionPreconditionTypeEnum : Array[TFhirConditionPreconditionTypeEnum] of String = ('', 'sensitive', 'specific');
  SYSTEMS_TFhirConditionPreconditionTypeEnum : Array[TFhirConditionPreconditionTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONDITION_PRECONDITION_TYPE, FHIR_URI_CONDITION_PRECONDITION_TYPE);
  CODES_TFhirConditionQuestionnairePurposeEnum : Array[TFhirConditionQuestionnairePurposeEnum] of String = ('', 'preadmit', 'diff-diagnosis', 'outcome');
  SYSTEMS_TFhirConditionQuestionnairePurposeEnum : Array[TFhirConditionQuestionnairePurposeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONDITION_QUESTIONNAIRE_PURPOSE, FHIR_URI_CONDITION_QUESTIONNAIRE_PURPOSE, FHIR_URI_CONDITION_QUESTIONNAIRE_PURPOSE);
  CODES_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = ('', 'not-supported', 'single', 'multiple');
  SYSTEMS_TFhirConditionalDeleteStatusEnum : Array[TFhirConditionalDeleteStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONDITIONAL_DELETE_STATUS, FHIR_URI_CONDITIONAL_DELETE_STATUS, FHIR_URI_CONDITIONAL_DELETE_STATUS);
  CODES_TFhirConditionalReadStatusEnum : Array[TFhirConditionalReadStatusEnum] of String = ('', 'not-supported', 'modified-since', 'not-match', 'full-support');
  SYSTEMS_TFhirConditionalReadStatusEnum : Array[TFhirConditionalReadStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONDITIONAL_READ_STATUS, FHIR_URI_CONDITIONAL_READ_STATUS, FHIR_URI_CONDITIONAL_READ_STATUS, FHIR_URI_CONDITIONAL_READ_STATUS);
  CODES_TFhirConformanceExpectationEnum : Array[TFhirConformanceExpectationEnum] of String = ('', 'SHALL', 'SHOULD', 'MAY', 'SHOULD-NOT');
  SYSTEMS_TFhirConformanceExpectationEnum : Array[TFhirConformanceExpectationEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONFORMANCE_EXPECTATION, FHIR_URI_CONFORMANCE_EXPECTATION, FHIR_URI_CONFORMANCE_EXPECTATION, FHIR_URI_CONFORMANCE_EXPECTATION);
  CODES_TFhirConsentDataMeaningEnum : Array[TFhirConsentDataMeaningEnum] of String = ('', 'instance', 'related', 'dependents', 'authoredby');
  SYSTEMS_TFhirConsentDataMeaningEnum : Array[TFhirConsentDataMeaningEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONSENT_DATA_MEANING, FHIR_URI_CONSENT_DATA_MEANING, FHIR_URI_CONSENT_DATA_MEANING, FHIR_URI_CONSENT_DATA_MEANING);
  CODES_TFhirConsentProvisionTypeEnum : Array[TFhirConsentProvisionTypeEnum] of String = ('', 'deny', 'permit');
  SYSTEMS_TFhirConsentProvisionTypeEnum : Array[TFhirConsentProvisionTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONSENT_PROVISION_TYPE, FHIR_URI_CONSENT_PROVISION_TYPE);
  CODES_TFhirConsentStateEnum : Array[TFhirConsentStateEnum] of String = ('', 'draft', 'active', 'inactive', 'not-done', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirConsentStateEnum : Array[TFhirConsentStateEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONSENT_STATE_CODES, FHIR_URI_CONSENT_STATE_CODES, FHIR_URI_CONSENT_STATE_CODES, FHIR_URI_CONSENT_STATE_CODES, FHIR_URI_CONSENT_STATE_CODES, FHIR_URI_CONSENT_STATE_CODES);
  CODES_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = ('', 'error', 'warning');
  SYSTEMS_TFhirConstraintSeverityEnum : Array[TFhirConstraintSeverityEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONSTRAINT_SEVERITY, FHIR_URI_CONSTRAINT_SEVERITY);
  CODES_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = ('', 'phone', 'fax', 'email', 'pager', 'url', 'sms', 'other');
  SYSTEMS_TFhirContactPointSystemEnum : Array[TFhirContactPointSystemEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM, FHIR_URI_CONTACT_POINT_SYSTEM);
  CODES_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = ('', 'home', 'work', 'temp', 'old', 'mobile');
  SYSTEMS_TFhirContactPointUseEnum : Array[TFhirContactPointUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONTACT_POINT_USE, FHIR_URI_CONTACT_POINT_USE, FHIR_URI_CONTACT_POINT_USE, FHIR_URI_CONTACT_POINT_USE, FHIR_URI_CONTACT_POINT_USE);
  CODES_TFhirContractResourcePublicationStatusCodesEnum : Array[TFhirContractResourcePublicationStatusCodesEnum] of String = ('', 'amended', 'appended', 'cancelled', 'disputed', 'entered-in-error', 'executable', 'executed', 'negotiable', 'offered', 'policy', 'rejected', 'renewed', 'revoked', 'resolved', 'terminated');
  SYSTEMS_TFhirContractResourcePublicationStatusCodesEnum : Array[TFhirContractResourcePublicationStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS, FHIR_URI_CONTRACT_PUBLICATIONSTATUS);
  CODES_TFhirContractResourceStatusCodesEnum : Array[TFhirContractResourceStatusCodesEnum] of String = ('', 'amended', 'appended', 'cancelled', 'disputed', 'entered-in-error', 'executable', 'executed', 'negotiable', 'offered', 'policy', 'rejected', 'renewed', 'revoked', 'resolved', 'terminated');
  SYSTEMS_TFhirContractResourceStatusCodesEnum : Array[TFhirContractResourceStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS, FHIR_URI_CONTRACT_STATUS);
  CODES_TFhirContributorTypeEnum : Array[TFhirContributorTypeEnum] of String = ('', 'author', 'editor', 'reviewer', 'endorser');
  SYSTEMS_TFhirContributorTypeEnum : Array[TFhirContributorTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_CONTRIBUTOR_TYPE, FHIR_URI_CONTRIBUTOR_TYPE, FHIR_URI_CONTRIBUTOR_TYPE, FHIR_URI_CONTRIBUTOR_TYPE);
  CODES_TFhirCriteriaNotExistsBehaviorEnum : Array[TFhirCriteriaNotExistsBehaviorEnum] of String = ('', 'test-passes', 'test-fails');
  SYSTEMS_TFhirCriteriaNotExistsBehaviorEnum : Array[TFhirCriteriaNotExistsBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSCRIPTIONTOPIC_CR_BEHAVIOR, FHIR_URI_SUBSCRIPTIONTOPIC_CR_BEHAVIOR);
  CODES_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = ('', 'mon', 'tue', 'wed', 'thu', 'fri', 'sat', 'sun');
  SYSTEMS_TFhirDaysOfWeekEnum : Array[TFhirDaysOfWeekEnum] of String = (FHIR_URI_NONE, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK, FHIR_URI_DAYS_OF_WEEK);
  CODES_TFhirDetectedIssueSeverityEnum : Array[TFhirDetectedIssueSeverityEnum] of String = ('', 'high', 'moderate', 'low');
  SYSTEMS_TFhirDetectedIssueSeverityEnum : Array[TFhirDetectedIssueSeverityEnum] of String = (FHIR_URI_NONE, FHIR_URI_DETECTEDISSUE_SEVERITY, FHIR_URI_DETECTEDISSUE_SEVERITY, FHIR_URI_DETECTEDISSUE_SEVERITY);
  CODES_TFhirDetectedIssueStatusEnum : Array[TFhirDetectedIssueStatusEnum] of String = ('', 'preliminary', 'final', 'entered-in-error', 'mitigated');
  SYSTEMS_TFhirDetectedIssueStatusEnum : Array[TFhirDetectedIssueStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_DETECTEDISSUE_STATUS);
  CODES_TFhirDeviceCorrectiveActionScopeEnum : Array[TFhirDeviceCorrectiveActionScopeEnum] of String = ('', 'model', 'lot-numbers', 'serial-numbers');
  SYSTEMS_TFhirDeviceCorrectiveActionScopeEnum : Array[TFhirDeviceCorrectiveActionScopeEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICE_CORRECTIVEACTIONSCOPE, FHIR_URI_DEVICE_CORRECTIVEACTIONSCOPE, FHIR_URI_DEVICE_CORRECTIVEACTIONSCOPE);
  CODES_TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum : Array[TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum] of String = ('', 'basic', 'master', 'license');
  SYSTEMS_TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum : Array[TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICEDEFINITION_REGULATORY_IDENTIFIER_TYPE, FHIR_URI_DEVICEDEFINITION_REGULATORY_IDENTIFIER_TYPE, FHIR_URI_DEVICEDEFINITION_REGULATORY_IDENTIFIER_TYPE);
  CODES_TFhirDeviceDispenseStatusCodesEnum : Array[TFhirDeviceDispenseStatusCodesEnum] of String = ('', 'preparation', 'in-progress', 'cancelled', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'declined', 'unknown');
  SYSTEMS_TFhirDeviceDispenseStatusCodesEnum : Array[TFhirDeviceDispenseStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS, FHIR_URI_DEVICEDISPENSE_STATUS);
  CODES_TFhirDeviceMetricCalibrationStateEnum : Array[TFhirDeviceMetricCalibrationStateEnum] of String = ('', 'not-calibrated', 'calibration-required', 'calibrated', 'unspecified');
  SYSTEMS_TFhirDeviceMetricCalibrationStateEnum : Array[TFhirDeviceMetricCalibrationStateEnum] of String = (FHIR_URI_NONE, FHIR_URI_METRIC_CALIBRATION_STATE, FHIR_URI_METRIC_CALIBRATION_STATE, FHIR_URI_METRIC_CALIBRATION_STATE, FHIR_URI_METRIC_CALIBRATION_STATE);
  CODES_TFhirDeviceMetricCalibrationTypeEnum : Array[TFhirDeviceMetricCalibrationTypeEnum] of String = ('', 'unspecified', 'offset', 'gain', 'two-point');
  SYSTEMS_TFhirDeviceMetricCalibrationTypeEnum : Array[TFhirDeviceMetricCalibrationTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_METRIC_CALIBRATION_TYPE, FHIR_URI_METRIC_CALIBRATION_TYPE, FHIR_URI_METRIC_CALIBRATION_TYPE, FHIR_URI_METRIC_CALIBRATION_TYPE);
  CODES_TFhirDeviceMetricCategoryEnum : Array[TFhirDeviceMetricCategoryEnum] of String = ('', 'measurement', 'setting', 'calculation', 'unspecified');
  SYSTEMS_TFhirDeviceMetricCategoryEnum : Array[TFhirDeviceMetricCategoryEnum] of String = (FHIR_URI_NONE, FHIR_URI_METRIC_CATEGORY, FHIR_URI_METRIC_CATEGORY, FHIR_URI_METRIC_CATEGORY, FHIR_URI_METRIC_CATEGORY);
  CODES_TFhirDeviceMetricColorEnum : Array[TFhirDeviceMetricColorEnum] of String = ('', 'black', 'red', 'green', 'yellow', 'blue', 'magenta', 'cyan', 'white');
  SYSTEMS_TFhirDeviceMetricColorEnum : Array[TFhirDeviceMetricColorEnum] of String = (FHIR_URI_NONE, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR, FHIR_URI_METRIC_COLOR);
  CODES_TFhirDeviceMetricOperationalStatusEnum : Array[TFhirDeviceMetricOperationalStatusEnum] of String = ('', 'on', 'off', 'standby', 'entered-in-error');
  SYSTEMS_TFhirDeviceMetricOperationalStatusEnum : Array[TFhirDeviceMetricOperationalStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_METRIC_OPERATIONAL_STATUS, FHIR_URI_METRIC_OPERATIONAL_STATUS, FHIR_URI_METRIC_OPERATIONAL_STATUS, FHIR_URI_METRIC_OPERATIONAL_STATUS);
  CODES_TFhirDeviceNameTypeEnum : Array[TFhirDeviceNameTypeEnum] of String = ('', 'registered-name', 'user-friendly-name', 'patient-reported-name');
  SYSTEMS_TFhirDeviceNameTypeEnum : Array[TFhirDeviceNameTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICE_NAMETYPE, FHIR_URI_DEVICE_NAMETYPE, FHIR_URI_DEVICE_NAMETYPE);
  CODES_TFhirDeviceProductionIdentifierInUDIEnum : Array[TFhirDeviceProductionIdentifierInUDIEnum] of String = ('', 'lot-number', 'manufactured-date', 'serial-number', 'expiration-date', 'biological-source', 'software-version');
  SYSTEMS_TFhirDeviceProductionIdentifierInUDIEnum : Array[TFhirDeviceProductionIdentifierInUDIEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI, FHIR_URI_DEVICE_PRODUCTIDENTIFIERINUDI);
  CODES_TFhirDeviceUsageStatusEnum : Array[TFhirDeviceUsageStatusEnum] of String = ('', 'active', 'completed', 'not-done', 'entered-in-error', 'intended', 'stopped', 'on-hold');
  SYSTEMS_TFhirDeviceUsageStatusEnum : Array[TFhirDeviceUsageStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS, FHIR_URI_DEVICEUSAGE_STATUS);
  CODES_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = ('', 'registered', 'partial', 'preliminary', 'final', 'amended', 'corrected', 'appended', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirDiagnosticReportStatusEnum : Array[TFhirDiagnosticReportStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS, FHIR_URI_DIAGNOSTIC_REPORT_STATUS);
  CODES_TFhirDiscriminatorTypeEnum : Array[TFhirDiscriminatorTypeEnum] of String = ('', 'value', 'exists', 'pattern', 'type', 'profile', 'position');
  SYSTEMS_TFhirDiscriminatorTypeEnum : Array[TFhirDiscriminatorTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_DISCRIMINATOR_TYPE, FHIR_URI_DISCRIMINATOR_TYPE, FHIR_URI_DISCRIMINATOR_TYPE, FHIR_URI_DISCRIMINATOR_TYPE, FHIR_URI_DISCRIMINATOR_TYPE, FHIR_URI_DISCRIMINATOR_TYPE);
  CODES_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = ('', 'producer', 'consumer');
  SYSTEMS_TFhirDocumentModeEnum : Array[TFhirDocumentModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_DOCUMENT_MODE, FHIR_URI_DOCUMENT_MODE);
  CODES_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = ('', 'current', 'superseded', 'entered-in-error');
  SYSTEMS_TFhirDocumentReferenceStatusEnum : Array[TFhirDocumentReferenceStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_DOCUMENT_REFERENCE_STATUS, FHIR_URI_DOCUMENT_REFERENCE_STATUS, FHIR_URI_DOCUMENT_REFERENCE_STATUS);
  CODES_TFhirEligibilityOutcomeEnum : Array[TFhirEligibilityOutcomeEnum] of String = ('', 'queued', 'complete', 'error', 'partial');
  SYSTEMS_TFhirEligibilityOutcomeEnum : Array[TFhirEligibilityOutcomeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ELIGIBILITY_OUTCOME, FHIR_URI_ELIGIBILITY_OUTCOME, FHIR_URI_ELIGIBILITY_OUTCOME, FHIR_URI_ELIGIBILITY_OUTCOME);
  CODES_TFhirEligibilityRequestPurposeEnum : Array[TFhirEligibilityRequestPurposeEnum] of String = ('', 'auth-requirements', 'benefits', 'discovery', 'validation');
  SYSTEMS_TFhirEligibilityRequestPurposeEnum : Array[TFhirEligibilityRequestPurposeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ELIGIBILITYREQUEST_PURPOSE, FHIR_URI_ELIGIBILITYREQUEST_PURPOSE, FHIR_URI_ELIGIBILITYREQUEST_PURPOSE, FHIR_URI_ELIGIBILITYREQUEST_PURPOSE);
  CODES_TFhirEligibilityResponsePurposeEnum : Array[TFhirEligibilityResponsePurposeEnum] of String = ('', 'auth-requirements', 'benefits', 'discovery', 'validation');
  SYSTEMS_TFhirEligibilityResponsePurposeEnum : Array[TFhirEligibilityResponsePurposeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ELIGIBILITYRESPONSE_PURPOSE, FHIR_URI_ELIGIBILITYRESPONSE_PURPOSE, FHIR_URI_ELIGIBILITYRESPONSE_PURPOSE, FHIR_URI_ELIGIBILITYRESPONSE_PURPOSE);
  CODES_TFhirEnableWhenBehaviorEnum : Array[TFhirEnableWhenBehaviorEnum] of String = ('', 'all', 'any');
  SYSTEMS_TFhirEnableWhenBehaviorEnum : Array[TFhirEnableWhenBehaviorEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUESTIONNAIRE_ENABLE_BEHAVIOR, FHIR_URI_QUESTIONNAIRE_ENABLE_BEHAVIOR);
  CODES_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = ('', 'planned', 'active', 'reserved', 'completed');
  SYSTEMS_TFhirEncounterLocationStatusEnum : Array[TFhirEncounterLocationStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_ENCOUNTER_LOCATION_STATUS, FHIR_URI_ENCOUNTER_LOCATION_STATUS, FHIR_URI_ENCOUNTER_LOCATION_STATUS, FHIR_URI_ENCOUNTER_LOCATION_STATUS);
  CODES_TFhirEncounterStatusEnum : Array[TFhirEncounterStatusEnum] of String = ('', 'planned', 'in-progress', 'onhold', 'discharged', 'completed', 'cancelled', 'discontinued', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirEncounterStatusEnum : Array[TFhirEncounterStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS, FHIR_URI_ENCOUNTER_STATUS);
  CODES_TFhirEndpointStatusEnum : Array[TFhirEndpointStatusEnum] of String = ('', 'active', 'suspended', 'error', 'off', 'entered-in-error');
  SYSTEMS_TFhirEndpointStatusEnum : Array[TFhirEndpointStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_ENDPOINT_STATUS, FHIR_URI_ENDPOINT_STATUS, FHIR_URI_ENDPOINT_STATUS, FHIR_URI_ENDPOINT_STATUS, FHIR_URI_ENDPOINT_STATUS);
  CODES_TFhirEnrollmentOutcomeEnum : Array[TFhirEnrollmentOutcomeEnum] of String = ('', 'queued', 'complete', 'error', 'partial');
  SYSTEMS_TFhirEnrollmentOutcomeEnum : Array[TFhirEnrollmentOutcomeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ENROLLMENT_OUTCOME, FHIR_URI_ENROLLMENT_OUTCOME, FHIR_URI_ENROLLMENT_OUTCOME, FHIR_URI_ENROLLMENT_OUTCOME);
  CODES_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = ('', 'planned', 'waitlist', 'active', 'onhold', 'finished', 'cancelled', 'entered-in-error');
  SYSTEMS_TFhirEpisodeOfCareStatusEnum : Array[TFhirEpisodeOfCareStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS, FHIR_URI_EPISODE_OF_CARE_STATUS);
  CODES_TFhirEventCapabilityModeEnum : Array[TFhirEventCapabilityModeEnum] of String = ('', 'sender', 'receiver');
  SYSTEMS_TFhirEventCapabilityModeEnum : Array[TFhirEventCapabilityModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_EVENT_CAPABILITY_MODE, FHIR_URI_EVENT_CAPABILITY_MODE);
  CODES_TFhirEventStatusEnum : Array[TFhirEventStatusEnum] of String = ('', 'preparation', 'in-progress', 'not-done', 'on-hold', 'stopped', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirEventStatusEnum : Array[TFhirEventStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS);
  CODES_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = ('', 'MORN', 'MORN.early', 'MORN.late', 'NOON', 'AFT', 'AFT.early', 'AFT.late', 'EVE', 'EVE.early', 'EVE.late', 'NIGHT', 'PHS', 'IMD', 'HS', 'WAKE', 'C', 'CM', 'CD', 'CV', 'AC', 'ACM', 'ACD', 'ACV', 'PC', 'PCM', 'PCD', 'PCV');
  SYSTEMS_TFhirEventTimingEnum : Array[TFhirEventTimingEnum] of String = (FHIR_URI_NONE, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_EVENT_TIMING, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT, FHIR_URI_V3_TIMINGEVENT);
  CODES_TFhirEvidenceVariableHandlingEnum : Array[TFhirEvidenceVariableHandlingEnum] of String = ('', 'continuous', 'dichotomous', 'ordinal', 'polychotomous');
  SYSTEMS_TFhirEvidenceVariableHandlingEnum : Array[TFhirEvidenceVariableHandlingEnum] of String = (FHIR_URI_NONE, FHIR_URI_VARIABLE_HANDLING, FHIR_URI_VARIABLE_HANDLING, FHIR_URI_VARIABLE_HANDLING, FHIR_URI_VARIABLE_HANDLING);
  CODES_TFhirExampleScenarioActorTypeEnum : Array[TFhirExampleScenarioActorTypeEnum] of String = ('', 'person', 'system');
  SYSTEMS_TFhirExampleScenarioActorTypeEnum : Array[TFhirExampleScenarioActorTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_EXAMPLESCENARIO_ACTOR_TYPE, FHIR_URI_EXAMPLESCENARIO_ACTOR_TYPE);
  CODES_TFhirExplanationOfBenefitStatusEnum : Array[TFhirExplanationOfBenefitStatusEnum] of String = ('', 'active', 'cancelled', 'draft', 'entered-in-error');
  SYSTEMS_TFhirExplanationOfBenefitStatusEnum : Array[TFhirExplanationOfBenefitStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_EXPLANATIONOFBENEFIT_STATUS, FHIR_URI_EXPLANATIONOFBENEFIT_STATUS, FHIR_URI_EXPLANATIONOFBENEFIT_STATUS, FHIR_URI_EXPLANATIONOFBENEFIT_STATUS);
  CODES_TFhirExtensionContextTypeEnum : Array[TFhirExtensionContextTypeEnum] of String = ('', 'fhirpath', 'element', 'extension');
  SYSTEMS_TFhirExtensionContextTypeEnum : Array[TFhirExtensionContextTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_EXTENSION_CONTEXT_TYPE, FHIR_URI_EXTENSION_CONTEXT_TYPE, FHIR_URI_EXTENSION_CONTEXT_TYPE);
  CODES_TFhirFHIRDeviceStatusEnum : Array[TFhirFHIRDeviceStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFHIRDeviceStatusEnum : Array[TFhirFHIRDeviceStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_DEVICE_STATUS, FHIR_URI_DEVICE_STATUS, FHIR_URI_DEVICE_STATUS);
  CODES_TFhirFHIRSubstanceStatusEnum : Array[TFhirFHIRSubstanceStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFHIRSubstanceStatusEnum : Array[TFhirFHIRSubstanceStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSTANCE_STATUS, FHIR_URI_SUBSTANCE_STATUS, FHIR_URI_SUBSTANCE_STATUS);
  CODES_TFhirFHIRTypesEnum : Array[TFhirFHIRTypesEnum] of String = ('', 'Base', 'Element', 'BackboneElement', 'DataType', 'Address', 'Annotation', 'Attachment', 'Availability', 'BackboneType', 'Dosage', 'ElementDefinition', 'MarketingStatus', 'Population', 'ProductShelfLife', 'Timing', 'CodeableConcept', 'CodeableReference', 'Coding', 'ContactDetail', 'ContactPoint', 'Contributor', 'DataRequirement', 'Expression', 'ExtendedContactDetail', 'Extension', 'HumanName', 'Identifier', 'Meta', 'MonetaryComponent', 'Money', 'Narrative', 'ParameterDefinition', 'Period', 'PrimitiveType', 'base64Binary', 'boolean', 'date', 'dateTime', 'decimal', 'instant', 'integer', 'positiveInt', 'unsignedInt', 'integer64', 'string', 'code', 'id', 'markdown', 'time', 'uri', 'canonical', 'oid', 'url', 'uuid', 'Quantity', 'Age', 'Count', 'Distance', 'Duration', 'Range', 'Ratio', 'RatioRange', 'Reference', 'RelatedArtifact', 'SampledData', 'Signature', 'TriggerDefinition', 'UsageContext', 'VirtualServiceDetail', 'xhtml', 'Resource',
       'Binary', 'Bundle', 'DomainResource', 'Account', 'ActivityDefinition', 'ActorDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'ArtifactAssessment', 'AuditEvent', 'Basic', 'BiologicallyDerivedProduct', 'BodyStructure', 'CanonicalResource', 'CapabilityStatement', 'CarePlan', 'CareTeam', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseDefinition', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceDispense', 'DeviceMetric', 'DeviceRequest', 'DeviceUsage', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'Encounter', 'Endpoint', 'EnrollmentRequest', 'EnrollmentResponse', 'EpisodeOfCare', 'EventDefinition',
       'Evidence', 'EvidenceReport', 'EvidenceVariable', 'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'FormularyItem', 'GenomicStudy', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingSelection', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'InventoryReport', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MetadataResource', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome', 'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition', 'Patient',
       'PaymentNotice', 'PaymentReconciliation', 'Permission', 'Person', 'PlanDefinition', 'Practitioner', 'PractitionerRole', 'Procedure', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestOrchestration', 'Requirements', 'ResearchStudy', 'ResearchSubject', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'Transport', 'ValueSet', 'VerificationResult', 'VisionPrescription', 'Parameters');
  SYSTEMS_TFhirFHIRTypesEnum : Array[TFhirFHIRTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES);
  CODES_TFhirFHIRVersionEnum : Array[TFhirFHIRVersionEnum] of String = ('', '0.01', '0.05', '0.06', '0.11', '0.0', '0.0.80', '0.0.81', '0.0.82', '0.4', '0.4.0', '0.5', '0.5.0', '1.0', '1.0.0', '1.0.1', '1.0.2', '1.1', '1.1.0', '1.4', '1.4.0', '1.6', '1.6.0', '1.8', '1.8.0', '3.0', '3.0.0', '3.0.1', '3.0.2', '3.3', '3.3.0', '3.5', '3.5.0', '4.0', '4.0.0', '4.0.1', '4.1', '4.1.0', '4.2', '4.2.0', '4.3', '4.3.0', '4.4', '4.4.0', '4.5', '4.5.0', '4.6', '4.6.0', '5.0', '5.0.0', '5.0.0-cibuild', '5.0.0-snapshot1', '5.0.0-snapshot2', '5.0.0-ballot');
  SYSTEMS_TFhirFHIRVersionEnum : Array[TFhirFHIRVersionEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION,
       FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION, FHIR_URI_FHIR_VERSION);
  CODES_TFhirFamilyHistoryStatusEnum : Array[TFhirFamilyHistoryStatusEnum] of String = ('', 'partial', 'completed', 'entered-in-error', 'health-unknown');
  SYSTEMS_TFhirFamilyHistoryStatusEnum : Array[TFhirFamilyHistoryStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_HISTORY_STATUS, FHIR_URI_HISTORY_STATUS, FHIR_URI_HISTORY_STATUS, FHIR_URI_HISTORY_STATUS);
  CODES_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = ('', '=', 'is-a', 'descendent-of', 'is-not-a', 'regex', 'in', 'not-in', 'generalizes', 'child-of', 'descendent-leaf', 'exists');
  SYSTEMS_TFhirFilterOperatorEnum : Array[TFhirFilterOperatorEnum] of String = (FHIR_URI_NONE, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR, FHIR_URI_FILTER_OPERATOR);
  CODES_TFhirFinancialResourceStatusCodesEnum : Array[TFhirFinancialResourceStatusCodesEnum] of String = ('', 'active', 'cancelled', 'draft', 'entered-in-error');
  SYSTEMS_TFhirFinancialResourceStatusCodesEnum : Array[TFhirFinancialResourceStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FM_STATUS, FHIR_URI_FM_STATUS, FHIR_URI_FM_STATUS, FHIR_URI_FM_STATUS);
  CODES_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirFlagStatusEnum : Array[TFhirFlagStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_FLAG_STATUS, FHIR_URI_FLAG_STATUS, FHIR_URI_FLAG_STATUS);
  CODES_TFhirFormularyItemStatusCodesEnum : Array[TFhirFormularyItemStatusCodesEnum] of String = ('', 'active', 'entered-in-error', 'inactive');
  SYSTEMS_TFhirFormularyItemStatusCodesEnum : Array[TFhirFormularyItemStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FORMULARYITEM_STATUS, FHIR_URI_FORMULARYITEM_STATUS, FHIR_URI_FORMULARYITEM_STATUS);
  CODES_TFhirGoalLifecycleStatusEnum : Array[TFhirGoalLifecycleStatusEnum] of String = ('', 'proposed', 'planned', 'accepted', 'active', 'on-hold', 'completed', 'cancelled', 'entered-in-error', 'rejected');
  SYSTEMS_TFhirGoalLifecycleStatusEnum : Array[TFhirGoalLifecycleStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS, FHIR_URI_GOAL_STATUS);
  CODES_TFhirGraphCompartmentRuleEnum : Array[TFhirGraphCompartmentRuleEnum] of String = ('', 'identical', 'matching', 'different', 'custom');
  SYSTEMS_TFhirGraphCompartmentRuleEnum : Array[TFhirGraphCompartmentRuleEnum] of String = (FHIR_URI_NONE, FHIR_URI_GRAPH_COMPARTMENT_RULE, FHIR_URI_GRAPH_COMPARTMENT_RULE, FHIR_URI_GRAPH_COMPARTMENT_RULE, FHIR_URI_GRAPH_COMPARTMENT_RULE);
  CODES_TFhirGraphCompartmentUseEnum : Array[TFhirGraphCompartmentUseEnum] of String = ('', 'condition', 'requirement');
  SYSTEMS_TFhirGraphCompartmentUseEnum : Array[TFhirGraphCompartmentUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_GRAPH_COMPARTMENT_USE, FHIR_URI_GRAPH_COMPARTMENT_USE);
  CODES_TFhirGroupMembershipBasisEnum : Array[TFhirGroupMembershipBasisEnum] of String = ('', 'definitional', 'enumerated');
  SYSTEMS_TFhirGroupMembershipBasisEnum : Array[TFhirGroupMembershipBasisEnum] of String = (FHIR_URI_NONE, FHIR_URI_GROUP_MEMBERSHIP_BASIS, FHIR_URI_GROUP_MEMBERSHIP_BASIS);
  CODES_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = ('', 'person', 'animal', 'practitioner', 'device', 'careteam', 'healthcareservice', 'location', 'organization', 'relatedperson', 'specimen');
  SYSTEMS_TFhirGroupTypeEnum : Array[TFhirGroupTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE, FHIR_URI_GROUP_TYPE);
  CODES_TFhirGuidanceResponseStatusEnum : Array[TFhirGuidanceResponseStatusEnum] of String = ('', 'success', 'data-requested', 'data-required', 'in-progress', 'failure', 'entered-in-error');
  SYSTEMS_TFhirGuidanceResponseStatusEnum : Array[TFhirGuidanceResponseStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_GUIDANCE_RESPONSE_STATUS, FHIR_URI_GUIDANCE_RESPONSE_STATUS, FHIR_URI_GUIDANCE_RESPONSE_STATUS, FHIR_URI_GUIDANCE_RESPONSE_STATUS, FHIR_URI_GUIDANCE_RESPONSE_STATUS, FHIR_URI_GUIDANCE_RESPONSE_STATUS);
  CODES_TFhirGuidePageGenerationEnum : Array[TFhirGuidePageGenerationEnum] of String = ('', 'html', 'markdown', 'xml', 'generated');
  SYSTEMS_TFhirGuidePageGenerationEnum : Array[TFhirGuidePageGenerationEnum] of String = (FHIR_URI_NONE, FHIR_URI_GUIDE_PAGE_GENERATION, FHIR_URI_GUIDE_PAGE_GENERATION, FHIR_URI_GUIDE_PAGE_GENERATION, FHIR_URI_GUIDE_PAGE_GENERATION);
  CODES_TFhirHTTPVerbEnum : Array[TFhirHTTPVerbEnum] of String = ('', 'GET', 'HEAD', 'POST', 'PUT', 'DELETE', 'PATCH');
  SYSTEMS_TFhirHTTPVerbEnum : Array[TFhirHTTPVerbEnum] of String = (FHIR_URI_NONE, FHIR_URI_HTTP_VERB, FHIR_URI_HTTP_VERB, FHIR_URI_HTTP_VERB, FHIR_URI_HTTP_VERB, FHIR_URI_HTTP_VERB, FHIR_URI_HTTP_VERB);
  CODES_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = ('', 'usual', 'official', 'temp', 'secondary', 'old');
  SYSTEMS_TFhirIdentifierUseEnum : Array[TFhirIdentifierUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_IDENTIFIER_USE, FHIR_URI_IDENTIFIER_USE, FHIR_URI_IDENTIFIER_USE, FHIR_URI_IDENTIFIER_USE, FHIR_URI_IDENTIFIER_USE);
  CODES_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = ('', 'level1', 'level2', 'level3', 'level4');
  SYSTEMS_TFhirIdentityAssuranceLevelEnum : Array[TFhirIdentityAssuranceLevelEnum] of String = (FHIR_URI_NONE, FHIR_URI_IDENTITY_ASSURANCELEVEL, FHIR_URI_IDENTITY_ASSURANCELEVEL, FHIR_URI_IDENTITY_ASSURANCELEVEL, FHIR_URI_IDENTITY_ASSURANCELEVEL);
  CODES_TFhirImagingSelection2DGraphicTypeEnum : Array[TFhirImagingSelection2DGraphicTypeEnum] of String = ('', 'point', 'polyline', 'interpolated', 'circle', 'ellipse');
  SYSTEMS_TFhirImagingSelection2DGraphicTypeEnum : Array[TFhirImagingSelection2DGraphicTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_2DGRAPHICTYPE);
  CODES_TFhirImagingSelection3DGraphicTypeEnum : Array[TFhirImagingSelection3DGraphicTypeEnum] of String = ('', 'point', 'multipoint', 'polyline', 'polygon', 'ellipse', 'ellipsoid');
  SYSTEMS_TFhirImagingSelection3DGraphicTypeEnum : Array[TFhirImagingSelection3DGraphicTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE, FHIR_URI_IMAGINGSELECTION_3DGRAPHICTYPE);
  CODES_TFhirImagingSelectionStatusEnum : Array[TFhirImagingSelectionStatusEnum] of String = ('', 'available', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirImagingSelectionStatusEnum : Array[TFhirImagingSelectionStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_IMAGINGSELECTION_STATUS, FHIR_URI_IMAGINGSELECTION_STATUS, FHIR_URI_IMAGINGSELECTION_STATUS);
  CODES_TFhirImagingStudyStatusEnum : Array[TFhirImagingStudyStatusEnum] of String = ('', 'registered', 'available', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirImagingStudyStatusEnum : Array[TFhirImagingStudyStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_IMAGINGSTUDY_STATUS, FHIR_URI_IMAGINGSTUDY_STATUS, FHIR_URI_IMAGINGSTUDY_STATUS, FHIR_URI_IMAGINGSTUDY_STATUS, FHIR_URI_IMAGINGSTUDY_STATUS);
  CODES_TFhirImmunizationEvaluationStatusCodesEnum : Array[TFhirImmunizationEvaluationStatusCodesEnum] of String = ('', 'completed', 'entered-in-error');
  SYSTEMS_TFhirImmunizationEvaluationStatusCodesEnum : Array[TFhirImmunizationEvaluationStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS);
  CODES_TFhirImmunizationStatusCodesEnum : Array[TFhirImmunizationStatusCodesEnum] of String = ('', 'completed', 'entered-in-error', 'not-done');
  SYSTEMS_TFhirImmunizationStatusCodesEnum : Array[TFhirImmunizationStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS, FHIR_URI_EVENT_STATUS);
  CODES_TFhirIngredientManufacturerRoleEnum : Array[TFhirIngredientManufacturerRoleEnum] of String = ('', 'allowed', 'possible', 'actual');
  SYSTEMS_TFhirIngredientManufacturerRoleEnum : Array[TFhirIngredientManufacturerRoleEnum] of String = (FHIR_URI_NONE, FHIR_URI_INGREDIENT_MANUFACTURER_ROLE, FHIR_URI_INGREDIENT_MANUFACTURER_ROLE, FHIR_URI_INGREDIENT_MANUFACTURER_ROLE);
  CODES_TFhirInteractionTriggerEnum : Array[TFhirInteractionTriggerEnum] of String = ('', 'create', 'update', 'delete');
  SYSTEMS_TFhirInteractionTriggerEnum : Array[TFhirInteractionTriggerEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION);
  CODES_TFhirInventoryCountTypeEnum : Array[TFhirInventoryCountTypeEnum] of String = ('', 'snapshot', 'difference');
  SYSTEMS_TFhirInventoryCountTypeEnum : Array[TFhirInventoryCountTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_INVENTORYREPORT_COUNTTYPE, FHIR_URI_INVENTORYREPORT_COUNTTYPE);
  CODES_TFhirInventoryReportStatusEnum : Array[TFhirInventoryReportStatusEnum] of String = ('', 'draft', 'requested', 'active', 'entered-in-error');
  SYSTEMS_TFhirInventoryReportStatusEnum : Array[TFhirInventoryReportStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_INVENTORYREPORT_STATUS, FHIR_URI_INVENTORYREPORT_STATUS, FHIR_URI_INVENTORYREPORT_STATUS, FHIR_URI_INVENTORYREPORT_STATUS);
  CODES_TFhirInvoiceStatusEnum : Array[TFhirInvoiceStatusEnum] of String = ('', 'draft', 'issued', 'balanced', 'cancelled', 'entered-in-error');
  SYSTEMS_TFhirInvoiceStatusEnum : Array[TFhirInvoiceStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_INVOICE_STATUS, FHIR_URI_INVOICE_STATUS, FHIR_URI_INVOICE_STATUS, FHIR_URI_INVOICE_STATUS, FHIR_URI_INVOICE_STATUS);
  CODES_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = ('', 'fatal', 'error', 'warning', 'information');
  SYSTEMS_TFhirIssueSeverityEnum : Array[TFhirIssueSeverityEnum] of String = (FHIR_URI_NONE, FHIR_URI_ISSUE_SEVERITY, FHIR_URI_ISSUE_SEVERITY, FHIR_URI_ISSUE_SEVERITY, FHIR_URI_ISSUE_SEVERITY);
  CODES_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = ('', 'invalid', 'structure', 'required', 'value', 'invariant', 'security', 'login', 'unknown', 'expired', 'forbidden', 'suppressed', 'processing', 'not-supported', 'duplicate', 'multiple-matches', 'not-found', 'deleted', 'too-long', 'code-invalid', 'extension', 'too-costly', 'business-rule', 'conflict', 'transient', 'lock-error', 'no-store', 'exception', 'timeout', 'incomplete', 'throttled', 'informational');
  SYSTEMS_TFhirIssueTypeEnum : Array[TFhirIssueTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE, FHIR_URI_ISSUE_TYPE);
  CODES_TFhirKindEnum : Array[TFhirKindEnum] of String = ('', 'insurance', 'self-pay', 'other');
  SYSTEMS_TFhirKindEnum : Array[TFhirKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_COVERAGE_KIND, FHIR_URI_COVERAGE_KIND, FHIR_URI_COVERAGE_KIND);
  CODES_TFhirLinkRelationTypesEnum : Array[TFhirLinkRelationTypesEnum] of String = ('', 'about', 'acl', 'alternate', 'amphtml', 'appendix', 'apple-touch-icon', 'apple-touch-startup-image', 'archives', 'author', 'blocked-by', 'bookmark', 'canonical', 'chapter', 'cite-as', 'collection', 'contents', 'convertedFrom', 'copyright', 'create-form', 'current', 'describedby', 'describes', 'disclosure', 'dns-prefetch', 'duplicate', 'edit', 'edit-form', 'edit-media', 'enclosure', 'external', 'first', 'glossary', 'help', 'hosts', 'hub', 'icon', 'index', 'intervalAfter', 'intervalBefore', 'intervalContains', 'intervalDisjoint', 'intervalDuring', 'intervalEquals', 'intervalFinishedBy', 'intervalFinishes', 'intervalIn', 'intervalMeets', 'intervalMetBy', 'intervalOverlappedBy', 'intervalOverlaps', 'intervalStartedBy', 'intervalStarts', 'item', 'last', 'latest-version', 'license', 'linkset', 'lrdd', 'manifest', 'mask-icon', 'media-feed', 'memento', 'micropub', 'modulepreload', 'monitor', 'monitor-group', 'next',
       'next-archive', 'nofollow', 'noopener', 'noreferrer', 'opener', 'openid2.local_id', 'openid2.provider', 'original', 'P3Pv1', 'payment', 'pingback', 'preconnect', 'predecessor-version', 'prefetch', 'preload', 'prerender', 'prev', 'preview', 'previous', 'prev-archive', 'privacy-policy', 'profile', 'publication', 'related', 'restconf', 'replies', 'ruleinput', 'search', 'section', 'self', 'service', 'service-desc', 'service-doc', 'service-meta', 'sponsored', 'start', 'status', 'stylesheet', 'subsection', 'successor-version', 'sunset', 'tag', 'terms-of-service', 'timegate', 'timemap', 'type', 'ugc', 'up', 'version-history', 'via', 'webmention', 'working-copy', 'working-copy-of');
  SYSTEMS_TFhirLinkRelationTypesEnum : Array[TFhirLinkRelationTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS,
       FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS,
       FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS,
       FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS, FHIR_URI_IANA_LINK_RELATIONS);
  CODES_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = ('', 'replaced-by', 'replaces', 'refer', 'seealso');
  SYSTEMS_TFhirLinkTypeEnum : Array[TFhirLinkTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_LINK_TYPE, FHIR_URI_LINK_TYPE, FHIR_URI_LINK_TYPE, FHIR_URI_LINK_TYPE);
  CODES_TFhirLinkageTypeEnum : Array[TFhirLinkageTypeEnum] of String = ('', 'source', 'alternate', 'historical');
  SYSTEMS_TFhirLinkageTypeEnum : Array[TFhirLinkageTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_LINKAGE_TYPE, FHIR_URI_LINKAGE_TYPE, FHIR_URI_LINKAGE_TYPE);
  CODES_TFhirListModeEnum : Array[TFhirListModeEnum] of String = ('', 'working', 'snapshot', 'changes');
  SYSTEMS_TFhirListModeEnum : Array[TFhirListModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_LIST_MODE, FHIR_URI_LIST_MODE, FHIR_URI_LIST_MODE);
  CODES_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = ('', 'current', 'retired', 'entered-in-error');
  SYSTEMS_TFhirListStatusEnum : Array[TFhirListStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_LIST_STATUS, FHIR_URI_LIST_STATUS, FHIR_URI_LIST_STATUS);
  CODES_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = ('', 'instance', 'kind');
  SYSTEMS_TFhirLocationModeEnum : Array[TFhirLocationModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_LOCATION_MODE, FHIR_URI_LOCATION_MODE);
  CODES_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = ('', 'active', 'suspended', 'inactive');
  SYSTEMS_TFhirLocationStatusEnum : Array[TFhirLocationStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_LOCATION_STATUS, FHIR_URI_LOCATION_STATUS, FHIR_URI_LOCATION_STATUS);
  CODES_TFhirMeasureReportStatusEnum : Array[TFhirMeasureReportStatusEnum] of String = ('', 'complete', 'pending', 'error');
  SYSTEMS_TFhirMeasureReportStatusEnum : Array[TFhirMeasureReportStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEASURE_REPORT_STATUS, FHIR_URI_MEASURE_REPORT_STATUS, FHIR_URI_MEASURE_REPORT_STATUS);
  CODES_TFhirMeasureReportTypeEnum : Array[TFhirMeasureReportTypeEnum] of String = ('', 'individual', 'subject-list', 'summary', 'data-exchange');
  SYSTEMS_TFhirMeasureReportTypeEnum : Array[TFhirMeasureReportTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEASURE_REPORT_TYPE, FHIR_URI_MEASURE_REPORT_TYPE, FHIR_URI_MEASURE_REPORT_TYPE, FHIR_URI_MEASURE_REPORT_TYPE);
  CODES_TFhirMedicationAdministrationStatusCodesEnum : Array[TFhirMedicationAdministrationStatusCodesEnum] of String = ('', 'in-progress', 'not-done', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'unknown');
  SYSTEMS_TFhirMedicationAdministrationStatusCodesEnum : Array[TFhirMedicationAdministrationStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS, FHIR_URI_MEDICATION_ADMIN_STATUS);
  CODES_TFhirMedicationDispenseStatusCodesEnum : Array[TFhirMedicationDispenseStatusCodesEnum] of String = ('', 'preparation', 'in-progress', 'cancelled', 'on-hold', 'completed', 'entered-in-error', 'stopped', 'declined', 'unknown');
  SYSTEMS_TFhirMedicationDispenseStatusCodesEnum : Array[TFhirMedicationDispenseStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS, FHIR_URI_MEDICATIONDISPENSE_STATUS);
  CODES_TFhirMedicationKnowledgeStatusCodesEnum : Array[TFhirMedicationKnowledgeStatusCodesEnum] of String = ('', 'active', 'entered-in-error', 'inactive');
  SYSTEMS_TFhirMedicationKnowledgeStatusCodesEnum : Array[TFhirMedicationKnowledgeStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATIONKNOWLEDGE_STATUS, FHIR_URI_MEDICATIONKNOWLEDGE_STATUS, FHIR_URI_MEDICATIONKNOWLEDGE_STATUS);
  CODES_TFhirMedicationRequestIntentEnum : Array[TFhirMedicationRequestIntentEnum] of String = ('', 'proposal', 'plan', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirMedicationRequestIntentEnum : Array[TFhirMedicationRequestIntentEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT, FHIR_URI_MEDICATIONREQUEST_INTENT);
  CODES_TFhirMedicationStatusCodesEnum : Array[TFhirMedicationStatusCodesEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirMedicationStatusCodesEnum : Array[TFhirMedicationStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATION_STATUS, FHIR_URI_MEDICATION_STATUS, FHIR_URI_MEDICATION_STATUS);
  CODES_TFhirMedicationUsageStatusCodesEnum : Array[TFhirMedicationUsageStatusCodesEnum] of String = ('', 'recorded', 'entered-in-error', 'draft');
  SYSTEMS_TFhirMedicationUsageStatusCodesEnum : Array[TFhirMedicationUsageStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATION_USAGE_STATUS, FHIR_URI_MEDICATION_USAGE_STATUS, FHIR_URI_MEDICATION_USAGE_STATUS);
  CODES_TFhirMedicationrequestStatusEnum : Array[TFhirMedicationrequestStatusEnum] of String = ('', 'active', 'on-hold', 'ended', 'stopped', 'completed', 'cancelled', 'entered-in-error', 'draft', 'unknown');
  SYSTEMS_TFhirMedicationrequestStatusEnum : Array[TFhirMedicationrequestStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS, FHIR_URI_MEDICATIONREQUEST_STATUS);
  CODES_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = ('', 'consequence', 'currency', 'notification');
  SYSTEMS_TFhirMessageSignificanceCategoryEnum : Array[TFhirMessageSignificanceCategoryEnum] of String = (FHIR_URI_NONE, FHIR_URI_MESSAGE_SIGNIFICANCE_CATEGORY, FHIR_URI_MESSAGE_SIGNIFICANCE_CATEGORY, FHIR_URI_MESSAGE_SIGNIFICANCE_CATEGORY);
  CODES_TFhirMessageheaderResponseRequestEnum : Array[TFhirMessageheaderResponseRequestEnum] of String = ('', 'always', 'on-error', 'never', 'on-success');
  SYSTEMS_TFhirMessageheaderResponseRequestEnum : Array[TFhirMessageheaderResponseRequestEnum] of String = (FHIR_URI_NONE, FHIR_URI_MESSAGEHEADER_RESPONSE_REQUEST, FHIR_URI_MESSAGEHEADER_RESPONSE_REQUEST, FHIR_URI_MESSAGEHEADER_RESPONSE_REQUEST, FHIR_URI_MESSAGEHEADER_RESPONSE_REQUEST);
  CODES_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = ('', 'usual', 'official', 'temp', 'nickname', 'anonymous', 'old', 'maiden');
  SYSTEMS_TFhirNameUseEnum : Array[TFhirNameUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE, FHIR_URI_NAME_USE);
  CODES_TFhirNamingSystemIdentifierTypeEnum : Array[TFhirNamingSystemIdentifierTypeEnum] of String = ('', 'oid', 'uuid', 'uri', 'v2csmnemonic', 'other');
  SYSTEMS_TFhirNamingSystemIdentifierTypeEnum : Array[TFhirNamingSystemIdentifierTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE, FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE, FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE, FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE, FHIR_URI_NAMINGSYSTEM_IDENTIFIER_TYPE);
  CODES_TFhirNamingSystemTypeEnum : Array[TFhirNamingSystemTypeEnum] of String = ('', 'codesystem', 'identifier', 'root');
  SYSTEMS_TFhirNamingSystemTypeEnum : Array[TFhirNamingSystemTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_NAMINGSYSTEM_TYPE, FHIR_URI_NAMINGSYSTEM_TYPE, FHIR_URI_NAMINGSYSTEM_TYPE);
  CODES_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = ('', 'generated', 'extensions', 'additional', 'empty');
  SYSTEMS_TFhirNarrativeStatusEnum : Array[TFhirNarrativeStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_NARRATIVE_STATUS, FHIR_URI_NARRATIVE_STATUS, FHIR_URI_NARRATIVE_STATUS, FHIR_URI_NARRATIVE_STATUS);
  CODES_TFhirNoteTypeEnum : Array[TFhirNoteTypeEnum] of String = ('', 'display', 'print', 'printoper');
  SYSTEMS_TFhirNoteTypeEnum : Array[TFhirNoteTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_NOTE_TYPE, FHIR_URI_NOTE_TYPE, FHIR_URI_NOTE_TYPE);
  CODES_TFhirNutritionProductStatusEnum : Array[TFhirNutritionProductStatusEnum] of String = ('', 'active', 'inactive', 'entered-in-error');
  SYSTEMS_TFhirNutritionProductStatusEnum : Array[TFhirNutritionProductStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_NUTRITIONPRODUCT_STATUS, FHIR_URI_NUTRITIONPRODUCT_STATUS, FHIR_URI_NUTRITIONPRODUCT_STATUS);
  CODES_TFhirObservationDataTypeEnum : Array[TFhirObservationDataTypeEnum] of String = ('', 'Quantity', 'CodeableConcept', 'string', 'boolean', 'integer', 'Range', 'Ratio', 'SampledData', 'time', 'dateTime', 'Period');
  SYSTEMS_TFhirObservationDataTypeEnum : Array[TFhirObservationDataTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE, FHIR_URI_PERMITTED_DATA_TYPE);
  CODES_TFhirObservationRangeCategoryEnum : Array[TFhirObservationRangeCategoryEnum] of String = ('', 'reference', 'critical', 'absolute');
  SYSTEMS_TFhirObservationRangeCategoryEnum : Array[TFhirObservationRangeCategoryEnum] of String = (FHIR_URI_NONE, FHIR_URI_OBSERVATION_RANGE_CATEGORY, FHIR_URI_OBSERVATION_RANGE_CATEGORY, FHIR_URI_OBSERVATION_RANGE_CATEGORY);
  CODES_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = ('', 'registered', 'preliminary', 'final', 'amended', 'corrected', 'cancelled', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirObservationStatusEnum : Array[TFhirObservationStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS, FHIR_URI_OBSERVATION_STATUS);
  CODES_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = ('', 'operation', 'query');
  SYSTEMS_TFhirOperationKindEnum : Array[TFhirOperationKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_OPERATION_KIND, FHIR_URI_OPERATION_KIND);
  CODES_TFhirOperationParameterScopeEnum : Array[TFhirOperationParameterScopeEnum] of String = ('', 'instance', 'type', 'system');
  SYSTEMS_TFhirOperationParameterScopeEnum : Array[TFhirOperationParameterScopeEnum] of String = (FHIR_URI_NONE, FHIR_URI_OPERATION_PARAMETER_SCOPE, FHIR_URI_OPERATION_PARAMETER_SCOPE, FHIR_URI_OPERATION_PARAMETER_SCOPE);
  CODES_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = ('', 'in', 'out');
  SYSTEMS_TFhirOperationParameterUseEnum : Array[TFhirOperationParameterUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_OPERATION_PARAMETER_USE, FHIR_URI_OPERATION_PARAMETER_USE);
  CODES_TFhirOrientationTypeEnum : Array[TFhirOrientationTypeEnum] of String = ('', 'sense', 'antisense');
  SYSTEMS_TFhirOrientationTypeEnum : Array[TFhirOrientationTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ORIENTATION_TYPE, FHIR_URI_ORIENTATION_TYPE);
  CODES_TFhirParticipationStatusEnum : Array[TFhirParticipationStatusEnum] of String = ('', 'accepted', 'declined', 'tentative', 'needs-action');
  SYSTEMS_TFhirParticipationStatusEnum : Array[TFhirParticipationStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_PARTICIPATIONSTATUS, FHIR_URI_PARTICIPATIONSTATUS, FHIR_URI_PARTICIPATIONSTATUS, FHIR_URI_PARTICIPATIONSTATUS);
  CODES_TFhirPaymentOutcomeEnum : Array[TFhirPaymentOutcomeEnum] of String = ('', 'queued', 'complete', 'error', 'partial');
  SYSTEMS_TFhirPaymentOutcomeEnum : Array[TFhirPaymentOutcomeEnum] of String = (FHIR_URI_NONE, FHIR_URI_PAYMENT_OUTCOME, FHIR_URI_PAYMENT_OUTCOME, FHIR_URI_PAYMENT_OUTCOME, FHIR_URI_PAYMENT_OUTCOME);
  CODES_TFhirPermissionRuleCombiningEnum : Array[TFhirPermissionRuleCombiningEnum] of String = ('', 'deny-overrides', 'permit-overrides', 'ordered-deny-overrides', 'ordered-permit-overrides', 'deny-unless-permit', 'permit-unless-deny');
  SYSTEMS_TFhirPermissionRuleCombiningEnum : Array[TFhirPermissionRuleCombiningEnum] of String = (FHIR_URI_NONE, FHIR_URI_PERMISSION_RULE_COMBINING, FHIR_URI_PERMISSION_RULE_COMBINING, FHIR_URI_PERMISSION_RULE_COMBINING, FHIR_URI_PERMISSION_RULE_COMBINING, FHIR_URI_PERMISSION_RULE_COMBINING, FHIR_URI_PERMISSION_RULE_COMBINING);
  CODES_TFhirPermissionStatusEnum : Array[TFhirPermissionStatusEnum] of String = ('', 'active', 'entered-in-error', 'draft', 'rejected');
  SYSTEMS_TFhirPermissionStatusEnum : Array[TFhirPermissionStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_PERMISSION_STATUS, FHIR_URI_PERMISSION_STATUS, FHIR_URI_PERMISSION_STATUS, FHIR_URI_PERMISSION_STATUS);
  CODES_TFhirPriceComponentTypeEnum : Array[TFhirPriceComponentTypeEnum] of String = ('', 'base', 'surcharge', 'deduction', 'discount', 'tax', 'informational');
  SYSTEMS_TFhirPriceComponentTypeEnum : Array[TFhirPriceComponentTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_PRICE_COMPONENT_TYPE, FHIR_URI_PRICE_COMPONENT_TYPE, FHIR_URI_PRICE_COMPONENT_TYPE, FHIR_URI_PRICE_COMPONENT_TYPE, FHIR_URI_PRICE_COMPONENT_TYPE, FHIR_URI_PRICE_COMPONENT_TYPE);
  CODES_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = ('', 'xmlAttr', 'xmlText', 'typeAttr', 'cdaText', 'xhtml');
  SYSTEMS_TFhirPropertyRepresentationEnum : Array[TFhirPropertyRepresentationEnum] of String = (FHIR_URI_NONE, FHIR_URI_PROPERTY_REPRESENTATION, FHIR_URI_PROPERTY_REPRESENTATION, FHIR_URI_PROPERTY_REPRESENTATION, FHIR_URI_PROPERTY_REPRESENTATION, FHIR_URI_PROPERTY_REPRESENTATION);
  CODES_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = ('', 'revision', 'quotation', 'source', 'instantiates', 'removal');
  SYSTEMS_TFhirProvenanceEntityRoleEnum : Array[TFhirProvenanceEntityRoleEnum] of String = (FHIR_URI_NONE, FHIR_URI_PROVENANCE_ENTITY_ROLE, FHIR_URI_PROVENANCE_ENTITY_ROLE, FHIR_URI_PROVENANCE_ENTITY_ROLE, FHIR_URI_PROVENANCE_ENTITY_ROLE, FHIR_URI_PROVENANCE_ENTITY_ROLE);
  CODES_TFhirPublicationStatusEnum : Array[TFhirPublicationStatusEnum] of String = ('', 'draft', 'active', 'retired', 'unknown');
  SYSTEMS_TFhirPublicationStatusEnum : Array[TFhirPublicationStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_PUBLICATION_STATUS, FHIR_URI_PUBLICATION_STATUS, FHIR_URI_PUBLICATION_STATUS, FHIR_URI_PUBLICATION_STATUS);
  CODES_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = ('', '<', '<=', '>=', '>', 'ad');
  SYSTEMS_TFhirQuantityComparatorEnum : Array[TFhirQuantityComparatorEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUANTITY_COMPARATOR, FHIR_URI_QUANTITY_COMPARATOR, FHIR_URI_QUANTITY_COMPARATOR, FHIR_URI_QUANTITY_COMPARATOR, FHIR_URI_QUANTITY_COMPARATOR);
  CODES_TFhirQuestionnaireAnswerConstraintEnum : Array[TFhirQuestionnaireAnswerConstraintEnum] of String = ('', 'optionsOnly', 'optionsOrType', 'optionsOrString');
  SYSTEMS_TFhirQuestionnaireAnswerConstraintEnum : Array[TFhirQuestionnaireAnswerConstraintEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUESTIONNAIRE_ANSWER_CONSTRAINT, FHIR_URI_QUESTIONNAIRE_ANSWER_CONSTRAINT, FHIR_URI_QUESTIONNAIRE_ANSWER_CONSTRAINT);
  CODES_TFhirQuestionnaireItemDisabledDisplayEnum : Array[TFhirQuestionnaireItemDisabledDisplayEnum] of String = ('', 'hidden', 'protected');
  SYSTEMS_TFhirQuestionnaireItemDisabledDisplayEnum : Array[TFhirQuestionnaireItemDisabledDisplayEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUESTIONNAIRE_DISABLED_DISPLAY, FHIR_URI_QUESTIONNAIRE_DISABLED_DISPLAY);
  CODES_TFhirQuestionnaireItemOperatorEnum : Array[TFhirQuestionnaireItemOperatorEnum] of String = ('', 'exists', '=', '!=', '>', '<', '>=', '<=');
  SYSTEMS_TFhirQuestionnaireItemOperatorEnum : Array[TFhirQuestionnaireItemOperatorEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR, FHIR_URI_QUESTIONNAIRE_ENABLE_OPERATOR);
  CODES_TFhirQuestionnaireItemTypeEnum : Array[TFhirQuestionnaireItemTypeEnum] of String = ('', 'group', 'display', 'question', 'boolean', 'decimal', 'integer', 'date', 'dateTime', 'time', 'string', 'text', 'url', 'coding', 'attachment', 'reference', 'quantity');
  SYSTEMS_TFhirQuestionnaireItemTypeEnum : Array[TFhirQuestionnaireItemTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE, FHIR_URI_ITEM_TYPE);
  CODES_TFhirQuestionnaireResponseStatusEnum : Array[TFhirQuestionnaireResponseStatusEnum] of String = ('', 'in-progress', 'completed', 'amended', 'entered-in-error', 'stopped');
  SYSTEMS_TFhirQuestionnaireResponseStatusEnum : Array[TFhirQuestionnaireResponseStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS, FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS, FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS, FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS, FHIR_URI_QUESTIONNAIRE_ANSWERS_STATUS);
  CODES_TFhirReferenceHandlingPolicyEnum : Array[TFhirReferenceHandlingPolicyEnum] of String = ('', 'literal', 'logical', 'resolves', 'enforced', 'local');
  SYSTEMS_TFhirReferenceHandlingPolicyEnum : Array[TFhirReferenceHandlingPolicyEnum] of String = (FHIR_URI_NONE, FHIR_URI_REFERENCE_HANDLING_POLICY, FHIR_URI_REFERENCE_HANDLING_POLICY, FHIR_URI_REFERENCE_HANDLING_POLICY, FHIR_URI_REFERENCE_HANDLING_POLICY, FHIR_URI_REFERENCE_HANDLING_POLICY);
  CODES_TFhirReferenceVersionRulesEnum : Array[TFhirReferenceVersionRulesEnum] of String = ('', 'either', 'independent', 'specific');
  SYSTEMS_TFhirReferenceVersionRulesEnum : Array[TFhirReferenceVersionRulesEnum] of String = (FHIR_URI_NONE, FHIR_URI_REFERENCE_VERSION_RULES, FHIR_URI_REFERENCE_VERSION_RULES, FHIR_URI_REFERENCE_VERSION_RULES);
  CODES_TFhirRelatedArtifactTypeEnum : Array[TFhirRelatedArtifactTypeEnum] of String = ('', 'documentation', 'justification', 'citation', 'predecessor', 'successor', 'derived-from', 'depends-on', 'composed-of', 'part-of', 'amends', 'amended-with', 'appends', 'appended-with', 'cites', 'cited-by', 'comments-on', 'comment-in', 'contains', 'contained-in', 'corrects', 'correction-in', 'replaces', 'replaced-with', 'retracts', 'retracted-by', 'signs', 'similar-to', 'supports', 'supported-with', 'transforms', 'transformed-into', 'transformed-with', 'documents', 'specification-of', 'created-with', 'cite-as');
  SYSTEMS_TFhirRelatedArtifactTypeEnum : Array[TFhirRelatedArtifactTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE,
       FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE);
  CODES_TFhirRelatedArtifactTypeExpandedEnum : Array[TFhirRelatedArtifactTypeExpandedEnum] of String = ('', 'documentation', 'justification', 'citation', 'predecessor', 'successor', 'derived-from', 'depends-on', 'composed-of', 'part-of', 'amends', 'amended-with', 'appends', 'appended-with', 'cites', 'cited-by', 'comments-on', 'comment-in', 'contains', 'contained-in', 'corrects', 'correction-in', 'replaces', 'replaced-with', 'retracts', 'retracted-by', 'signs', 'similar-to', 'supports', 'supported-with', 'transforms', 'transformed-into', 'transformed-with', 'documents', 'specification-of', 'created-with', 'cite-as', 'reprint', 'reprint-of');
  SYSTEMS_TFhirRelatedArtifactTypeExpandedEnum : Array[TFhirRelatedArtifactTypeExpandedEnum] of String = (FHIR_URI_NONE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE,
       FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE, FHIR_URI_RELATED_ARTIFACT_TYPE_EXPANDED, FHIR_URI_RELATED_ARTIFACT_TYPE_EXPANDED);
  CODES_TFhirReportRelationshipTypeEnum : Array[TFhirReportRelationshipTypeEnum] of String = ('', 'replaces', 'amends', 'appends', 'transforms', 'replacedWith', 'amendedWith', 'appendedWith', 'transformedWith');
  SYSTEMS_TFhirReportRelationshipTypeEnum : Array[TFhirReportRelationshipTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE, FHIR_URI_REPORT_RELATION_TYPE);
  CODES_TFhirRequestIntentEnum : Array[TFhirRequestIntentEnum] of String = ('', 'proposal', 'plan', 'directive', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirRequestIntentEnum : Array[TFhirRequestIntentEnum] of String = (FHIR_URI_NONE, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT);
  CODES_TFhirRequestPriorityEnum : Array[TFhirRequestPriorityEnum] of String = ('', 'routine', 'urgent', 'asap', 'stat');
  SYSTEMS_TFhirRequestPriorityEnum : Array[TFhirRequestPriorityEnum] of String = (FHIR_URI_NONE, FHIR_URI_REQUEST_PRIORITY, FHIR_URI_REQUEST_PRIORITY, FHIR_URI_REQUEST_PRIORITY, FHIR_URI_REQUEST_PRIORITY);
  CODES_TFhirRequestResourceTypesEnum : Array[TFhirRequestResourceTypesEnum] of String = ('', 'Appointment', 'AppointmentResponse', 'CarePlan', 'Claim', 'CommunicationRequest', 'Contract', 'CoverageEligibilityRequest', 'DeviceRequest', 'EnrollmentRequest', 'ImmunizationRecommendation', 'MedicationRequest', 'NutritionOrder', 'RequestOrchestration', 'ServiceRequest', 'SupplyRequest', 'VisionPrescription');
  SYSTEMS_TFhirRequestResourceTypesEnum : Array[TFhirRequestResourceTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES);
  CODES_TFhirRequestStatusEnum : Array[TFhirRequestStatusEnum] of String = ('', 'draft', 'active', 'on-hold', 'revoked', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirRequestStatusEnum : Array[TFhirRequestStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS, FHIR_URI_REQUEST_STATUS);
  CODES_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = ('', 'Account', 'ActivityDefinition', 'ActorDefinition', 'AdministrableProductDefinition', 'AdverseEvent', 'AllergyIntolerance', 'Appointment', 'AppointmentResponse', 'ArtifactAssessment', 'AuditEvent', 'Basic', 'Binary', 'BiologicallyDerivedProduct', 'BodyStructure', 'Bundle', 'CapabilityStatement', 'CarePlan', 'CareTeam', 'ChargeItem', 'ChargeItemDefinition', 'Citation', 'Claim', 'ClaimResponse', 'ClinicalImpression', 'ClinicalUseDefinition', 'CodeSystem', 'Communication', 'CommunicationRequest', 'CompartmentDefinition', 'Composition', 'ConceptMap', 'Condition', 'ConditionDefinition', 'Consent', 'Contract', 'Coverage', 'CoverageEligibilityRequest', 'CoverageEligibilityResponse', 'DetectedIssue', 'Device', 'DeviceDefinition', 'DeviceDispense', 'DeviceMetric', 'DeviceRequest', 'DeviceUsage', 'DiagnosticReport', 'DocumentManifest', 'DocumentReference', 'Encounter', 'Endpoint', 'EnrollmentRequest', 'EnrollmentResponse',
       'EpisodeOfCare', 'EventDefinition', 'Evidence', 'EvidenceReport', 'EvidenceVariable', 'ExampleScenario', 'ExplanationOfBenefit', 'FamilyMemberHistory', 'Flag', 'FormularyItem', 'GenomicStudy', 'Goal', 'GraphDefinition', 'Group', 'GuidanceResponse', 'HealthcareService', 'ImagingSelection', 'ImagingStudy', 'Immunization', 'ImmunizationEvaluation', 'ImmunizationRecommendation', 'ImplementationGuide', 'Ingredient', 'InsurancePlan', 'InventoryReport', 'Invoice', 'Library', 'Linkage', 'List', 'Location', 'ManufacturedItemDefinition', 'Measure', 'MeasureReport', 'Medication', 'MedicationAdministration', 'MedicationDispense', 'MedicationKnowledge', 'MedicationRequest', 'MedicationUsage', 'MedicinalProductDefinition', 'MessageDefinition', 'MessageHeader', 'MolecularSequence', 'NamingSystem', 'NutritionIntake', 'NutritionOrder', 'NutritionProduct', 'Observation', 'ObservationDefinition', 'OperationDefinition', 'OperationOutcome', 'Organization', 'OrganizationAffiliation', 'PackagedProductDefinition',
       'Parameters', 'Patient', 'PaymentNotice', 'PaymentReconciliation', 'Permission', 'Person', 'PlanDefinition', 'Practitioner', 'PractitionerRole', 'Procedure', 'Provenance', 'Questionnaire', 'QuestionnaireResponse', 'RegulatedAuthorization', 'RelatedPerson', 'RequestOrchestration', 'Requirements', 'ResearchStudy', 'ResearchSubject', 'RiskAssessment', 'Schedule', 'SearchParameter', 'ServiceRequest', 'Slot', 'Specimen', 'SpecimenDefinition', 'StructureDefinition', 'StructureMap', 'Subscription', 'SubscriptionStatus', 'SubscriptionTopic', 'Substance', 'SubstanceDefinition', 'SubstanceNucleicAcid', 'SubstancePolymer', 'SubstanceProtein', 'SubstanceReferenceInformation', 'SubstanceSourceMaterial', 'SupplyDelivery', 'SupplyRequest', 'Task', 'TerminologyCapabilities', 'TestReport', 'TestScript', 'Transport', 'ValueSet', 'VerificationResult', 'VisionPrescription');
  SYSTEMS_TFhirResourceTypesEnum : Array[TFhirResourceTypesEnum] of String = (FHIR_URI_NONE, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES,
       FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES, FHIR_URI_FHIR_TYPES);
  CODES_TFhirResourceVersionPolicyEnum : Array[TFhirResourceVersionPolicyEnum] of String = ('', 'no-version', 'versioned', 'versioned-update');
  SYSTEMS_TFhirResourceVersionPolicyEnum : Array[TFhirResourceVersionPolicyEnum] of String = (FHIR_URI_NONE, FHIR_URI_VERSIONING_POLICY, FHIR_URI_VERSIONING_POLICY, FHIR_URI_VERSIONING_POLICY);
  CODES_TFhirResponseTypeEnum : Array[TFhirResponseTypeEnum] of String = ('', 'ok', 'transient-error', 'fatal-error');
  SYSTEMS_TFhirResponseTypeEnum : Array[TFhirResponseTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESPONSE_CODE, FHIR_URI_RESPONSE_CODE, FHIR_URI_RESPONSE_CODE);
  CODES_TFhirRestfulCapabilityModeEnum : Array[TFhirRestfulCapabilityModeEnum] of String = ('', 'client', 'server');
  SYSTEMS_TFhirRestfulCapabilityModeEnum : Array[TFhirRestfulCapabilityModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESTFUL_CAPABILITY_MODE, FHIR_URI_RESTFUL_CAPABILITY_MODE);
  CODES_TFhirSPDXLicenseEnum : Array[TFhirSPDXLicenseEnum] of String = ('', 'not-open-source', '0BSD', 'AAL', 'Abstyles', 'Adobe-2006', 'Adobe-Glyph', 'ADSL', 'AFL-1.1', 'AFL-1.2', 'AFL-2.0', 'AFL-2.1', 'AFL-3.0', 'Afmparse', 'AGPL-1.0-only', 'AGPL-1.0-or-later', 'AGPL-3.0-only', 'AGPL-3.0-or-later', 'Aladdin', 'AMDPLPA', 'AML', 'AMPAS', 'ANTLR-PD', 'Apache-1.0', 'Apache-1.1', 'Apache-2.0', 'APAFML', 'APL-1.0', 'APSL-1.0', 'APSL-1.1', 'APSL-1.2', 'APSL-2.0', 'Artistic-1.0-cl8', 'Artistic-1.0-Perl', 'Artistic-1.0', 'Artistic-2.0', 'Bahyph', 'Barr', 'Beerware', 'BitTorrent-1.0', 'BitTorrent-1.1', 'Borceux', 'BSD-1-Clause', 'BSD-2-Clause-FreeBSD', 'BSD-2-Clause-NetBSD', 'BSD-2-Clause-Patent', 'BSD-2-Clause', 'BSD-3-Clause-Attribution', 'BSD-3-Clause-Clear', 'BSD-3-Clause-LBNL', 'BSD-3-Clause-No-Nuclear-License-2014', 'BSD-3-Clause-No-Nuclear-License', 'BSD-3-Clause-No-Nuclear-Warranty', 'BSD-3-Clause', 'BSD-4-Clause-UC', 'BSD-4-Clause', 'BSD-Protection', 'BSD-Source-Code', 'BSL-1.0', 'bzip2-1.0.5',
       'bzip2-1.0.6', 'Caldera', 'CATOSL-1.1', 'CC-BY-1.0', 'CC-BY-2.0', 'CC-BY-2.5', 'CC-BY-3.0', 'CC-BY-4.0', 'CC-BY-NC-1.0', 'CC-BY-NC-2.0', 'CC-BY-NC-2.5', 'CC-BY-NC-3.0', 'CC-BY-NC-4.0', 'CC-BY-NC-ND-1.0', 'CC-BY-NC-ND-2.0', 'CC-BY-NC-ND-2.5', 'CC-BY-NC-ND-3.0', 'CC-BY-NC-ND-4.0', 'CC-BY-NC-SA-1.0', 'CC-BY-NC-SA-2.0', 'CC-BY-NC-SA-2.5', 'CC-BY-NC-SA-3.0', 'CC-BY-NC-SA-4.0', 'CC-BY-ND-1.0', 'CC-BY-ND-2.0', 'CC-BY-ND-2.5', 'CC-BY-ND-3.0', 'CC-BY-ND-4.0', 'CC-BY-SA-1.0', 'CC-BY-SA-2.0', 'CC-BY-SA-2.5', 'CC-BY-SA-3.0', 'CC-BY-SA-4.0', 'CC0-1.0', 'CDDL-1.0', 'CDDL-1.1', 'CDLA-Permissive-1.0', 'CDLA-Sharing-1.0', 'CECILL-1.0', 'CECILL-1.1', 'CECILL-2.0', 'CECILL-2.1', 'CECILL-B', 'CECILL-C', 'ClArtistic', 'CNRI-Jython', 'CNRI-Python-GPL-Compatible', 'CNRI-Python', 'Condor-1.1', 'CPAL-1.0', 'CPL-1.0', 'CPOL-1.02', 'Crossword', 'CrystalStacker', 'CUA-OPL-1.0', 'Cube', 'curl', 'D-FSL-1.0', 'diffmark', 'DOC', 'Dotseqn', 'DSDP', 'dvipdfm', 'ECL-1.0', 'ECL-2.0', 'EFL-1.0', 'EFL-2.0', 'eGenix', 'Entessa',
       'EPL-1.0', 'EPL-2.0', 'ErlPL-1.1', 'EUDatagrid', 'EUPL-1.0', 'EUPL-1.1', 'EUPL-1.2', 'Eurosym', 'Fair', 'Frameworx-1.0', 'FreeImage', 'FSFAP', 'FSFUL', 'FSFULLR', 'FTL', 'GFDL-1.1-only', 'GFDL-1.1-or-later', 'GFDL-1.2-only', 'GFDL-1.2-or-later', 'GFDL-1.3-only', 'GFDL-1.3-or-later', 'Giftware', 'GL2PS', 'Glide', 'Glulxe', 'gnuplot', 'GPL-1.0-only', 'GPL-1.0-or-later', 'GPL-2.0-only', 'GPL-2.0-or-later', 'GPL-3.0-only', 'GPL-3.0-or-later', 'gSOAP-1.3b', 'HaskellReport', 'HPND', 'IBM-pibs', 'ICU', 'IJG', 'ImageMagick', 'iMatix', 'Imlib2', 'Info-ZIP', 'Intel-ACPI', 'Intel', 'Interbase-1.0', 'IPA', 'IPL-1.0', 'ISC', 'JasPer-2.0', 'JSON', 'LAL-1.2', 'LAL-1.3', 'Latex2e', 'Leptonica', 'LGPL-2.0-only', 'LGPL-2.0-or-later', 'LGPL-2.1-only', 'LGPL-2.1-or-later', 'LGPL-3.0-only', 'LGPL-3.0-or-later', 'LGPLLR', 'Libpng', 'libtiff', 'LiLiQ-P-1.1', 'LiLiQ-R-1.1', 'LiLiQ-Rplus-1.1', 'Linux-OpenIB', 'LPL-1.0', 'LPL-1.02', 'LPPL-1.0', 'LPPL-1.1', 'LPPL-1.2', 'LPPL-1.3a', 'LPPL-1.3c', 'MakeIndex', 'MirOS',
       'MIT-0', 'MIT-advertising', 'MIT-CMU', 'MIT-enna', 'MIT-feh', 'MIT', 'MITNFA', 'Motosoto', 'mpich2', 'MPL-1.0', 'MPL-1.1', 'MPL-2.0-no-copyleft-exception', 'MPL-2.0', 'MS-PL', 'MS-RL', 'MTLL', 'Multics', 'Mup', 'NASA-1.3', 'Naumen', 'NBPL-1.0', 'NCSA', 'Net-SNMP', 'NetCDF', 'Newsletr', 'NGPL', 'NLOD-1.0', 'NLPL', 'Nokia', 'NOSL', 'Noweb', 'NPL-1.0', 'NPL-1.1', 'NPOSL-3.0', 'NRL', 'NTP', 'OCCT-PL', 'OCLC-2.0', 'ODbL-1.0', 'OFL-1.0', 'OFL-1.1', 'OGTSL', 'OLDAP-1.1', 'OLDAP-1.2', 'OLDAP-1.3', 'OLDAP-1.4', 'OLDAP-2.0.1', 'OLDAP-2.0', 'OLDAP-2.1', 'OLDAP-2.2.1', 'OLDAP-2.2.2', 'OLDAP-2.2', 'OLDAP-2.3', 'OLDAP-2.4', 'OLDAP-2.5', 'OLDAP-2.6', 'OLDAP-2.7', 'OLDAP-2.8', 'OML', 'OpenSSL', 'OPL-1.0', 'OSET-PL-2.1', 'OSL-1.0', 'OSL-1.1', 'OSL-2.0', 'OSL-2.1', 'OSL-3.0', 'PDDL-1.0', 'PHP-3.0', 'PHP-3.01', 'Plexus', 'PostgreSQL', 'psfrag', 'psutils', 'Python-2.0', 'Qhull', 'QPL-1.0', 'Rdisc', 'RHeCos-1.1', 'RPL-1.1', 'RPL-1.5', 'RPSL-1.0', 'RSA-MD', 'RSCPL', 'Ruby', 'SAX-PD', 'Saxpath', 'SCEA', 'Sendmail',
       'SGI-B-1.0', 'SGI-B-1.1', 'SGI-B-2.0', 'SimPL-2.0', 'SISSL-1.2', 'SISSL', 'Sleepycat', 'SMLNJ', 'SMPPL', 'SNIA', 'Spencer-86', 'Spencer-94', 'Spencer-99', 'SPL-1.0', 'SugarCRM-1.1.3', 'SWL', 'TCL', 'TCP-wrappers', 'TMate', 'TORQUE-1.1', 'TOSL', 'Unicode-DFS-2015', 'Unicode-DFS-2016', 'Unicode-TOU', 'Unlicense', 'UPL-1.0', 'Vim', 'VOSTROM', 'VSL-1.0', 'W3C-19980720', 'W3C-20150513', 'W3C', 'Watcom-1.0', 'Wsuipa', 'WTFPL', 'X11', 'Xerox', 'XFree86-1.1', 'xinetd', 'Xnet', 'xpp', 'XSkat', 'YPL-1.0', 'YPL-1.1', 'Zed', 'Zend-2.0', 'Zimbra-1.3', 'Zimbra-1.4', 'zlib-acknowledgement', 'Zlib', 'ZPL-1.1', 'ZPL-2.0', 'ZPL-2.1');
  SYSTEMS_TFhirSPDXLicenseEnum : Array[TFhirSPDXLicenseEnum] of String = (FHIR_URI_NONE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE,
       FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE, FHIR_URI_SPDX_LICENSE);
  CODES_TFhirSearchComparatorEnum : Array[TFhirSearchComparatorEnum] of String = ('', 'eq', 'ne', 'gt', 'lt', 'ge', 'le', 'sa', 'eb', 'ap');
  SYSTEMS_TFhirSearchComparatorEnum : Array[TFhirSearchComparatorEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR);
  CODES_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = ('', 'match', 'include', 'outcome');
  SYSTEMS_TFhirSearchEntryModeEnum : Array[TFhirSearchEntryModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_ENTRY_MODE, FHIR_URI_SEARCH_ENTRY_MODE, FHIR_URI_SEARCH_ENTRY_MODE);
  CODES_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = ('', 'missing', 'exact', 'contains', 'not', 'text', 'in', 'not-in', 'below', 'above', 'type', 'identifier', 'of-type', 'code-text', 'text-advanced', 'iterate');
  SYSTEMS_TFhirSearchModifierCodeEnum : Array[TFhirSearchModifierCodeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE, FHIR_URI_SEARCH_MODIFIER_CODE);
  CODES_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = ('', 'number', 'date', 'string', 'token', 'reference', 'composite', 'quantity', 'uri', 'special');
  SYSTEMS_TFhirSearchParamTypeEnum : Array[TFhirSearchParamTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE, FHIR_URI_SEARCH_PARAM_TYPE);
  CODES_TFhirSearchProcessingModeTypeEnum : Array[TFhirSearchProcessingModeTypeEnum] of String = ('', 'normal', 'phonetic', 'other');
  SYSTEMS_TFhirSearchProcessingModeTypeEnum : Array[TFhirSearchProcessingModeTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_PROCESSINGMODE, FHIR_URI_SEARCH_PROCESSINGMODE, FHIR_URI_SEARCH_PROCESSINGMODE);
  CODES_TFhirSequenceTypeEnum : Array[TFhirSequenceTypeEnum] of String = ('', 'aa', 'dna', 'rna');
  SYSTEMS_TFhirSequenceTypeEnum : Array[TFhirSequenceTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEQUENCE_TYPE, FHIR_URI_SEQUENCE_TYPE, FHIR_URI_SEQUENCE_TYPE);
  CODES_TFhirSlicingRulesEnum : Array[TFhirSlicingRulesEnum] of String = ('', 'closed', 'open', 'openAtEnd');
  SYSTEMS_TFhirSlicingRulesEnum : Array[TFhirSlicingRulesEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESOURCE_SLICING_RULES, FHIR_URI_RESOURCE_SLICING_RULES, FHIR_URI_RESOURCE_SLICING_RULES);
  CODES_TFhirSlotStatusEnum : Array[TFhirSlotStatusEnum] of String = ('', 'busy', 'free', 'busy-unavailable', 'busy-tentative', 'entered-in-error');
  SYSTEMS_TFhirSlotStatusEnum : Array[TFhirSlotStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_SLOTSTATUS, FHIR_URI_SLOTSTATUS, FHIR_URI_SLOTSTATUS, FHIR_URI_SLOTSTATUS, FHIR_URI_SLOTSTATUS);
  CODES_TFhirSortDirectionEnum : Array[TFhirSortDirectionEnum] of String = ('', 'ascending', 'descending');
  SYSTEMS_TFhirSortDirectionEnum : Array[TFhirSortDirectionEnum] of String = (FHIR_URI_NONE, FHIR_URI_SORT_DIRECTION, FHIR_URI_SORT_DIRECTION);
  CODES_TFhirSpecimenCombinedEnum : Array[TFhirSpecimenCombinedEnum] of String = ('', 'grouped', 'pooled');
  SYSTEMS_TFhirSpecimenCombinedEnum : Array[TFhirSpecimenCombinedEnum] of String = (FHIR_URI_NONE, FHIR_URI_SPECIMEN_COMBINED, FHIR_URI_SPECIMEN_COMBINED);
  CODES_TFhirSpecimenContainedPreferenceEnum : Array[TFhirSpecimenContainedPreferenceEnum] of String = ('', 'preferred', 'alternate');
  SYSTEMS_TFhirSpecimenContainedPreferenceEnum : Array[TFhirSpecimenContainedPreferenceEnum] of String = (FHIR_URI_NONE, FHIR_URI_SPECIMEN_CONTAINED_PREFERENCE, FHIR_URI_SPECIMEN_CONTAINED_PREFERENCE);
  CODES_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = ('', 'available', 'unavailable', 'unsatisfactory', 'entered-in-error');
  SYSTEMS_TFhirSpecimenStatusEnum : Array[TFhirSpecimenStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_SPECIMEN_STATUS, FHIR_URI_SPECIMEN_STATUS, FHIR_URI_SPECIMEN_STATUS, FHIR_URI_SPECIMEN_STATUS);
  CODES_TFhirStatusEnum : Array[TFhirStatusEnum] of String = ('', 'attested', 'validated', 'in-process', 'req-revalid', 'val-fail', 'reval-fail');
  SYSTEMS_TFhirStatusEnum : Array[TFhirStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_VERIFICATIONRESULT_STATUS, FHIR_URI_VERIFICATIONRESULT_STATUS, FHIR_URI_VERIFICATIONRESULT_STATUS, FHIR_URI_VERIFICATIONRESULT_STATUS, FHIR_URI_VERIFICATIONRESULT_STATUS, FHIR_URI_VERIFICATIONRESULT_STATUS);
  CODES_TFhirStrandTypeEnum : Array[TFhirStrandTypeEnum] of String = ('', 'watson', 'crick');
  SYSTEMS_TFhirStrandTypeEnum : Array[TFhirStrandTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_STRAND_TYPE, FHIR_URI_STRAND_TYPE);
  CODES_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = ('', 'primitive-type', 'complex-type', 'resource', 'logical');
  SYSTEMS_TFhirStructureDefinitionKindEnum : Array[TFhirStructureDefinitionKindEnum] of String = (FHIR_URI_NONE, FHIR_URI_STRUCTURE_DEFINITION_KIND, FHIR_URI_STRUCTURE_DEFINITION_KIND, FHIR_URI_STRUCTURE_DEFINITION_KIND, FHIR_URI_STRUCTURE_DEFINITION_KIND);
  CODES_TFhirStructureMapGroupTypeModeEnum : Array[TFhirStructureMapGroupTypeModeEnum] of String = ('', 'types', 'type-and-types');
  SYSTEMS_TFhirStructureMapGroupTypeModeEnum : Array[TFhirStructureMapGroupTypeModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_GROUP_TYPE_MODE, FHIR_URI_MAP_GROUP_TYPE_MODE);
  CODES_TFhirStructureMapInputModeEnum : Array[TFhirStructureMapInputModeEnum] of String = ('', 'source', 'target');
  SYSTEMS_TFhirStructureMapInputModeEnum : Array[TFhirStructureMapInputModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_INPUT_MODE, FHIR_URI_MAP_INPUT_MODE);
  CODES_TFhirStructureMapModelModeEnum : Array[TFhirStructureMapModelModeEnum] of String = ('', 'source', 'queried', 'target', 'produced');
  SYSTEMS_TFhirStructureMapModelModeEnum : Array[TFhirStructureMapModelModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_MODEL_MODE, FHIR_URI_MAP_MODEL_MODE, FHIR_URI_MAP_MODEL_MODE, FHIR_URI_MAP_MODEL_MODE);
  CODES_TFhirStructureMapSourceListModeEnum : Array[TFhirStructureMapSourceListModeEnum] of String = ('', 'first', 'not_first', 'last', 'not_last', 'only_one');
  SYSTEMS_TFhirStructureMapSourceListModeEnum : Array[TFhirStructureMapSourceListModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_SOURCE_LIST_MODE, FHIR_URI_MAP_SOURCE_LIST_MODE, FHIR_URI_MAP_SOURCE_LIST_MODE, FHIR_URI_MAP_SOURCE_LIST_MODE, FHIR_URI_MAP_SOURCE_LIST_MODE);
  CODES_TFhirStructureMapTargetListModeEnum : Array[TFhirStructureMapTargetListModeEnum] of String = ('', 'first', 'share', 'last', 'collate');
  SYSTEMS_TFhirStructureMapTargetListModeEnum : Array[TFhirStructureMapTargetListModeEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_TARGET_LIST_MODE, FHIR_URI_MAP_TARGET_LIST_MODE, FHIR_URI_MAP_TARGET_LIST_MODE, FHIR_URI_MAP_TARGET_LIST_MODE);
  CODES_TFhirStructureMapTransformEnum : Array[TFhirStructureMapTransformEnum] of String = ('', 'create', 'copy', 'truncate', 'escape', 'cast', 'append', 'translate', 'reference', 'dateOp', 'uuid', 'pointer', 'evaluate', 'cc', 'c', 'qty', 'id', 'cp');
  SYSTEMS_TFhirStructureMapTransformEnum : Array[TFhirStructureMapTransformEnum] of String = (FHIR_URI_NONE, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM, FHIR_URI_MAP_TRANSFORM);
  CODES_TFhirSubmitDataUpdateTypeEnum : Array[TFhirSubmitDataUpdateTypeEnum] of String = ('', 'incremental', 'snapshot');
  SYSTEMS_TFhirSubmitDataUpdateTypeEnum : Array[TFhirSubmitDataUpdateTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBMIT_DATA_UPDATE_TYPE, FHIR_URI_SUBMIT_DATA_UPDATE_TYPE);
  CODES_TFhirSubscriptionNotificationTypeEnum : Array[TFhirSubscriptionNotificationTypeEnum] of String = ('', 'handshake', 'heartbeat', 'event-notification', 'query-status', 'query-event');
  SYSTEMS_TFhirSubscriptionNotificationTypeEnum : Array[TFhirSubscriptionNotificationTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE, FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE, FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE, FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE, FHIR_URI_SUBSCRIPTION_NOTIFICATION_TYPE);
  CODES_TFhirSubscriptionPayloadContentEnum : Array[TFhirSubscriptionPayloadContentEnum] of String = ('', 'empty', 'id-only', 'full-resource');
  SYSTEMS_TFhirSubscriptionPayloadContentEnum : Array[TFhirSubscriptionPayloadContentEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSCRIPTION_PAYLOAD_CONTENT, FHIR_URI_SUBSCRIPTION_PAYLOAD_CONTENT, FHIR_URI_SUBSCRIPTION_PAYLOAD_CONTENT);
  CODES_TFhirSubscriptionSearchModifierEnum : Array[TFhirSubscriptionSearchModifierEnum] of String = ('', '=', 'eq', 'ne', 'gt', 'lt', 'ge', 'le', 'sa', 'eb', 'ap', 'above', 'below', 'in', 'not-in', 'of-type');
  SYSTEMS_TFhirSubscriptionSearchModifierEnum : Array[TFhirSubscriptionSearchModifierEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER, FHIR_URI_SUBSCRIPTION_SEARCH_MODIFIER);
  CODES_TFhirSubscriptionStatusCodesEnum : Array[TFhirSubscriptionStatusCodesEnum] of String = ('', 'requested', 'active', 'error', 'off', 'entered-in-error');
  SYSTEMS_TFhirSubscriptionStatusCodesEnum : Array[TFhirSubscriptionStatusCodesEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUBSCRIPTION_STATUS, FHIR_URI_SUBSCRIPTION_STATUS, FHIR_URI_SUBSCRIPTION_STATUS, FHIR_URI_SUBSCRIPTION_STATUS, FHIR_URI_SUBSCRIPTION_STATUS);
  CODES_TFhirSupplyDeliveryStatusEnum : Array[TFhirSupplyDeliveryStatusEnum] of String = ('', 'in-progress', 'completed', 'abandoned', 'entered-in-error');
  SYSTEMS_TFhirSupplyDeliveryStatusEnum : Array[TFhirSupplyDeliveryStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUPPLYDELIVERY_STATUS, FHIR_URI_SUPPLYDELIVERY_STATUS, FHIR_URI_SUPPLYDELIVERY_STATUS, FHIR_URI_SUPPLYDELIVERY_STATUS);
  CODES_TFhirSupplyRequestStatusEnum : Array[TFhirSupplyRequestStatusEnum] of String = ('', 'draft', 'active', 'suspended', 'cancelled', 'completed', 'entered-in-error', 'unknown');
  SYSTEMS_TFhirSupplyRequestStatusEnum : Array[TFhirSupplyRequestStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS, FHIR_URI_SUPPLYREQUEST_STATUS);
  CODES_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = ('', 'transaction', 'batch', 'search-system', 'history-system');
  SYSTEMS_TFhirSystemRestfulInteractionEnum : Array[TFhirSystemRestfulInteractionEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION);
  CODES_TFhirTaskIntentEnum : Array[TFhirTaskIntentEnum] of String = ('', 'unknown', 'proposal', 'plan', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirTaskIntentEnum : Array[TFhirTaskIntentEnum] of String = (FHIR_URI_NONE, FHIR_URI_TASK_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT);
  CODES_TFhirTaskStatusEnum : Array[TFhirTaskStatusEnum] of String = ('', 'draft', 'requested', 'received', 'accepted', 'rejected', 'ready', 'cancelled', 'in-progress', 'on-hold', 'failed', 'completed', 'entered-in-error');
  SYSTEMS_TFhirTaskStatusEnum : Array[TFhirTaskStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS, FHIR_URI_TASK_STATUS);
  CODES_TFhirTestReportActionResultEnum : Array[TFhirTestReportActionResultEnum] of String = ('', 'pass', 'skip', 'fail', 'warning', 'error');
  SYSTEMS_TFhirTestReportActionResultEnum : Array[TFhirTestReportActionResultEnum] of String = (FHIR_URI_NONE, FHIR_URI_REPORT_ACTION_RESULT_CODES, FHIR_URI_REPORT_ACTION_RESULT_CODES, FHIR_URI_REPORT_ACTION_RESULT_CODES, FHIR_URI_REPORT_ACTION_RESULT_CODES, FHIR_URI_REPORT_ACTION_RESULT_CODES);
  CODES_TFhirTestReportParticipantTypeEnum : Array[TFhirTestReportParticipantTypeEnum] of String = ('', 'test-engine', 'client', 'server');
  SYSTEMS_TFhirTestReportParticipantTypeEnum : Array[TFhirTestReportParticipantTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_REPORT_PARTICIPANT_TYPE, FHIR_URI_REPORT_PARTICIPANT_TYPE, FHIR_URI_REPORT_PARTICIPANT_TYPE);
  CODES_TFhirTestReportResultEnum : Array[TFhirTestReportResultEnum] of String = ('', 'pass', 'fail', 'pending');
  SYSTEMS_TFhirTestReportResultEnum : Array[TFhirTestReportResultEnum] of String = (FHIR_URI_NONE, FHIR_URI_REPORT_RESULT_CODES, FHIR_URI_REPORT_RESULT_CODES, FHIR_URI_REPORT_RESULT_CODES);
  CODES_TFhirTestReportStatusEnum : Array[TFhirTestReportStatusEnum] of String = ('', 'completed', 'in-progress', 'waiting', 'stopped', 'entered-in-error');
  SYSTEMS_TFhirTestReportStatusEnum : Array[TFhirTestReportStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_REPORT_STATUS_CODES, FHIR_URI_REPORT_STATUS_CODES, FHIR_URI_REPORT_STATUS_CODES, FHIR_URI_REPORT_STATUS_CODES, FHIR_URI_REPORT_STATUS_CODES);
  CODES_TFhirTestScriptRequestMethodCodeEnum : Array[TFhirTestScriptRequestMethodCodeEnum] of String = ('', 'delete', 'get', 'options', 'patch', 'post', 'put', 'head');
  SYSTEMS_TFhirTestScriptRequestMethodCodeEnum : Array[TFhirTestScriptRequestMethodCodeEnum] of String = (FHIR_URI_NONE, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS, FHIR_URI_HTTP_OPERATIONS);
  CODES_TFhirTransportIntentEnum : Array[TFhirTransportIntentEnum] of String = ('', 'unknown', 'proposal', 'plan', 'order', 'original-order', 'reflex-order', 'filler-order', 'instance-order', 'option');
  SYSTEMS_TFhirTransportIntentEnum : Array[TFhirTransportIntentEnum] of String = (FHIR_URI_NONE, FHIR_URI_TRANSPORT_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT, FHIR_URI_REQUEST_INTENT);
  CODES_TFhirTransportStatusEnum : Array[TFhirTransportStatusEnum] of String = ('', 'in-progress', 'completed', 'abandoned', 'cancelled', 'planned', 'entered-in-error');
  SYSTEMS_TFhirTransportStatusEnum : Array[TFhirTransportStatusEnum] of String = (FHIR_URI_NONE, FHIR_URI_TRANSPORT_STATUS, FHIR_URI_TRANSPORT_STATUS, FHIR_URI_TRANSPORT_STATUS, FHIR_URI_TRANSPORT_STATUS, FHIR_URI_TRANSPORT_STATUS, FHIR_URI_TRANSPORT_STATUS);
  CODES_TFhirTriggerTypeEnum : Array[TFhirTriggerTypeEnum] of String = ('', 'named-event', 'periodic', 'data-changed', 'data-added', 'data-modified', 'data-removed', 'data-accessed', 'data-access-ended');
  SYSTEMS_TFhirTriggerTypeEnum : Array[TFhirTriggerTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE, FHIR_URI_TRIGGER_TYPE);
  CODES_TFhirTriggeredBytypeEnum : Array[TFhirTriggeredBytypeEnum] of String = ('', 'reflex', 'repeat', 're-run');
  SYSTEMS_TFhirTriggeredBytypeEnum : Array[TFhirTriggeredBytypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_OBSERVATION_TRIGGEREDBYTYPE, FHIR_URI_OBSERVATION_TRIGGEREDBYTYPE, FHIR_URI_OBSERVATION_TRIGGEREDBYTYPE);
  CODES_TFhirTypeDerivationRuleEnum : Array[TFhirTypeDerivationRuleEnum] of String = ('', 'specialization', 'constraint');
  SYSTEMS_TFhirTypeDerivationRuleEnum : Array[TFhirTypeDerivationRuleEnum] of String = (FHIR_URI_NONE, FHIR_URI_TYPE_DERIVATION_RULE, FHIR_URI_TYPE_DERIVATION_RULE);
  CODES_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = ('', 'read', 'vread', 'update', 'patch', 'delete', 'history-instance', 'history-type', 'create', 'search-type');
  SYSTEMS_TFhirTypeRestfulInteractionEnum : Array[TFhirTypeRestfulInteractionEnum] of String = (FHIR_URI_NONE, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION, FHIR_URI_RESTFUL_INTERACTION);
  CODES_TFhirUDIEntryTypeEnum : Array[TFhirUDIEntryTypeEnum] of String = ('', 'barcode', 'rfid', 'manual', 'card', 'self-reported', 'electronic-transmission', 'unknown');
  SYSTEMS_TFhirUDIEntryTypeEnum : Array[TFhirUDIEntryTypeEnum] of String = (FHIR_URI_NONE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE, FHIR_URI_UDI_ENTRY_TYPE);
  CODES_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = ('', 's', 'min', 'h', 'd', 'wk', 'mo', 'a');
  SYSTEMS_TFhirUnitsOfTimeEnum : Array[TFhirUnitsOfTimeEnum] of String = (FHIR_URI_NONE, URI_UCUM, URI_UCUM, URI_UCUM, URI_UCUM, URI_UCUM, URI_UCUM, URI_UCUM);
  CODES_TFhirUseEnum : Array[TFhirUseEnum] of String = ('', 'claim', 'preauthorization', 'predetermination');
  SYSTEMS_TFhirUseEnum : Array[TFhirUseEnum] of String = (FHIR_URI_NONE, FHIR_URI_CLAIM_USE, FHIR_URI_CLAIM_USE, FHIR_URI_CLAIM_USE);
  CODES_TFhirValueFilterComparatorEnum : Array[TFhirValueFilterComparatorEnum] of String = ('', 'eq', 'gt', 'lt', 'ge', 'le', 'sa', 'eb');
  SYSTEMS_TFhirValueFilterComparatorEnum : Array[TFhirValueFilterComparatorEnum] of String = (FHIR_URI_NONE, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR, FHIR_URI_SEARCH_COMPARATOR);
  CODES_TFhirVisionBaseEnum : Array[TFhirVisionBaseEnum] of String = ('', 'up', 'down', 'in', 'out');
  SYSTEMS_TFhirVisionBaseEnum : Array[TFhirVisionBaseEnum] of String = (FHIR_URI_NONE, FHIR_URI_VISION_BASE_CODES, FHIR_URI_VISION_BASE_CODES, FHIR_URI_VISION_BASE_CODES, FHIR_URI_VISION_BASE_CODES);
  CODES_TFhirVisionEyesEnum : Array[TFhirVisionEyesEnum] of String = ('', 'right', 'left');
  SYSTEMS_TFhirVisionEyesEnum : Array[TFhirVisionEyesEnum] of String = (FHIR_URI_NONE, FHIR_URI_VISION_EYE_CODES, FHIR_URI_VISION_EYE_CODES);


function TFhirAccountStatusEnumListAsInteger(aSet : TFhirAccountStatusEnumList) : Integer; overload;
function IntegerAsTFhirAccountStatusEnumList(i : integer) : TFhirAccountStatusEnumList; overload;
function TFhirActionCardinalityBehaviorEnumListAsInteger(aSet : TFhirActionCardinalityBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionCardinalityBehaviorEnumList(i : integer) : TFhirActionCardinalityBehaviorEnumList; overload;
function TFhirActionConditionKindEnumListAsInteger(aSet : TFhirActionConditionKindEnumList) : Integer; overload;
function IntegerAsTFhirActionConditionKindEnumList(i : integer) : TFhirActionConditionKindEnumList; overload;
function TFhirActionGroupingBehaviorEnumListAsInteger(aSet : TFhirActionGroupingBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionGroupingBehaviorEnumList(i : integer) : TFhirActionGroupingBehaviorEnumList; overload;
function TFhirActionParticipantTypeEnumListAsInteger(aSet : TFhirActionParticipantTypeEnumList) : Integer; overload;
function IntegerAsTFhirActionParticipantTypeEnumList(i : integer) : TFhirActionParticipantTypeEnumList; overload;
function TFhirActionPrecheckBehaviorEnumListAsInteger(aSet : TFhirActionPrecheckBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionPrecheckBehaviorEnumList(i : integer) : TFhirActionPrecheckBehaviorEnumList; overload;
function TFhirActionRelationshipTypeEnumListAsInteger(aSet : TFhirActionRelationshipTypeEnumList) : Integer; overload;
function IntegerAsTFhirActionRelationshipTypeEnumList(i : integer) : TFhirActionRelationshipTypeEnumList; overload;
function TFhirActionRequiredBehaviorEnumListAsInteger(aSet : TFhirActionRequiredBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionRequiredBehaviorEnumList(i : integer) : TFhirActionRequiredBehaviorEnumList; overload;
function TFhirActionSelectionBehaviorEnumListAsInteger(aSet : TFhirActionSelectionBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirActionSelectionBehaviorEnumList(i : integer) : TFhirActionSelectionBehaviorEnumList; overload;
function TFhirAddressTypeEnumListAsInteger(aSet : TFhirAddressTypeEnumList) : Integer; overload;
function IntegerAsTFhirAddressTypeEnumList(i : integer) : TFhirAddressTypeEnumList; overload;
function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer; overload;
function IntegerAsTFhirAddressUseEnumList(i : integer) : TFhirAddressUseEnumList; overload;
function TFhirAdministrativeGenderEnumListAsInteger(aSet : TFhirAdministrativeGenderEnumList) : Integer; overload;
function IntegerAsTFhirAdministrativeGenderEnumList(i : integer) : TFhirAdministrativeGenderEnumList; overload;
function TFhirAdverseEventActualityEnumListAsInteger(aSet : TFhirAdverseEventActualityEnumList) : Integer; overload;
function IntegerAsTFhirAdverseEventActualityEnumList(i : integer) : TFhirAdverseEventActualityEnumList; overload;
function TFhirAdverseEventStatusEnumListAsInteger(aSet : TFhirAdverseEventStatusEnumList) : Integer; overload;
function IntegerAsTFhirAdverseEventStatusEnumList(i : integer) : TFhirAdverseEventStatusEnumList; overload;
function TFhirAggregationModeEnumListAsInteger(aSet : TFhirAggregationModeEnumList) : Integer; overload;
function IntegerAsTFhirAggregationModeEnumList(i : integer) : TFhirAggregationModeEnumList; overload;
function TFhirAllResourceTypesEnumListAsInteger(aSet : TFhirAllResourceTypesEnumList) : Integer; overload;
function IntegerAsTFhirAllResourceTypesEnumList(i : integer) : TFhirAllResourceTypesEnumList; overload;
function TFhirAllergyIntoleranceCategoryEnumListAsInteger(aSet : TFhirAllergyIntoleranceCategoryEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : integer) : TFhirAllergyIntoleranceCategoryEnumList; overload;
function TFhirAllergyIntoleranceCriticalityEnumListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : integer) : TFhirAllergyIntoleranceCriticalityEnumList; overload;
function TFhirAllergyIntoleranceSeverityEnumListAsInteger(aSet : TFhirAllergyIntoleranceSeverityEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceSeverityEnumList(i : integer) : TFhirAllergyIntoleranceSeverityEnumList; overload;
function TFhirAllergyIntoleranceTypeEnumListAsInteger(aSet : TFhirAllergyIntoleranceTypeEnumList) : Integer; overload;
function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : integer) : TFhirAllergyIntoleranceTypeEnumList; overload;
function TFhirAppointmentStatusEnumListAsInteger(aSet : TFhirAppointmentStatusEnumList) : Integer; overload;
function IntegerAsTFhirAppointmentStatusEnumList(i : integer) : TFhirAppointmentStatusEnumList; overload;
function TFhirArtifactAssessmentDispositionEnumListAsInteger(aSet : TFhirArtifactAssessmentDispositionEnumList) : Integer; overload;
function IntegerAsTFhirArtifactAssessmentDispositionEnumList(i : integer) : TFhirArtifactAssessmentDispositionEnumList; overload;
function TFhirArtifactAssessmentInformationTypeEnumListAsInteger(aSet : TFhirArtifactAssessmentInformationTypeEnumList) : Integer; overload;
function IntegerAsTFhirArtifactAssessmentInformationTypeEnumList(i : integer) : TFhirArtifactAssessmentInformationTypeEnumList; overload;
function TFhirArtifactAssessmentWorkflowStatusEnumListAsInteger(aSet : TFhirArtifactAssessmentWorkflowStatusEnumList) : Integer; overload;
function IntegerAsTFhirArtifactAssessmentWorkflowStatusEnumList(i : integer) : TFhirArtifactAssessmentWorkflowStatusEnumList; overload;
function TFhirAssertionDirectionTypeEnumListAsInteger(aSet : TFhirAssertionDirectionTypeEnumList) : Integer; overload;
function IntegerAsTFhirAssertionDirectionTypeEnumList(i : integer) : TFhirAssertionDirectionTypeEnumList; overload;
function TFhirAssertionOperatorTypeEnumListAsInteger(aSet : TFhirAssertionOperatorTypeEnumList) : Integer; overload;
function IntegerAsTFhirAssertionOperatorTypeEnumList(i : integer) : TFhirAssertionOperatorTypeEnumList; overload;
function TFhirAssertionResponseTypesEnumListAsInteger(aSet : TFhirAssertionResponseTypesEnumList) : Integer; overload;
function IntegerAsTFhirAssertionResponseTypesEnumList(i : integer) : TFhirAssertionResponseTypesEnumList; overload;
function TFhirAuditEventActionEnumListAsInteger(aSet : TFhirAuditEventActionEnumList) : Integer; overload;
function IntegerAsTFhirAuditEventActionEnumList(i : integer) : TFhirAuditEventActionEnumList; overload;
function TFhirAuditEventSeverityEnumListAsInteger(aSet : TFhirAuditEventSeverityEnumList) : Integer; overload;
function IntegerAsTFhirAuditEventSeverityEnumList(i : integer) : TFhirAuditEventSeverityEnumList; overload;
function TFhirBindingStrengthEnumListAsInteger(aSet : TFhirBindingStrengthEnumList) : Integer; overload;
function IntegerAsTFhirBindingStrengthEnumList(i : integer) : TFhirBindingStrengthEnumList; overload;
function TFhirBundleTypeEnumListAsInteger(aSet : TFhirBundleTypeEnumList) : Integer; overload;
function IntegerAsTFhirBundleTypeEnumList(i : integer) : TFhirBundleTypeEnumList; overload;
function TFhirCapabilityStatementKindEnumListAsInteger(aSet : TFhirCapabilityStatementKindEnumList) : Integer; overload;
function IntegerAsTFhirCapabilityStatementKindEnumList(i : integer) : TFhirCapabilityStatementKindEnumList; overload;
function TFhirCarePlanActivityKindEnumListAsInteger(aSet : TFhirCarePlanActivityKindEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanActivityKindEnumList(i : integer) : TFhirCarePlanActivityKindEnumList; overload;
function TFhirCarePlanActivityStatusEnumListAsInteger(aSet : TFhirCarePlanActivityStatusEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanActivityStatusEnumList(i : integer) : TFhirCarePlanActivityStatusEnumList; overload;
function TFhirCarePlanIntentEnumListAsInteger(aSet : TFhirCarePlanIntentEnumList) : Integer; overload;
function IntegerAsTFhirCarePlanIntentEnumList(i : integer) : TFhirCarePlanIntentEnumList; overload;
function TFhirCareTeamStatusEnumListAsInteger(aSet : TFhirCareTeamStatusEnumList) : Integer; overload;
function IntegerAsTFhirCareTeamStatusEnumList(i : integer) : TFhirCareTeamStatusEnumList; overload;
function TFhirCharacteristicCombinationEnumListAsInteger(aSet : TFhirCharacteristicCombinationEnumList) : Integer; overload;
function IntegerAsTFhirCharacteristicCombinationEnumList(i : integer) : TFhirCharacteristicCombinationEnumList; overload;
function TFhirChargeItemStatusEnumListAsInteger(aSet : TFhirChargeItemStatusEnumList) : Integer; overload;
function IntegerAsTFhirChargeItemStatusEnumList(i : integer) : TFhirChargeItemStatusEnumList; overload;
function TFhirClaimProcessingCodesEnumListAsInteger(aSet : TFhirClaimProcessingCodesEnumList) : Integer; overload;
function IntegerAsTFhirClaimProcessingCodesEnumList(i : integer) : TFhirClaimProcessingCodesEnumList; overload;
function TFhirClinicalUseDefinitionTypeEnumListAsInteger(aSet : TFhirClinicalUseDefinitionTypeEnumList) : Integer; overload;
function IntegerAsTFhirClinicalUseDefinitionTypeEnumList(i : integer) : TFhirClinicalUseDefinitionTypeEnumList; overload;
function TFhirCodeSearchSupportEnumListAsInteger(aSet : TFhirCodeSearchSupportEnumList) : Integer; overload;
function IntegerAsTFhirCodeSearchSupportEnumList(i : integer) : TFhirCodeSearchSupportEnumList; overload;
function TFhirCodeSystemContentModeEnumListAsInteger(aSet : TFhirCodeSystemContentModeEnumList) : Integer; overload;
function IntegerAsTFhirCodeSystemContentModeEnumList(i : integer) : TFhirCodeSystemContentModeEnumList; overload;
function TFhirCodeSystemHierarchyMeaningEnumListAsInteger(aSet : TFhirCodeSystemHierarchyMeaningEnumList) : Integer; overload;
function IntegerAsTFhirCodeSystemHierarchyMeaningEnumList(i : integer) : TFhirCodeSystemHierarchyMeaningEnumList; overload;
function TFhirCommonLanguagesEnumListAsInteger(aSet : TFhirCommonLanguagesEnumList) : Integer; overload;
function IntegerAsTFhirCommonLanguagesEnumList(i : integer) : TFhirCommonLanguagesEnumList; overload;
function TFhirCompartmentTypeEnumListAsInteger(aSet : TFhirCompartmentTypeEnumList) : Integer; overload;
function IntegerAsTFhirCompartmentTypeEnumList(i : integer) : TFhirCompartmentTypeEnumList; overload;
function TFhirCompositionStatusEnumListAsInteger(aSet : TFhirCompositionStatusEnumList) : Integer; overload;
function IntegerAsTFhirCompositionStatusEnumList(i : integer) : TFhirCompositionStatusEnumList; overload;
function TFhirConceptMapGroupUnmappedModeEnumListAsInteger(aSet : TFhirConceptMapGroupUnmappedModeEnumList) : Integer; overload;
function IntegerAsTFhirConceptMapGroupUnmappedModeEnumList(i : integer) : TFhirConceptMapGroupUnmappedModeEnumList; overload;
function TFhirConceptMapRelationshipEnumListAsInteger(aSet : TFhirConceptMapRelationshipEnumList) : Integer; overload;
function IntegerAsTFhirConceptMapRelationshipEnumList(i : integer) : TFhirConceptMapRelationshipEnumList; overload;
function TFhirConceptPropertyTypeEnumListAsInteger(aSet : TFhirConceptPropertyTypeEnumList) : Integer; overload;
function IntegerAsTFhirConceptPropertyTypeEnumList(i : integer) : TFhirConceptPropertyTypeEnumList; overload;
function TFhirConditionPreconditionTypeEnumListAsInteger(aSet : TFhirConditionPreconditionTypeEnumList) : Integer; overload;
function IntegerAsTFhirConditionPreconditionTypeEnumList(i : integer) : TFhirConditionPreconditionTypeEnumList; overload;
function TFhirConditionQuestionnairePurposeEnumListAsInteger(aSet : TFhirConditionQuestionnairePurposeEnumList) : Integer; overload;
function IntegerAsTFhirConditionQuestionnairePurposeEnumList(i : integer) : TFhirConditionQuestionnairePurposeEnumList; overload;
function TFhirConditionalDeleteStatusEnumListAsInteger(aSet : TFhirConditionalDeleteStatusEnumList) : Integer; overload;
function IntegerAsTFhirConditionalDeleteStatusEnumList(i : integer) : TFhirConditionalDeleteStatusEnumList; overload;
function TFhirConditionalReadStatusEnumListAsInteger(aSet : TFhirConditionalReadStatusEnumList) : Integer; overload;
function IntegerAsTFhirConditionalReadStatusEnumList(i : integer) : TFhirConditionalReadStatusEnumList; overload;
function TFhirConformanceExpectationEnumListAsInteger(aSet : TFhirConformanceExpectationEnumList) : Integer; overload;
function IntegerAsTFhirConformanceExpectationEnumList(i : integer) : TFhirConformanceExpectationEnumList; overload;
function TFhirConsentDataMeaningEnumListAsInteger(aSet : TFhirConsentDataMeaningEnumList) : Integer; overload;
function IntegerAsTFhirConsentDataMeaningEnumList(i : integer) : TFhirConsentDataMeaningEnumList; overload;
function TFhirConsentProvisionTypeEnumListAsInteger(aSet : TFhirConsentProvisionTypeEnumList) : Integer; overload;
function IntegerAsTFhirConsentProvisionTypeEnumList(i : integer) : TFhirConsentProvisionTypeEnumList; overload;
function TFhirConsentStateEnumListAsInteger(aSet : TFhirConsentStateEnumList) : Integer; overload;
function IntegerAsTFhirConsentStateEnumList(i : integer) : TFhirConsentStateEnumList; overload;
function TFhirConstraintSeverityEnumListAsInteger(aSet : TFhirConstraintSeverityEnumList) : Integer; overload;
function IntegerAsTFhirConstraintSeverityEnumList(i : integer) : TFhirConstraintSeverityEnumList; overload;
function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer; overload;
function IntegerAsTFhirContactPointSystemEnumList(i : integer) : TFhirContactPointSystemEnumList; overload;
function TFhirContactPointUseEnumListAsInteger(aSet : TFhirContactPointUseEnumList) : Integer; overload;
function IntegerAsTFhirContactPointUseEnumList(i : integer) : TFhirContactPointUseEnumList; overload;
function TFhirContractResourcePublicationStatusCodesEnumListAsInteger(aSet : TFhirContractResourcePublicationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirContractResourcePublicationStatusCodesEnumList(i : integer) : TFhirContractResourcePublicationStatusCodesEnumList; overload;
function TFhirContractResourceStatusCodesEnumListAsInteger(aSet : TFhirContractResourceStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirContractResourceStatusCodesEnumList(i : integer) : TFhirContractResourceStatusCodesEnumList; overload;
function TFhirContributorTypeEnumListAsInteger(aSet : TFhirContributorTypeEnumList) : Integer; overload;
function IntegerAsTFhirContributorTypeEnumList(i : integer) : TFhirContributorTypeEnumList; overload;
function TFhirCriteriaNotExistsBehaviorEnumListAsInteger(aSet : TFhirCriteriaNotExistsBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirCriteriaNotExistsBehaviorEnumList(i : integer) : TFhirCriteriaNotExistsBehaviorEnumList; overload;
function TFhirDaysOfWeekEnumListAsInteger(aSet : TFhirDaysOfWeekEnumList) : Integer; overload;
function IntegerAsTFhirDaysOfWeekEnumList(i : integer) : TFhirDaysOfWeekEnumList; overload;
function TFhirDetectedIssueSeverityEnumListAsInteger(aSet : TFhirDetectedIssueSeverityEnumList) : Integer; overload;
function IntegerAsTFhirDetectedIssueSeverityEnumList(i : integer) : TFhirDetectedIssueSeverityEnumList; overload;
function TFhirDetectedIssueStatusEnumListAsInteger(aSet : TFhirDetectedIssueStatusEnumList) : Integer; overload;
function IntegerAsTFhirDetectedIssueStatusEnumList(i : integer) : TFhirDetectedIssueStatusEnumList; overload;
function TFhirDeviceCorrectiveActionScopeEnumListAsInteger(aSet : TFhirDeviceCorrectiveActionScopeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceCorrectiveActionScopeEnumList(i : integer) : TFhirDeviceCorrectiveActionScopeEnumList; overload;
function TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumListAsInteger(aSet : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList(i : integer) : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList; overload;
function TFhirDeviceDispenseStatusCodesEnumListAsInteger(aSet : TFhirDeviceDispenseStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirDeviceDispenseStatusCodesEnumList(i : integer) : TFhirDeviceDispenseStatusCodesEnumList; overload;
function TFhirDeviceMetricCalibrationStateEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationStateEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCalibrationStateEnumList(i : integer) : TFhirDeviceMetricCalibrationStateEnumList; overload;
function TFhirDeviceMetricCalibrationTypeEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationTypeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCalibrationTypeEnumList(i : integer) : TFhirDeviceMetricCalibrationTypeEnumList; overload;
function TFhirDeviceMetricCategoryEnumListAsInteger(aSet : TFhirDeviceMetricCategoryEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricCategoryEnumList(i : integer) : TFhirDeviceMetricCategoryEnumList; overload;
function TFhirDeviceMetricColorEnumListAsInteger(aSet : TFhirDeviceMetricColorEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricColorEnumList(i : integer) : TFhirDeviceMetricColorEnumList; overload;
function TFhirDeviceMetricOperationalStatusEnumListAsInteger(aSet : TFhirDeviceMetricOperationalStatusEnumList) : Integer; overload;
function IntegerAsTFhirDeviceMetricOperationalStatusEnumList(i : integer) : TFhirDeviceMetricOperationalStatusEnumList; overload;
function TFhirDeviceNameTypeEnumListAsInteger(aSet : TFhirDeviceNameTypeEnumList) : Integer; overload;
function IntegerAsTFhirDeviceNameTypeEnumList(i : integer) : TFhirDeviceNameTypeEnumList; overload;
function TFhirDeviceProductionIdentifierInUDIEnumListAsInteger(aSet : TFhirDeviceProductionIdentifierInUDIEnumList) : Integer; overload;
function IntegerAsTFhirDeviceProductionIdentifierInUDIEnumList(i : integer) : TFhirDeviceProductionIdentifierInUDIEnumList; overload;
function TFhirDeviceUsageStatusEnumListAsInteger(aSet : TFhirDeviceUsageStatusEnumList) : Integer; overload;
function IntegerAsTFhirDeviceUsageStatusEnumList(i : integer) : TFhirDeviceUsageStatusEnumList; overload;
function TFhirDiagnosticReportStatusEnumListAsInteger(aSet : TFhirDiagnosticReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirDiagnosticReportStatusEnumList(i : integer) : TFhirDiagnosticReportStatusEnumList; overload;
function TFhirDiscriminatorTypeEnumListAsInteger(aSet : TFhirDiscriminatorTypeEnumList) : Integer; overload;
function IntegerAsTFhirDiscriminatorTypeEnumList(i : integer) : TFhirDiscriminatorTypeEnumList; overload;
function TFhirDocumentModeEnumListAsInteger(aSet : TFhirDocumentModeEnumList) : Integer; overload;
function IntegerAsTFhirDocumentModeEnumList(i : integer) : TFhirDocumentModeEnumList; overload;
function TFhirDocumentReferenceStatusEnumListAsInteger(aSet : TFhirDocumentReferenceStatusEnumList) : Integer; overload;
function IntegerAsTFhirDocumentReferenceStatusEnumList(i : integer) : TFhirDocumentReferenceStatusEnumList; overload;
function TFhirEligibilityOutcomeEnumListAsInteger(aSet : TFhirEligibilityOutcomeEnumList) : Integer; overload;
function IntegerAsTFhirEligibilityOutcomeEnumList(i : integer) : TFhirEligibilityOutcomeEnumList; overload;
function TFhirEligibilityRequestPurposeEnumListAsInteger(aSet : TFhirEligibilityRequestPurposeEnumList) : Integer; overload;
function IntegerAsTFhirEligibilityRequestPurposeEnumList(i : integer) : TFhirEligibilityRequestPurposeEnumList; overload;
function TFhirEligibilityResponsePurposeEnumListAsInteger(aSet : TFhirEligibilityResponsePurposeEnumList) : Integer; overload;
function IntegerAsTFhirEligibilityResponsePurposeEnumList(i : integer) : TFhirEligibilityResponsePurposeEnumList; overload;
function TFhirEnableWhenBehaviorEnumListAsInteger(aSet : TFhirEnableWhenBehaviorEnumList) : Integer; overload;
function IntegerAsTFhirEnableWhenBehaviorEnumList(i : integer) : TFhirEnableWhenBehaviorEnumList; overload;
function TFhirEncounterLocationStatusEnumListAsInteger(aSet : TFhirEncounterLocationStatusEnumList) : Integer; overload;
function IntegerAsTFhirEncounterLocationStatusEnumList(i : integer) : TFhirEncounterLocationStatusEnumList; overload;
function TFhirEncounterStatusEnumListAsInteger(aSet : TFhirEncounterStatusEnumList) : Integer; overload;
function IntegerAsTFhirEncounterStatusEnumList(i : integer) : TFhirEncounterStatusEnumList; overload;
function TFhirEndpointStatusEnumListAsInteger(aSet : TFhirEndpointStatusEnumList) : Integer; overload;
function IntegerAsTFhirEndpointStatusEnumList(i : integer) : TFhirEndpointStatusEnumList; overload;
function TFhirEnrollmentOutcomeEnumListAsInteger(aSet : TFhirEnrollmentOutcomeEnumList) : Integer; overload;
function IntegerAsTFhirEnrollmentOutcomeEnumList(i : integer) : TFhirEnrollmentOutcomeEnumList; overload;
function TFhirEpisodeOfCareStatusEnumListAsInteger(aSet : TFhirEpisodeOfCareStatusEnumList) : Integer; overload;
function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : integer) : TFhirEpisodeOfCareStatusEnumList; overload;
function TFhirEventCapabilityModeEnumListAsInteger(aSet : TFhirEventCapabilityModeEnumList) : Integer; overload;
function IntegerAsTFhirEventCapabilityModeEnumList(i : integer) : TFhirEventCapabilityModeEnumList; overload;
function TFhirEventStatusEnumListAsInteger(aSet : TFhirEventStatusEnumList) : Integer; overload;
function IntegerAsTFhirEventStatusEnumList(i : integer) : TFhirEventStatusEnumList; overload;
function TFhirEventTimingEnumListAsInteger(aSet : TFhirEventTimingEnumList) : Integer; overload;
function IntegerAsTFhirEventTimingEnumList(i : integer) : TFhirEventTimingEnumList; overload;
function TFhirEvidenceVariableHandlingEnumListAsInteger(aSet : TFhirEvidenceVariableHandlingEnumList) : Integer; overload;
function IntegerAsTFhirEvidenceVariableHandlingEnumList(i : integer) : TFhirEvidenceVariableHandlingEnumList; overload;
function TFhirExampleScenarioActorTypeEnumListAsInteger(aSet : TFhirExampleScenarioActorTypeEnumList) : Integer; overload;
function IntegerAsTFhirExampleScenarioActorTypeEnumList(i : integer) : TFhirExampleScenarioActorTypeEnumList; overload;
function TFhirExplanationOfBenefitStatusEnumListAsInteger(aSet : TFhirExplanationOfBenefitStatusEnumList) : Integer; overload;
function IntegerAsTFhirExplanationOfBenefitStatusEnumList(i : integer) : TFhirExplanationOfBenefitStatusEnumList; overload;
function TFhirExtensionContextTypeEnumListAsInteger(aSet : TFhirExtensionContextTypeEnumList) : Integer; overload;
function IntegerAsTFhirExtensionContextTypeEnumList(i : integer) : TFhirExtensionContextTypeEnumList; overload;
function TFhirFHIRDeviceStatusEnumListAsInteger(aSet : TFhirFHIRDeviceStatusEnumList) : Integer; overload;
function IntegerAsTFhirFHIRDeviceStatusEnumList(i : integer) : TFhirFHIRDeviceStatusEnumList; overload;
function TFhirFHIRSubstanceStatusEnumListAsInteger(aSet : TFhirFHIRSubstanceStatusEnumList) : Integer; overload;
function IntegerAsTFhirFHIRSubstanceStatusEnumList(i : integer) : TFhirFHIRSubstanceStatusEnumList; overload;
function TFhirFHIRTypesEnumListAsInteger(aSet : TFhirFHIRTypesEnumList) : Integer; overload;
function IntegerAsTFhirFHIRTypesEnumList(i : integer) : TFhirFHIRTypesEnumList; overload;
function TFhirFHIRVersionEnumListAsInteger(aSet : TFhirFHIRVersionEnumList) : Integer; overload;
function IntegerAsTFhirFHIRVersionEnumList(i : integer) : TFhirFHIRVersionEnumList; overload;
function TFhirFamilyHistoryStatusEnumListAsInteger(aSet : TFhirFamilyHistoryStatusEnumList) : Integer; overload;
function IntegerAsTFhirFamilyHistoryStatusEnumList(i : integer) : TFhirFamilyHistoryStatusEnumList; overload;
function TFhirFilterOperatorEnumListAsInteger(aSet : TFhirFilterOperatorEnumList) : Integer; overload;
function IntegerAsTFhirFilterOperatorEnumList(i : integer) : TFhirFilterOperatorEnumList; overload;
function TFhirFinancialResourceStatusCodesEnumListAsInteger(aSet : TFhirFinancialResourceStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirFinancialResourceStatusCodesEnumList(i : integer) : TFhirFinancialResourceStatusCodesEnumList; overload;
function TFhirFlagStatusEnumListAsInteger(aSet : TFhirFlagStatusEnumList) : Integer; overload;
function IntegerAsTFhirFlagStatusEnumList(i : integer) : TFhirFlagStatusEnumList; overload;
function TFhirFormularyItemStatusCodesEnumListAsInteger(aSet : TFhirFormularyItemStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirFormularyItemStatusCodesEnumList(i : integer) : TFhirFormularyItemStatusCodesEnumList; overload;
function TFhirGoalLifecycleStatusEnumListAsInteger(aSet : TFhirGoalLifecycleStatusEnumList) : Integer; overload;
function IntegerAsTFhirGoalLifecycleStatusEnumList(i : integer) : TFhirGoalLifecycleStatusEnumList; overload;
function TFhirGraphCompartmentRuleEnumListAsInteger(aSet : TFhirGraphCompartmentRuleEnumList) : Integer; overload;
function IntegerAsTFhirGraphCompartmentRuleEnumList(i : integer) : TFhirGraphCompartmentRuleEnumList; overload;
function TFhirGraphCompartmentUseEnumListAsInteger(aSet : TFhirGraphCompartmentUseEnumList) : Integer; overload;
function IntegerAsTFhirGraphCompartmentUseEnumList(i : integer) : TFhirGraphCompartmentUseEnumList; overload;
function TFhirGroupMembershipBasisEnumListAsInteger(aSet : TFhirGroupMembershipBasisEnumList) : Integer; overload;
function IntegerAsTFhirGroupMembershipBasisEnumList(i : integer) : TFhirGroupMembershipBasisEnumList; overload;
function TFhirGroupTypeEnumListAsInteger(aSet : TFhirGroupTypeEnumList) : Integer; overload;
function IntegerAsTFhirGroupTypeEnumList(i : integer) : TFhirGroupTypeEnumList; overload;
function TFhirGuidanceResponseStatusEnumListAsInteger(aSet : TFhirGuidanceResponseStatusEnumList) : Integer; overload;
function IntegerAsTFhirGuidanceResponseStatusEnumList(i : integer) : TFhirGuidanceResponseStatusEnumList; overload;
function TFhirGuidePageGenerationEnumListAsInteger(aSet : TFhirGuidePageGenerationEnumList) : Integer; overload;
function IntegerAsTFhirGuidePageGenerationEnumList(i : integer) : TFhirGuidePageGenerationEnumList; overload;
function TFhirHTTPVerbEnumListAsInteger(aSet : TFhirHTTPVerbEnumList) : Integer; overload;
function IntegerAsTFhirHTTPVerbEnumList(i : integer) : TFhirHTTPVerbEnumList; overload;
function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer; overload;
function IntegerAsTFhirIdentifierUseEnumList(i : integer) : TFhirIdentifierUseEnumList; overload;
function TFhirIdentityAssuranceLevelEnumListAsInteger(aSet : TFhirIdentityAssuranceLevelEnumList) : Integer; overload;
function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : integer) : TFhirIdentityAssuranceLevelEnumList; overload;
function TFhirImagingSelection2DGraphicTypeEnumListAsInteger(aSet : TFhirImagingSelection2DGraphicTypeEnumList) : Integer; overload;
function IntegerAsTFhirImagingSelection2DGraphicTypeEnumList(i : integer) : TFhirImagingSelection2DGraphicTypeEnumList; overload;
function TFhirImagingSelection3DGraphicTypeEnumListAsInteger(aSet : TFhirImagingSelection3DGraphicTypeEnumList) : Integer; overload;
function IntegerAsTFhirImagingSelection3DGraphicTypeEnumList(i : integer) : TFhirImagingSelection3DGraphicTypeEnumList; overload;
function TFhirImagingSelectionStatusEnumListAsInteger(aSet : TFhirImagingSelectionStatusEnumList) : Integer; overload;
function IntegerAsTFhirImagingSelectionStatusEnumList(i : integer) : TFhirImagingSelectionStatusEnumList; overload;
function TFhirImagingStudyStatusEnumListAsInteger(aSet : TFhirImagingStudyStatusEnumList) : Integer; overload;
function IntegerAsTFhirImagingStudyStatusEnumList(i : integer) : TFhirImagingStudyStatusEnumList; overload;
function TFhirImmunizationEvaluationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationEvaluationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirImmunizationEvaluationStatusCodesEnumList(i : integer) : TFhirImmunizationEvaluationStatusCodesEnumList; overload;
function TFhirImmunizationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirImmunizationStatusCodesEnumList(i : integer) : TFhirImmunizationStatusCodesEnumList; overload;
function TFhirIngredientManufacturerRoleEnumListAsInteger(aSet : TFhirIngredientManufacturerRoleEnumList) : Integer; overload;
function IntegerAsTFhirIngredientManufacturerRoleEnumList(i : integer) : TFhirIngredientManufacturerRoleEnumList; overload;
function TFhirInteractionTriggerEnumListAsInteger(aSet : TFhirInteractionTriggerEnumList) : Integer; overload;
function IntegerAsTFhirInteractionTriggerEnumList(i : integer) : TFhirInteractionTriggerEnumList; overload;
function TFhirInventoryCountTypeEnumListAsInteger(aSet : TFhirInventoryCountTypeEnumList) : Integer; overload;
function IntegerAsTFhirInventoryCountTypeEnumList(i : integer) : TFhirInventoryCountTypeEnumList; overload;
function TFhirInventoryReportStatusEnumListAsInteger(aSet : TFhirInventoryReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirInventoryReportStatusEnumList(i : integer) : TFhirInventoryReportStatusEnumList; overload;
function TFhirInvoiceStatusEnumListAsInteger(aSet : TFhirInvoiceStatusEnumList) : Integer; overload;
function IntegerAsTFhirInvoiceStatusEnumList(i : integer) : TFhirInvoiceStatusEnumList; overload;
function TFhirIssueSeverityEnumListAsInteger(aSet : TFhirIssueSeverityEnumList) : Integer; overload;
function IntegerAsTFhirIssueSeverityEnumList(i : integer) : TFhirIssueSeverityEnumList; overload;
function TFhirIssueTypeEnumListAsInteger(aSet : TFhirIssueTypeEnumList) : Integer; overload;
function IntegerAsTFhirIssueTypeEnumList(i : integer) : TFhirIssueTypeEnumList; overload;
function TFhirKindEnumListAsInteger(aSet : TFhirKindEnumList) : Integer; overload;
function IntegerAsTFhirKindEnumList(i : integer) : TFhirKindEnumList; overload;
function TFhirLinkRelationTypesEnumListAsInteger(aSet : TFhirLinkRelationTypesEnumList) : Integer; overload;
function IntegerAsTFhirLinkRelationTypesEnumList(i : integer) : TFhirLinkRelationTypesEnumList; overload;
function TFhirLinkTypeEnumListAsInteger(aSet : TFhirLinkTypeEnumList) : Integer; overload;
function IntegerAsTFhirLinkTypeEnumList(i : integer) : TFhirLinkTypeEnumList; overload;
function TFhirLinkageTypeEnumListAsInteger(aSet : TFhirLinkageTypeEnumList) : Integer; overload;
function IntegerAsTFhirLinkageTypeEnumList(i : integer) : TFhirLinkageTypeEnumList; overload;
function TFhirListModeEnumListAsInteger(aSet : TFhirListModeEnumList) : Integer; overload;
function IntegerAsTFhirListModeEnumList(i : integer) : TFhirListModeEnumList; overload;
function TFhirListStatusEnumListAsInteger(aSet : TFhirListStatusEnumList) : Integer; overload;
function IntegerAsTFhirListStatusEnumList(i : integer) : TFhirListStatusEnumList; overload;
function TFhirLocationModeEnumListAsInteger(aSet : TFhirLocationModeEnumList) : Integer; overload;
function IntegerAsTFhirLocationModeEnumList(i : integer) : TFhirLocationModeEnumList; overload;
function TFhirLocationStatusEnumListAsInteger(aSet : TFhirLocationStatusEnumList) : Integer; overload;
function IntegerAsTFhirLocationStatusEnumList(i : integer) : TFhirLocationStatusEnumList; overload;
function TFhirMeasureReportStatusEnumListAsInteger(aSet : TFhirMeasureReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirMeasureReportStatusEnumList(i : integer) : TFhirMeasureReportStatusEnumList; overload;
function TFhirMeasureReportTypeEnumListAsInteger(aSet : TFhirMeasureReportTypeEnumList) : Integer; overload;
function IntegerAsTFhirMeasureReportTypeEnumList(i : integer) : TFhirMeasureReportTypeEnumList; overload;
function TFhirMedicationAdministrationStatusCodesEnumListAsInteger(aSet : TFhirMedicationAdministrationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationAdministrationStatusCodesEnumList(i : integer) : TFhirMedicationAdministrationStatusCodesEnumList; overload;
function TFhirMedicationDispenseStatusCodesEnumListAsInteger(aSet : TFhirMedicationDispenseStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationDispenseStatusCodesEnumList(i : integer) : TFhirMedicationDispenseStatusCodesEnumList; overload;
function TFhirMedicationKnowledgeStatusCodesEnumListAsInteger(aSet : TFhirMedicationKnowledgeStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationKnowledgeStatusCodesEnumList(i : integer) : TFhirMedicationKnowledgeStatusCodesEnumList; overload;
function TFhirMedicationRequestIntentEnumListAsInteger(aSet : TFhirMedicationRequestIntentEnumList) : Integer; overload;
function IntegerAsTFhirMedicationRequestIntentEnumList(i : integer) : TFhirMedicationRequestIntentEnumList; overload;
function TFhirMedicationStatusCodesEnumListAsInteger(aSet : TFhirMedicationStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationStatusCodesEnumList(i : integer) : TFhirMedicationStatusCodesEnumList; overload;
function TFhirMedicationUsageStatusCodesEnumListAsInteger(aSet : TFhirMedicationUsageStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirMedicationUsageStatusCodesEnumList(i : integer) : TFhirMedicationUsageStatusCodesEnumList; overload;
function TFhirMedicationrequestStatusEnumListAsInteger(aSet : TFhirMedicationrequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirMedicationrequestStatusEnumList(i : integer) : TFhirMedicationrequestStatusEnumList; overload;
function TFhirMessageSignificanceCategoryEnumListAsInteger(aSet : TFhirMessageSignificanceCategoryEnumList) : Integer; overload;
function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : integer) : TFhirMessageSignificanceCategoryEnumList; overload;
function TFhirMessageheaderResponseRequestEnumListAsInteger(aSet : TFhirMessageheaderResponseRequestEnumList) : Integer; overload;
function IntegerAsTFhirMessageheaderResponseRequestEnumList(i : integer) : TFhirMessageheaderResponseRequestEnumList; overload;
function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer; overload;
function IntegerAsTFhirNameUseEnumList(i : integer) : TFhirNameUseEnumList; overload;
function TFhirNamingSystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingSystemIdentifierTypeEnumList) : Integer; overload;
function IntegerAsTFhirNamingSystemIdentifierTypeEnumList(i : integer) : TFhirNamingSystemIdentifierTypeEnumList; overload;
function TFhirNamingSystemTypeEnumListAsInteger(aSet : TFhirNamingSystemTypeEnumList) : Integer; overload;
function IntegerAsTFhirNamingSystemTypeEnumList(i : integer) : TFhirNamingSystemTypeEnumList; overload;
function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer; overload;
function IntegerAsTFhirNarrativeStatusEnumList(i : integer) : TFhirNarrativeStatusEnumList; overload;
function TFhirNoteTypeEnumListAsInteger(aSet : TFhirNoteTypeEnumList) : Integer; overload;
function IntegerAsTFhirNoteTypeEnumList(i : integer) : TFhirNoteTypeEnumList; overload;
function TFhirNutritionProductStatusEnumListAsInteger(aSet : TFhirNutritionProductStatusEnumList) : Integer; overload;
function IntegerAsTFhirNutritionProductStatusEnumList(i : integer) : TFhirNutritionProductStatusEnumList; overload;
function TFhirObservationDataTypeEnumListAsInteger(aSet : TFhirObservationDataTypeEnumList) : Integer; overload;
function IntegerAsTFhirObservationDataTypeEnumList(i : integer) : TFhirObservationDataTypeEnumList; overload;
function TFhirObservationRangeCategoryEnumListAsInteger(aSet : TFhirObservationRangeCategoryEnumList) : Integer; overload;
function IntegerAsTFhirObservationRangeCategoryEnumList(i : integer) : TFhirObservationRangeCategoryEnumList; overload;
function TFhirObservationStatusEnumListAsInteger(aSet : TFhirObservationStatusEnumList) : Integer; overload;
function IntegerAsTFhirObservationStatusEnumList(i : integer) : TFhirObservationStatusEnumList; overload;
function TFhirOperationKindEnumListAsInteger(aSet : TFhirOperationKindEnumList) : Integer; overload;
function IntegerAsTFhirOperationKindEnumList(i : integer) : TFhirOperationKindEnumList; overload;
function TFhirOperationParameterScopeEnumListAsInteger(aSet : TFhirOperationParameterScopeEnumList) : Integer; overload;
function IntegerAsTFhirOperationParameterScopeEnumList(i : integer) : TFhirOperationParameterScopeEnumList; overload;
function TFhirOperationParameterUseEnumListAsInteger(aSet : TFhirOperationParameterUseEnumList) : Integer; overload;
function IntegerAsTFhirOperationParameterUseEnumList(i : integer) : TFhirOperationParameterUseEnumList; overload;
function TFhirOrientationTypeEnumListAsInteger(aSet : TFhirOrientationTypeEnumList) : Integer; overload;
function IntegerAsTFhirOrientationTypeEnumList(i : integer) : TFhirOrientationTypeEnumList; overload;
function TFhirParticipationStatusEnumListAsInteger(aSet : TFhirParticipationStatusEnumList) : Integer; overload;
function IntegerAsTFhirParticipationStatusEnumList(i : integer) : TFhirParticipationStatusEnumList; overload;
function TFhirPaymentOutcomeEnumListAsInteger(aSet : TFhirPaymentOutcomeEnumList) : Integer; overload;
function IntegerAsTFhirPaymentOutcomeEnumList(i : integer) : TFhirPaymentOutcomeEnumList; overload;
function TFhirPermissionRuleCombiningEnumListAsInteger(aSet : TFhirPermissionRuleCombiningEnumList) : Integer; overload;
function IntegerAsTFhirPermissionRuleCombiningEnumList(i : integer) : TFhirPermissionRuleCombiningEnumList; overload;
function TFhirPermissionStatusEnumListAsInteger(aSet : TFhirPermissionStatusEnumList) : Integer; overload;
function IntegerAsTFhirPermissionStatusEnumList(i : integer) : TFhirPermissionStatusEnumList; overload;
function TFhirPriceComponentTypeEnumListAsInteger(aSet : TFhirPriceComponentTypeEnumList) : Integer; overload;
function IntegerAsTFhirPriceComponentTypeEnumList(i : integer) : TFhirPriceComponentTypeEnumList; overload;
function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer; overload;
function IntegerAsTFhirPropertyRepresentationEnumList(i : integer) : TFhirPropertyRepresentationEnumList; overload;
function TFhirProvenanceEntityRoleEnumListAsInteger(aSet : TFhirProvenanceEntityRoleEnumList) : Integer; overload;
function IntegerAsTFhirProvenanceEntityRoleEnumList(i : integer) : TFhirProvenanceEntityRoleEnumList; overload;
function TFhirPublicationStatusEnumListAsInteger(aSet : TFhirPublicationStatusEnumList) : Integer; overload;
function IntegerAsTFhirPublicationStatusEnumList(i : integer) : TFhirPublicationStatusEnumList; overload;
function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer; overload;
function IntegerAsTFhirQuantityComparatorEnumList(i : integer) : TFhirQuantityComparatorEnumList; overload;
function TFhirQuestionnaireAnswerConstraintEnumListAsInteger(aSet : TFhirQuestionnaireAnswerConstraintEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireAnswerConstraintEnumList(i : integer) : TFhirQuestionnaireAnswerConstraintEnumList; overload;
function TFhirQuestionnaireItemDisabledDisplayEnumListAsInteger(aSet : TFhirQuestionnaireItemDisabledDisplayEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireItemDisabledDisplayEnumList(i : integer) : TFhirQuestionnaireItemDisabledDisplayEnumList; overload;
function TFhirQuestionnaireItemOperatorEnumListAsInteger(aSet : TFhirQuestionnaireItemOperatorEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireItemOperatorEnumList(i : integer) : TFhirQuestionnaireItemOperatorEnumList; overload;
function TFhirQuestionnaireItemTypeEnumListAsInteger(aSet : TFhirQuestionnaireItemTypeEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireItemTypeEnumList(i : integer) : TFhirQuestionnaireItemTypeEnumList; overload;
function TFhirQuestionnaireResponseStatusEnumListAsInteger(aSet : TFhirQuestionnaireResponseStatusEnumList) : Integer; overload;
function IntegerAsTFhirQuestionnaireResponseStatusEnumList(i : integer) : TFhirQuestionnaireResponseStatusEnumList; overload;
function TFhirReferenceHandlingPolicyEnumListAsInteger(aSet : TFhirReferenceHandlingPolicyEnumList) : Integer; overload;
function IntegerAsTFhirReferenceHandlingPolicyEnumList(i : integer) : TFhirReferenceHandlingPolicyEnumList; overload;
function TFhirReferenceVersionRulesEnumListAsInteger(aSet : TFhirReferenceVersionRulesEnumList) : Integer; overload;
function IntegerAsTFhirReferenceVersionRulesEnumList(i : integer) : TFhirReferenceVersionRulesEnumList; overload;
function TFhirRelatedArtifactTypeEnumListAsInteger(aSet : TFhirRelatedArtifactTypeEnumList) : Integer; overload;
function IntegerAsTFhirRelatedArtifactTypeEnumList(i : integer) : TFhirRelatedArtifactTypeEnumList; overload;
function TFhirRelatedArtifactTypeExpandedEnumListAsInteger(aSet : TFhirRelatedArtifactTypeExpandedEnumList) : Integer; overload;
function IntegerAsTFhirRelatedArtifactTypeExpandedEnumList(i : integer) : TFhirRelatedArtifactTypeExpandedEnumList; overload;
function TFhirReportRelationshipTypeEnumListAsInteger(aSet : TFhirReportRelationshipTypeEnumList) : Integer; overload;
function IntegerAsTFhirReportRelationshipTypeEnumList(i : integer) : TFhirReportRelationshipTypeEnumList; overload;
function TFhirRequestIntentEnumListAsInteger(aSet : TFhirRequestIntentEnumList) : Integer; overload;
function IntegerAsTFhirRequestIntentEnumList(i : integer) : TFhirRequestIntentEnumList; overload;
function TFhirRequestPriorityEnumListAsInteger(aSet : TFhirRequestPriorityEnumList) : Integer; overload;
function IntegerAsTFhirRequestPriorityEnumList(i : integer) : TFhirRequestPriorityEnumList; overload;
function TFhirRequestResourceTypesEnumListAsInteger(aSet : TFhirRequestResourceTypesEnumList) : Integer; overload;
function IntegerAsTFhirRequestResourceTypesEnumList(i : integer) : TFhirRequestResourceTypesEnumList; overload;
function TFhirRequestStatusEnumListAsInteger(aSet : TFhirRequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirRequestStatusEnumList(i : integer) : TFhirRequestStatusEnumList; overload;
function TFhirResourceTypesEnumListAsInteger(aSet : TFhirResourceTypesEnumList) : Integer; overload;
function IntegerAsTFhirResourceTypesEnumList(i : integer) : TFhirResourceTypesEnumList; overload;
function TFhirResourceVersionPolicyEnumListAsInteger(aSet : TFhirResourceVersionPolicyEnumList) : Integer; overload;
function IntegerAsTFhirResourceVersionPolicyEnumList(i : integer) : TFhirResourceVersionPolicyEnumList; overload;
function TFhirResponseTypeEnumListAsInteger(aSet : TFhirResponseTypeEnumList) : Integer; overload;
function IntegerAsTFhirResponseTypeEnumList(i : integer) : TFhirResponseTypeEnumList; overload;
function TFhirRestfulCapabilityModeEnumListAsInteger(aSet : TFhirRestfulCapabilityModeEnumList) : Integer; overload;
function IntegerAsTFhirRestfulCapabilityModeEnumList(i : integer) : TFhirRestfulCapabilityModeEnumList; overload;
function TFhirSearchComparatorEnumListAsInteger(aSet : TFhirSearchComparatorEnumList) : Integer; overload;
function IntegerAsTFhirSearchComparatorEnumList(i : integer) : TFhirSearchComparatorEnumList; overload;
function TFhirSearchEntryModeEnumListAsInteger(aSet : TFhirSearchEntryModeEnumList) : Integer; overload;
function IntegerAsTFhirSearchEntryModeEnumList(i : integer) : TFhirSearchEntryModeEnumList; overload;
function TFhirSearchModifierCodeEnumListAsInteger(aSet : TFhirSearchModifierCodeEnumList) : Integer; overload;
function IntegerAsTFhirSearchModifierCodeEnumList(i : integer) : TFhirSearchModifierCodeEnumList; overload;
function TFhirSearchParamTypeEnumListAsInteger(aSet : TFhirSearchParamTypeEnumList) : Integer; overload;
function IntegerAsTFhirSearchParamTypeEnumList(i : integer) : TFhirSearchParamTypeEnumList; overload;
function TFhirSearchProcessingModeTypeEnumListAsInteger(aSet : TFhirSearchProcessingModeTypeEnumList) : Integer; overload;
function IntegerAsTFhirSearchProcessingModeTypeEnumList(i : integer) : TFhirSearchProcessingModeTypeEnumList; overload;
function TFhirSequenceTypeEnumListAsInteger(aSet : TFhirSequenceTypeEnumList) : Integer; overload;
function IntegerAsTFhirSequenceTypeEnumList(i : integer) : TFhirSequenceTypeEnumList; overload;
function TFhirSlicingRulesEnumListAsInteger(aSet : TFhirSlicingRulesEnumList) : Integer; overload;
function IntegerAsTFhirSlicingRulesEnumList(i : integer) : TFhirSlicingRulesEnumList; overload;
function TFhirSlotStatusEnumListAsInteger(aSet : TFhirSlotStatusEnumList) : Integer; overload;
function IntegerAsTFhirSlotStatusEnumList(i : integer) : TFhirSlotStatusEnumList; overload;
function TFhirSortDirectionEnumListAsInteger(aSet : TFhirSortDirectionEnumList) : Integer; overload;
function IntegerAsTFhirSortDirectionEnumList(i : integer) : TFhirSortDirectionEnumList; overload;
function TFhirSpecimenCombinedEnumListAsInteger(aSet : TFhirSpecimenCombinedEnumList) : Integer; overload;
function IntegerAsTFhirSpecimenCombinedEnumList(i : integer) : TFhirSpecimenCombinedEnumList; overload;
function TFhirSpecimenContainedPreferenceEnumListAsInteger(aSet : TFhirSpecimenContainedPreferenceEnumList) : Integer; overload;
function IntegerAsTFhirSpecimenContainedPreferenceEnumList(i : integer) : TFhirSpecimenContainedPreferenceEnumList; overload;
function TFhirSpecimenStatusEnumListAsInteger(aSet : TFhirSpecimenStatusEnumList) : Integer; overload;
function IntegerAsTFhirSpecimenStatusEnumList(i : integer) : TFhirSpecimenStatusEnumList; overload;
function TFhirStatusEnumListAsInteger(aSet : TFhirStatusEnumList) : Integer; overload;
function IntegerAsTFhirStatusEnumList(i : integer) : TFhirStatusEnumList; overload;
function TFhirStrandTypeEnumListAsInteger(aSet : TFhirStrandTypeEnumList) : Integer; overload;
function IntegerAsTFhirStrandTypeEnumList(i : integer) : TFhirStrandTypeEnumList; overload;
function TFhirStructureDefinitionKindEnumListAsInteger(aSet : TFhirStructureDefinitionKindEnumList) : Integer; overload;
function IntegerAsTFhirStructureDefinitionKindEnumList(i : integer) : TFhirStructureDefinitionKindEnumList; overload;
function TFhirStructureMapGroupTypeModeEnumListAsInteger(aSet : TFhirStructureMapGroupTypeModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapGroupTypeModeEnumList(i : integer) : TFhirStructureMapGroupTypeModeEnumList; overload;
function TFhirStructureMapInputModeEnumListAsInteger(aSet : TFhirStructureMapInputModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapInputModeEnumList(i : integer) : TFhirStructureMapInputModeEnumList; overload;
function TFhirStructureMapModelModeEnumListAsInteger(aSet : TFhirStructureMapModelModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapModelModeEnumList(i : integer) : TFhirStructureMapModelModeEnumList; overload;
function TFhirStructureMapSourceListModeEnumListAsInteger(aSet : TFhirStructureMapSourceListModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapSourceListModeEnumList(i : integer) : TFhirStructureMapSourceListModeEnumList; overload;
function TFhirStructureMapTargetListModeEnumListAsInteger(aSet : TFhirStructureMapTargetListModeEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapTargetListModeEnumList(i : integer) : TFhirStructureMapTargetListModeEnumList; overload;
function TFhirStructureMapTransformEnumListAsInteger(aSet : TFhirStructureMapTransformEnumList) : Integer; overload;
function IntegerAsTFhirStructureMapTransformEnumList(i : integer) : TFhirStructureMapTransformEnumList; overload;
function TFhirSubmitDataUpdateTypeEnumListAsInteger(aSet : TFhirSubmitDataUpdateTypeEnumList) : Integer; overload;
function IntegerAsTFhirSubmitDataUpdateTypeEnumList(i : integer) : TFhirSubmitDataUpdateTypeEnumList; overload;
function TFhirSubscriptionNotificationTypeEnumListAsInteger(aSet : TFhirSubscriptionNotificationTypeEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionNotificationTypeEnumList(i : integer) : TFhirSubscriptionNotificationTypeEnumList; overload;
function TFhirSubscriptionPayloadContentEnumListAsInteger(aSet : TFhirSubscriptionPayloadContentEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionPayloadContentEnumList(i : integer) : TFhirSubscriptionPayloadContentEnumList; overload;
function TFhirSubscriptionSearchModifierEnumListAsInteger(aSet : TFhirSubscriptionSearchModifierEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionSearchModifierEnumList(i : integer) : TFhirSubscriptionSearchModifierEnumList; overload;
function TFhirSubscriptionStatusCodesEnumListAsInteger(aSet : TFhirSubscriptionStatusCodesEnumList) : Integer; overload;
function IntegerAsTFhirSubscriptionStatusCodesEnumList(i : integer) : TFhirSubscriptionStatusCodesEnumList; overload;
function TFhirSupplyDeliveryStatusEnumListAsInteger(aSet : TFhirSupplyDeliveryStatusEnumList) : Integer; overload;
function IntegerAsTFhirSupplyDeliveryStatusEnumList(i : integer) : TFhirSupplyDeliveryStatusEnumList; overload;
function TFhirSupplyRequestStatusEnumListAsInteger(aSet : TFhirSupplyRequestStatusEnumList) : Integer; overload;
function IntegerAsTFhirSupplyRequestStatusEnumList(i : integer) : TFhirSupplyRequestStatusEnumList; overload;
function TFhirSystemRestfulInteractionEnumListAsInteger(aSet : TFhirSystemRestfulInteractionEnumList) : Integer; overload;
function IntegerAsTFhirSystemRestfulInteractionEnumList(i : integer) : TFhirSystemRestfulInteractionEnumList; overload;
function TFhirTaskIntentEnumListAsInteger(aSet : TFhirTaskIntentEnumList) : Integer; overload;
function IntegerAsTFhirTaskIntentEnumList(i : integer) : TFhirTaskIntentEnumList; overload;
function TFhirTaskStatusEnumListAsInteger(aSet : TFhirTaskStatusEnumList) : Integer; overload;
function IntegerAsTFhirTaskStatusEnumList(i : integer) : TFhirTaskStatusEnumList; overload;
function TFhirTestReportActionResultEnumListAsInteger(aSet : TFhirTestReportActionResultEnumList) : Integer; overload;
function IntegerAsTFhirTestReportActionResultEnumList(i : integer) : TFhirTestReportActionResultEnumList; overload;
function TFhirTestReportParticipantTypeEnumListAsInteger(aSet : TFhirTestReportParticipantTypeEnumList) : Integer; overload;
function IntegerAsTFhirTestReportParticipantTypeEnumList(i : integer) : TFhirTestReportParticipantTypeEnumList; overload;
function TFhirTestReportResultEnumListAsInteger(aSet : TFhirTestReportResultEnumList) : Integer; overload;
function IntegerAsTFhirTestReportResultEnumList(i : integer) : TFhirTestReportResultEnumList; overload;
function TFhirTestReportStatusEnumListAsInteger(aSet : TFhirTestReportStatusEnumList) : Integer; overload;
function IntegerAsTFhirTestReportStatusEnumList(i : integer) : TFhirTestReportStatusEnumList; overload;
function TFhirTestScriptRequestMethodCodeEnumListAsInteger(aSet : TFhirTestScriptRequestMethodCodeEnumList) : Integer; overload;
function IntegerAsTFhirTestScriptRequestMethodCodeEnumList(i : integer) : TFhirTestScriptRequestMethodCodeEnumList; overload;
function TFhirTransportIntentEnumListAsInteger(aSet : TFhirTransportIntentEnumList) : Integer; overload;
function IntegerAsTFhirTransportIntentEnumList(i : integer) : TFhirTransportIntentEnumList; overload;
function TFhirTransportStatusEnumListAsInteger(aSet : TFhirTransportStatusEnumList) : Integer; overload;
function IntegerAsTFhirTransportStatusEnumList(i : integer) : TFhirTransportStatusEnumList; overload;
function TFhirTriggerTypeEnumListAsInteger(aSet : TFhirTriggerTypeEnumList) : Integer; overload;
function IntegerAsTFhirTriggerTypeEnumList(i : integer) : TFhirTriggerTypeEnumList; overload;
function TFhirTriggeredBytypeEnumListAsInteger(aSet : TFhirTriggeredBytypeEnumList) : Integer; overload;
function IntegerAsTFhirTriggeredBytypeEnumList(i : integer) : TFhirTriggeredBytypeEnumList; overload;
function TFhirTypeDerivationRuleEnumListAsInteger(aSet : TFhirTypeDerivationRuleEnumList) : Integer; overload;
function IntegerAsTFhirTypeDerivationRuleEnumList(i : integer) : TFhirTypeDerivationRuleEnumList; overload;
function TFhirTypeRestfulInteractionEnumListAsInteger(aSet : TFhirTypeRestfulInteractionEnumList) : Integer; overload;
function IntegerAsTFhirTypeRestfulInteractionEnumList(i : integer) : TFhirTypeRestfulInteractionEnumList; overload;
function TFhirUDIEntryTypeEnumListAsInteger(aSet : TFhirUDIEntryTypeEnumList) : Integer; overload;
function IntegerAsTFhirUDIEntryTypeEnumList(i : integer) : TFhirUDIEntryTypeEnumList; overload;
function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer; overload;
function IntegerAsTFhirUnitsOfTimeEnumList(i : integer) : TFhirUnitsOfTimeEnumList; overload;
function TFhirUseEnumListAsInteger(aSet : TFhirUseEnumList) : Integer; overload;
function IntegerAsTFhirUseEnumList(i : integer) : TFhirUseEnumList; overload;
function TFhirValueFilterComparatorEnumListAsInteger(aSet : TFhirValueFilterComparatorEnumList) : Integer; overload;
function IntegerAsTFhirValueFilterComparatorEnumList(i : integer) : TFhirValueFilterComparatorEnumList; overload;
function TFhirVisionBaseEnumListAsInteger(aSet : TFhirVisionBaseEnumList) : Integer; overload;
function IntegerAsTFhirVisionBaseEnumList(i : integer) : TFhirVisionBaseEnumList; overload;
function TFhirVisionEyesEnumListAsInteger(aSet : TFhirVisionEyesEnumList) : Integer; overload;
function IntegerAsTFhirVisionEyesEnumList(i : integer) : TFhirVisionEyesEnumList; overload;


implementation


function TFhirAccountStatusEnumListAsInteger(aSet : TFhirAccountStatusEnumList) : Integer;
var
  a : TFhirAccountStatusEnum;
begin
  result := 0;
  for a := low(TFhirAccountStatusEnum) to high(TFhirAccountStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAccountStatusEnumList(i : Integer) : TFhirAccountStatusEnumList;
var
  aLoop : TFhirAccountStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAccountStatusEnum) to high(TFhirAccountStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionCardinalityBehaviorEnumListAsInteger(aSet : TFhirActionCardinalityBehaviorEnumList) : Integer;
var
  a : TFhirActionCardinalityBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionCardinalityBehaviorEnum) to high(TFhirActionCardinalityBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionCardinalityBehaviorEnumList(i : Integer) : TFhirActionCardinalityBehaviorEnumList;
var
  aLoop : TFhirActionCardinalityBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionCardinalityBehaviorEnum) to high(TFhirActionCardinalityBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionConditionKindEnumListAsInteger(aSet : TFhirActionConditionKindEnumList) : Integer;
var
  a : TFhirActionConditionKindEnum;
begin
  result := 0;
  for a := low(TFhirActionConditionKindEnum) to high(TFhirActionConditionKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionConditionKindEnumList(i : Integer) : TFhirActionConditionKindEnumList;
var
  aLoop : TFhirActionConditionKindEnum;
begin
  result := [];
  for aLoop := low(TFhirActionConditionKindEnum) to high(TFhirActionConditionKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionGroupingBehaviorEnumListAsInteger(aSet : TFhirActionGroupingBehaviorEnumList) : Integer;
var
  a : TFhirActionGroupingBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionGroupingBehaviorEnum) to high(TFhirActionGroupingBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionGroupingBehaviorEnumList(i : Integer) : TFhirActionGroupingBehaviorEnumList;
var
  aLoop : TFhirActionGroupingBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionGroupingBehaviorEnum) to high(TFhirActionGroupingBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionParticipantTypeEnumListAsInteger(aSet : TFhirActionParticipantTypeEnumList) : Integer;
var
  a : TFhirActionParticipantTypeEnum;
begin
  result := 0;
  for a := low(TFhirActionParticipantTypeEnum) to high(TFhirActionParticipantTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionParticipantTypeEnumList(i : Integer) : TFhirActionParticipantTypeEnumList;
var
  aLoop : TFhirActionParticipantTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirActionParticipantTypeEnum) to high(TFhirActionParticipantTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionPrecheckBehaviorEnumListAsInteger(aSet : TFhirActionPrecheckBehaviorEnumList) : Integer;
var
  a : TFhirActionPrecheckBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionPrecheckBehaviorEnum) to high(TFhirActionPrecheckBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionPrecheckBehaviorEnumList(i : Integer) : TFhirActionPrecheckBehaviorEnumList;
var
  aLoop : TFhirActionPrecheckBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionPrecheckBehaviorEnum) to high(TFhirActionPrecheckBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionRelationshipTypeEnumListAsInteger(aSet : TFhirActionRelationshipTypeEnumList) : Integer;
var
  a : TFhirActionRelationshipTypeEnum;
begin
  result := 0;
  for a := low(TFhirActionRelationshipTypeEnum) to high(TFhirActionRelationshipTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionRelationshipTypeEnumList(i : Integer) : TFhirActionRelationshipTypeEnumList;
var
  aLoop : TFhirActionRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirActionRelationshipTypeEnum) to high(TFhirActionRelationshipTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionRequiredBehaviorEnumListAsInteger(aSet : TFhirActionRequiredBehaviorEnumList) : Integer;
var
  a : TFhirActionRequiredBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionRequiredBehaviorEnum) to high(TFhirActionRequiredBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionRequiredBehaviorEnumList(i : Integer) : TFhirActionRequiredBehaviorEnumList;
var
  aLoop : TFhirActionRequiredBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionRequiredBehaviorEnum) to high(TFhirActionRequiredBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirActionSelectionBehaviorEnumListAsInteger(aSet : TFhirActionSelectionBehaviorEnumList) : Integer;
var
  a : TFhirActionSelectionBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirActionSelectionBehaviorEnum) to high(TFhirActionSelectionBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirActionSelectionBehaviorEnumList(i : Integer) : TFhirActionSelectionBehaviorEnumList;
var
  aLoop : TFhirActionSelectionBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirActionSelectionBehaviorEnum) to high(TFhirActionSelectionBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAddressTypeEnumListAsInteger(aSet : TFhirAddressTypeEnumList) : Integer;
var
  a : TFhirAddressTypeEnum;
begin
  result := 0;
  for a := low(TFhirAddressTypeEnum) to high(TFhirAddressTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressTypeEnumList(i : Integer) : TFhirAddressTypeEnumList;
var
  aLoop : TFhirAddressTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressTypeEnum) to high(TFhirAddressTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAddressUseEnumListAsInteger(aSet : TFhirAddressUseEnumList) : Integer;
var
  a : TFhirAddressUseEnum;
begin
  result := 0;
  for a := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAddressUseEnumList(i : Integer) : TFhirAddressUseEnumList;
var
  aLoop : TFhirAddressUseEnum;
begin
  result := [];
  for aLoop := low(TFhirAddressUseEnum) to high(TFhirAddressUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdministrativeGenderEnumListAsInteger(aSet : TFhirAdministrativeGenderEnumList) : Integer;
var
  a : TFhirAdministrativeGenderEnum;
begin
  result := 0;
  for a := low(TFhirAdministrativeGenderEnum) to high(TFhirAdministrativeGenderEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdministrativeGenderEnumList(i : Integer) : TFhirAdministrativeGenderEnumList;
var
  aLoop : TFhirAdministrativeGenderEnum;
begin
  result := [];
  for aLoop := low(TFhirAdministrativeGenderEnum) to high(TFhirAdministrativeGenderEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdverseEventActualityEnumListAsInteger(aSet : TFhirAdverseEventActualityEnumList) : Integer;
var
  a : TFhirAdverseEventActualityEnum;
begin
  result := 0;
  for a := low(TFhirAdverseEventActualityEnum) to high(TFhirAdverseEventActualityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdverseEventActualityEnumList(i : Integer) : TFhirAdverseEventActualityEnumList;
var
  aLoop : TFhirAdverseEventActualityEnum;
begin
  result := [];
  for aLoop := low(TFhirAdverseEventActualityEnum) to high(TFhirAdverseEventActualityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAdverseEventStatusEnumListAsInteger(aSet : TFhirAdverseEventStatusEnumList) : Integer;
var
  a : TFhirAdverseEventStatusEnum;
begin
  result := 0;
  for a := low(TFhirAdverseEventStatusEnum) to high(TFhirAdverseEventStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAdverseEventStatusEnumList(i : Integer) : TFhirAdverseEventStatusEnumList;
var
  aLoop : TFhirAdverseEventStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAdverseEventStatusEnum) to high(TFhirAdverseEventStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAggregationModeEnumListAsInteger(aSet : TFhirAggregationModeEnumList) : Integer;
var
  a : TFhirAggregationModeEnum;
begin
  result := 0;
  for a := low(TFhirAggregationModeEnum) to high(TFhirAggregationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAggregationModeEnumList(i : Integer) : TFhirAggregationModeEnumList;
var
  aLoop : TFhirAggregationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirAggregationModeEnum) to high(TFhirAggregationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllResourceTypesEnumListAsInteger(aSet : TFhirAllResourceTypesEnumList) : Integer;
var
  a : TFhirAllResourceTypesEnum;
begin
  result := 0;
  for a := low(TFhirAllResourceTypesEnum) to high(TFhirAllResourceTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllResourceTypesEnumList(i : Integer) : TFhirAllResourceTypesEnumList;
var
  aLoop : TFhirAllResourceTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirAllResourceTypesEnum) to high(TFhirAllResourceTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceCategoryEnumListAsInteger(aSet : TFhirAllergyIntoleranceCategoryEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceCategoryEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCategoryEnum) to high(TFhirAllergyIntoleranceCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCategoryEnumList(i : Integer) : TFhirAllergyIntoleranceCategoryEnumList;
var
  aLoop : TFhirAllergyIntoleranceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCategoryEnum) to high(TFhirAllergyIntoleranceCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceCriticalityEnumListAsInteger(aSet : TFhirAllergyIntoleranceCriticalityEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceCriticalityEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceCriticalityEnum) to high(TFhirAllergyIntoleranceCriticalityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceCriticalityEnumList(i : Integer) : TFhirAllergyIntoleranceCriticalityEnumList;
var
  aLoop : TFhirAllergyIntoleranceCriticalityEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceCriticalityEnum) to high(TFhirAllergyIntoleranceCriticalityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceSeverityEnumListAsInteger(aSet : TFhirAllergyIntoleranceSeverityEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceSeverityEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceSeverityEnum) to high(TFhirAllergyIntoleranceSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceSeverityEnumList(i : Integer) : TFhirAllergyIntoleranceSeverityEnumList;
var
  aLoop : TFhirAllergyIntoleranceSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceSeverityEnum) to high(TFhirAllergyIntoleranceSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAllergyIntoleranceTypeEnumListAsInteger(aSet : TFhirAllergyIntoleranceTypeEnumList) : Integer;
var
  a : TFhirAllergyIntoleranceTypeEnum;
begin
  result := 0;
  for a := low(TFhirAllergyIntoleranceTypeEnum) to high(TFhirAllergyIntoleranceTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAllergyIntoleranceTypeEnumList(i : Integer) : TFhirAllergyIntoleranceTypeEnumList;
var
  aLoop : TFhirAllergyIntoleranceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAllergyIntoleranceTypeEnum) to high(TFhirAllergyIntoleranceTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAppointmentStatusEnumListAsInteger(aSet : TFhirAppointmentStatusEnumList) : Integer;
var
  a : TFhirAppointmentStatusEnum;
begin
  result := 0;
  for a := low(TFhirAppointmentStatusEnum) to high(TFhirAppointmentStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAppointmentStatusEnumList(i : Integer) : TFhirAppointmentStatusEnumList;
var
  aLoop : TFhirAppointmentStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirAppointmentStatusEnum) to high(TFhirAppointmentStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirArtifactAssessmentDispositionEnumListAsInteger(aSet : TFhirArtifactAssessmentDispositionEnumList) : Integer;
var
  a : TFhirArtifactAssessmentDispositionEnum;
begin
  result := 0;
  for a := low(TFhirArtifactAssessmentDispositionEnum) to high(TFhirArtifactAssessmentDispositionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirArtifactAssessmentDispositionEnumList(i : Integer) : TFhirArtifactAssessmentDispositionEnumList;
var
  aLoop : TFhirArtifactAssessmentDispositionEnum;
begin
  result := [];
  for aLoop := low(TFhirArtifactAssessmentDispositionEnum) to high(TFhirArtifactAssessmentDispositionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirArtifactAssessmentInformationTypeEnumListAsInteger(aSet : TFhirArtifactAssessmentInformationTypeEnumList) : Integer;
var
  a : TFhirArtifactAssessmentInformationTypeEnum;
begin
  result := 0;
  for a := low(TFhirArtifactAssessmentInformationTypeEnum) to high(TFhirArtifactAssessmentInformationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirArtifactAssessmentInformationTypeEnumList(i : Integer) : TFhirArtifactAssessmentInformationTypeEnumList;
var
  aLoop : TFhirArtifactAssessmentInformationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirArtifactAssessmentInformationTypeEnum) to high(TFhirArtifactAssessmentInformationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirArtifactAssessmentWorkflowStatusEnumListAsInteger(aSet : TFhirArtifactAssessmentWorkflowStatusEnumList) : Integer;
var
  a : TFhirArtifactAssessmentWorkflowStatusEnum;
begin
  result := 0;
  for a := low(TFhirArtifactAssessmentWorkflowStatusEnum) to high(TFhirArtifactAssessmentWorkflowStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirArtifactAssessmentWorkflowStatusEnumList(i : Integer) : TFhirArtifactAssessmentWorkflowStatusEnumList;
var
  aLoop : TFhirArtifactAssessmentWorkflowStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirArtifactAssessmentWorkflowStatusEnum) to high(TFhirArtifactAssessmentWorkflowStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionDirectionTypeEnumListAsInteger(aSet : TFhirAssertionDirectionTypeEnumList) : Integer;
var
  a : TFhirAssertionDirectionTypeEnum;
begin
  result := 0;
  for a := low(TFhirAssertionDirectionTypeEnum) to high(TFhirAssertionDirectionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionDirectionTypeEnumList(i : Integer) : TFhirAssertionDirectionTypeEnumList;
var
  aLoop : TFhirAssertionDirectionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionDirectionTypeEnum) to high(TFhirAssertionDirectionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionOperatorTypeEnumListAsInteger(aSet : TFhirAssertionOperatorTypeEnumList) : Integer;
var
  a : TFhirAssertionOperatorTypeEnum;
begin
  result := 0;
  for a := low(TFhirAssertionOperatorTypeEnum) to high(TFhirAssertionOperatorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionOperatorTypeEnumList(i : Integer) : TFhirAssertionOperatorTypeEnumList;
var
  aLoop : TFhirAssertionOperatorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionOperatorTypeEnum) to high(TFhirAssertionOperatorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAssertionResponseTypesEnumListAsInteger(aSet : TFhirAssertionResponseTypesEnumList) : Integer;
var
  a : TFhirAssertionResponseTypesEnum;
begin
  result := 0;
  for a := low(TFhirAssertionResponseTypesEnum) to high(TFhirAssertionResponseTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAssertionResponseTypesEnumList(i : Integer) : TFhirAssertionResponseTypesEnumList;
var
  aLoop : TFhirAssertionResponseTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirAssertionResponseTypesEnum) to high(TFhirAssertionResponseTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAuditEventActionEnumListAsInteger(aSet : TFhirAuditEventActionEnumList) : Integer;
var
  a : TFhirAuditEventActionEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventActionEnum) to high(TFhirAuditEventActionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventActionEnumList(i : Integer) : TFhirAuditEventActionEnumList;
var
  aLoop : TFhirAuditEventActionEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventActionEnum) to high(TFhirAuditEventActionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirAuditEventSeverityEnumListAsInteger(aSet : TFhirAuditEventSeverityEnumList) : Integer;
var
  a : TFhirAuditEventSeverityEnum;
begin
  result := 0;
  for a := low(TFhirAuditEventSeverityEnum) to high(TFhirAuditEventSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirAuditEventSeverityEnumList(i : Integer) : TFhirAuditEventSeverityEnumList;
var
  aLoop : TFhirAuditEventSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirAuditEventSeverityEnum) to high(TFhirAuditEventSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBindingStrengthEnumListAsInteger(aSet : TFhirBindingStrengthEnumList) : Integer;
var
  a : TFhirBindingStrengthEnum;
begin
  result := 0;
  for a := low(TFhirBindingStrengthEnum) to high(TFhirBindingStrengthEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBindingStrengthEnumList(i : Integer) : TFhirBindingStrengthEnumList;
var
  aLoop : TFhirBindingStrengthEnum;
begin
  result := [];
  for aLoop := low(TFhirBindingStrengthEnum) to high(TFhirBindingStrengthEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirBundleTypeEnumListAsInteger(aSet : TFhirBundleTypeEnumList) : Integer;
var
  a : TFhirBundleTypeEnum;
begin
  result := 0;
  for a := low(TFhirBundleTypeEnum) to high(TFhirBundleTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirBundleTypeEnumList(i : Integer) : TFhirBundleTypeEnumList;
var
  aLoop : TFhirBundleTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirBundleTypeEnum) to high(TFhirBundleTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCapabilityStatementKindEnumListAsInteger(aSet : TFhirCapabilityStatementKindEnumList) : Integer;
var
  a : TFhirCapabilityStatementKindEnum;
begin
  result := 0;
  for a := low(TFhirCapabilityStatementKindEnum) to high(TFhirCapabilityStatementKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCapabilityStatementKindEnumList(i : Integer) : TFhirCapabilityStatementKindEnumList;
var
  aLoop : TFhirCapabilityStatementKindEnum;
begin
  result := [];
  for aLoop := low(TFhirCapabilityStatementKindEnum) to high(TFhirCapabilityStatementKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanActivityKindEnumListAsInteger(aSet : TFhirCarePlanActivityKindEnumList) : Integer;
var
  a : TFhirCarePlanActivityKindEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityKindEnum) to high(TFhirCarePlanActivityKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityKindEnumList(i : Integer) : TFhirCarePlanActivityKindEnumList;
var
  aLoop : TFhirCarePlanActivityKindEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityKindEnum) to high(TFhirCarePlanActivityKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanActivityStatusEnumListAsInteger(aSet : TFhirCarePlanActivityStatusEnumList) : Integer;
var
  a : TFhirCarePlanActivityStatusEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanActivityStatusEnum) to high(TFhirCarePlanActivityStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanActivityStatusEnumList(i : Integer) : TFhirCarePlanActivityStatusEnumList;
var
  aLoop : TFhirCarePlanActivityStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanActivityStatusEnum) to high(TFhirCarePlanActivityStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCarePlanIntentEnumListAsInteger(aSet : TFhirCarePlanIntentEnumList) : Integer;
var
  a : TFhirCarePlanIntentEnum;
begin
  result := 0;
  for a := low(TFhirCarePlanIntentEnum) to high(TFhirCarePlanIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCarePlanIntentEnumList(i : Integer) : TFhirCarePlanIntentEnumList;
var
  aLoop : TFhirCarePlanIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirCarePlanIntentEnum) to high(TFhirCarePlanIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCareTeamStatusEnumListAsInteger(aSet : TFhirCareTeamStatusEnumList) : Integer;
var
  a : TFhirCareTeamStatusEnum;
begin
  result := 0;
  for a := low(TFhirCareTeamStatusEnum) to high(TFhirCareTeamStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCareTeamStatusEnumList(i : Integer) : TFhirCareTeamStatusEnumList;
var
  aLoop : TFhirCareTeamStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCareTeamStatusEnum) to high(TFhirCareTeamStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCharacteristicCombinationEnumListAsInteger(aSet : TFhirCharacteristicCombinationEnumList) : Integer;
var
  a : TFhirCharacteristicCombinationEnum;
begin
  result := 0;
  for a := low(TFhirCharacteristicCombinationEnum) to high(TFhirCharacteristicCombinationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCharacteristicCombinationEnumList(i : Integer) : TFhirCharacteristicCombinationEnumList;
var
  aLoop : TFhirCharacteristicCombinationEnum;
begin
  result := [];
  for aLoop := low(TFhirCharacteristicCombinationEnum) to high(TFhirCharacteristicCombinationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirChargeItemStatusEnumListAsInteger(aSet : TFhirChargeItemStatusEnumList) : Integer;
var
  a : TFhirChargeItemStatusEnum;
begin
  result := 0;
  for a := low(TFhirChargeItemStatusEnum) to high(TFhirChargeItemStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirChargeItemStatusEnumList(i : Integer) : TFhirChargeItemStatusEnumList;
var
  aLoop : TFhirChargeItemStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirChargeItemStatusEnum) to high(TFhirChargeItemStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirClaimProcessingCodesEnumListAsInteger(aSet : TFhirClaimProcessingCodesEnumList) : Integer;
var
  a : TFhirClaimProcessingCodesEnum;
begin
  result := 0;
  for a := low(TFhirClaimProcessingCodesEnum) to high(TFhirClaimProcessingCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClaimProcessingCodesEnumList(i : Integer) : TFhirClaimProcessingCodesEnumList;
var
  aLoop : TFhirClaimProcessingCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirClaimProcessingCodesEnum) to high(TFhirClaimProcessingCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirClinicalUseDefinitionTypeEnumListAsInteger(aSet : TFhirClinicalUseDefinitionTypeEnumList) : Integer;
var
  a : TFhirClinicalUseDefinitionTypeEnum;
begin
  result := 0;
  for a := low(TFhirClinicalUseDefinitionTypeEnum) to high(TFhirClinicalUseDefinitionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirClinicalUseDefinitionTypeEnumList(i : Integer) : TFhirClinicalUseDefinitionTypeEnumList;
var
  aLoop : TFhirClinicalUseDefinitionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirClinicalUseDefinitionTypeEnum) to high(TFhirClinicalUseDefinitionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSearchSupportEnumListAsInteger(aSet : TFhirCodeSearchSupportEnumList) : Integer;
var
  a : TFhirCodeSearchSupportEnum;
begin
  result := 0;
  for a := low(TFhirCodeSearchSupportEnum) to high(TFhirCodeSearchSupportEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSearchSupportEnumList(i : Integer) : TFhirCodeSearchSupportEnumList;
var
  aLoop : TFhirCodeSearchSupportEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSearchSupportEnum) to high(TFhirCodeSearchSupportEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSystemContentModeEnumListAsInteger(aSet : TFhirCodeSystemContentModeEnumList) : Integer;
var
  a : TFhirCodeSystemContentModeEnum;
begin
  result := 0;
  for a := low(TFhirCodeSystemContentModeEnum) to high(TFhirCodeSystemContentModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSystemContentModeEnumList(i : Integer) : TFhirCodeSystemContentModeEnumList;
var
  aLoop : TFhirCodeSystemContentModeEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSystemContentModeEnum) to high(TFhirCodeSystemContentModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCodeSystemHierarchyMeaningEnumListAsInteger(aSet : TFhirCodeSystemHierarchyMeaningEnumList) : Integer;
var
  a : TFhirCodeSystemHierarchyMeaningEnum;
begin
  result := 0;
  for a := low(TFhirCodeSystemHierarchyMeaningEnum) to high(TFhirCodeSystemHierarchyMeaningEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCodeSystemHierarchyMeaningEnumList(i : Integer) : TFhirCodeSystemHierarchyMeaningEnumList;
var
  aLoop : TFhirCodeSystemHierarchyMeaningEnum;
begin
  result := [];
  for aLoop := low(TFhirCodeSystemHierarchyMeaningEnum) to high(TFhirCodeSystemHierarchyMeaningEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCommonLanguagesEnumListAsInteger(aSet : TFhirCommonLanguagesEnumList) : Integer;
var
  a : TFhirCommonLanguagesEnum;
begin
  result := 0;
  for a := low(TFhirCommonLanguagesEnum) to high(TFhirCommonLanguagesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCommonLanguagesEnumList(i : Integer) : TFhirCommonLanguagesEnumList;
var
  aLoop : TFhirCommonLanguagesEnum;
begin
  result := [];
  for aLoop := low(TFhirCommonLanguagesEnum) to high(TFhirCommonLanguagesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCompartmentTypeEnumListAsInteger(aSet : TFhirCompartmentTypeEnumList) : Integer;
var
  a : TFhirCompartmentTypeEnum;
begin
  result := 0;
  for a := low(TFhirCompartmentTypeEnum) to high(TFhirCompartmentTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompartmentTypeEnumList(i : Integer) : TFhirCompartmentTypeEnumList;
var
  aLoop : TFhirCompartmentTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirCompartmentTypeEnum) to high(TFhirCompartmentTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCompositionStatusEnumListAsInteger(aSet : TFhirCompositionStatusEnumList) : Integer;
var
  a : TFhirCompositionStatusEnum;
begin
  result := 0;
  for a := low(TFhirCompositionStatusEnum) to high(TFhirCompositionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCompositionStatusEnumList(i : Integer) : TFhirCompositionStatusEnumList;
var
  aLoop : TFhirCompositionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirCompositionStatusEnum) to high(TFhirCompositionStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptMapGroupUnmappedModeEnumListAsInteger(aSet : TFhirConceptMapGroupUnmappedModeEnumList) : Integer;
var
  a : TFhirConceptMapGroupUnmappedModeEnum;
begin
  result := 0;
  for a := low(TFhirConceptMapGroupUnmappedModeEnum) to high(TFhirConceptMapGroupUnmappedModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapGroupUnmappedModeEnumList(i : Integer) : TFhirConceptMapGroupUnmappedModeEnumList;
var
  aLoop : TFhirConceptMapGroupUnmappedModeEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptMapGroupUnmappedModeEnum) to high(TFhirConceptMapGroupUnmappedModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptMapRelationshipEnumListAsInteger(aSet : TFhirConceptMapRelationshipEnumList) : Integer;
var
  a : TFhirConceptMapRelationshipEnum;
begin
  result := 0;
  for a := low(TFhirConceptMapRelationshipEnum) to high(TFhirConceptMapRelationshipEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptMapRelationshipEnumList(i : Integer) : TFhirConceptMapRelationshipEnumList;
var
  aLoop : TFhirConceptMapRelationshipEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptMapRelationshipEnum) to high(TFhirConceptMapRelationshipEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConceptPropertyTypeEnumListAsInteger(aSet : TFhirConceptPropertyTypeEnumList) : Integer;
var
  a : TFhirConceptPropertyTypeEnum;
begin
  result := 0;
  for a := low(TFhirConceptPropertyTypeEnum) to high(TFhirConceptPropertyTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConceptPropertyTypeEnumList(i : Integer) : TFhirConceptPropertyTypeEnumList;
var
  aLoop : TFhirConceptPropertyTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConceptPropertyTypeEnum) to high(TFhirConceptPropertyTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionPreconditionTypeEnumListAsInteger(aSet : TFhirConditionPreconditionTypeEnumList) : Integer;
var
  a : TFhirConditionPreconditionTypeEnum;
begin
  result := 0;
  for a := low(TFhirConditionPreconditionTypeEnum) to high(TFhirConditionPreconditionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionPreconditionTypeEnumList(i : Integer) : TFhirConditionPreconditionTypeEnumList;
var
  aLoop : TFhirConditionPreconditionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionPreconditionTypeEnum) to high(TFhirConditionPreconditionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionQuestionnairePurposeEnumListAsInteger(aSet : TFhirConditionQuestionnairePurposeEnumList) : Integer;
var
  a : TFhirConditionQuestionnairePurposeEnum;
begin
  result := 0;
  for a := low(TFhirConditionQuestionnairePurposeEnum) to high(TFhirConditionQuestionnairePurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionQuestionnairePurposeEnumList(i : Integer) : TFhirConditionQuestionnairePurposeEnumList;
var
  aLoop : TFhirConditionQuestionnairePurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionQuestionnairePurposeEnum) to high(TFhirConditionQuestionnairePurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionalDeleteStatusEnumListAsInteger(aSet : TFhirConditionalDeleteStatusEnumList) : Integer;
var
  a : TFhirConditionalDeleteStatusEnum;
begin
  result := 0;
  for a := low(TFhirConditionalDeleteStatusEnum) to high(TFhirConditionalDeleteStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalDeleteStatusEnumList(i : Integer) : TFhirConditionalDeleteStatusEnumList;
var
  aLoop : TFhirConditionalDeleteStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionalDeleteStatusEnum) to high(TFhirConditionalDeleteStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConditionalReadStatusEnumListAsInteger(aSet : TFhirConditionalReadStatusEnumList) : Integer;
var
  a : TFhirConditionalReadStatusEnum;
begin
  result := 0;
  for a := low(TFhirConditionalReadStatusEnum) to high(TFhirConditionalReadStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConditionalReadStatusEnumList(i : Integer) : TFhirConditionalReadStatusEnumList;
var
  aLoop : TFhirConditionalReadStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirConditionalReadStatusEnum) to high(TFhirConditionalReadStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConformanceExpectationEnumListAsInteger(aSet : TFhirConformanceExpectationEnumList) : Integer;
var
  a : TFhirConformanceExpectationEnum;
begin
  result := 0;
  for a := low(TFhirConformanceExpectationEnum) to high(TFhirConformanceExpectationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConformanceExpectationEnumList(i : Integer) : TFhirConformanceExpectationEnumList;
var
  aLoop : TFhirConformanceExpectationEnum;
begin
  result := [];
  for aLoop := low(TFhirConformanceExpectationEnum) to high(TFhirConformanceExpectationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentDataMeaningEnumListAsInteger(aSet : TFhirConsentDataMeaningEnumList) : Integer;
var
  a : TFhirConsentDataMeaningEnum;
begin
  result := 0;
  for a := low(TFhirConsentDataMeaningEnum) to high(TFhirConsentDataMeaningEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentDataMeaningEnumList(i : Integer) : TFhirConsentDataMeaningEnumList;
var
  aLoop : TFhirConsentDataMeaningEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentDataMeaningEnum) to high(TFhirConsentDataMeaningEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentProvisionTypeEnumListAsInteger(aSet : TFhirConsentProvisionTypeEnumList) : Integer;
var
  a : TFhirConsentProvisionTypeEnum;
begin
  result := 0;
  for a := low(TFhirConsentProvisionTypeEnum) to high(TFhirConsentProvisionTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentProvisionTypeEnumList(i : Integer) : TFhirConsentProvisionTypeEnumList;
var
  aLoop : TFhirConsentProvisionTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentProvisionTypeEnum) to high(TFhirConsentProvisionTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConsentStateEnumListAsInteger(aSet : TFhirConsentStateEnumList) : Integer;
var
  a : TFhirConsentStateEnum;
begin
  result := 0;
  for a := low(TFhirConsentStateEnum) to high(TFhirConsentStateEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConsentStateEnumList(i : Integer) : TFhirConsentStateEnumList;
var
  aLoop : TFhirConsentStateEnum;
begin
  result := [];
  for aLoop := low(TFhirConsentStateEnum) to high(TFhirConsentStateEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirConstraintSeverityEnumListAsInteger(aSet : TFhirConstraintSeverityEnumList) : Integer;
var
  a : TFhirConstraintSeverityEnum;
begin
  result := 0;
  for a := low(TFhirConstraintSeverityEnum) to high(TFhirConstraintSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirConstraintSeverityEnumList(i : Integer) : TFhirConstraintSeverityEnumList;
var
  aLoop : TFhirConstraintSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirConstraintSeverityEnum) to high(TFhirConstraintSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContactPointSystemEnumListAsInteger(aSet : TFhirContactPointSystemEnumList) : Integer;
var
  a : TFhirContactPointSystemEnum;
begin
  result := 0;
  for a := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointSystemEnumList(i : Integer) : TFhirContactPointSystemEnumList;
var
  aLoop : TFhirContactPointSystemEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointSystemEnum) to high(TFhirContactPointSystemEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContactPointUseEnumListAsInteger(aSet : TFhirContactPointUseEnumList) : Integer;
var
  a : TFhirContactPointUseEnum;
begin
  result := 0;
  for a := low(TFhirContactPointUseEnum) to high(TFhirContactPointUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContactPointUseEnumList(i : Integer) : TFhirContactPointUseEnumList;
var
  aLoop : TFhirContactPointUseEnum;
begin
  result := [];
  for aLoop := low(TFhirContactPointUseEnum) to high(TFhirContactPointUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContractResourcePublicationStatusCodesEnumListAsInteger(aSet : TFhirContractResourcePublicationStatusCodesEnumList) : Integer;
var
  a : TFhirContractResourcePublicationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirContractResourcePublicationStatusCodesEnum) to high(TFhirContractResourcePublicationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContractResourcePublicationStatusCodesEnumList(i : Integer) : TFhirContractResourcePublicationStatusCodesEnumList;
var
  aLoop : TFhirContractResourcePublicationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirContractResourcePublicationStatusCodesEnum) to high(TFhirContractResourcePublicationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContractResourceStatusCodesEnumListAsInteger(aSet : TFhirContractResourceStatusCodesEnumList) : Integer;
var
  a : TFhirContractResourceStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirContractResourceStatusCodesEnum) to high(TFhirContractResourceStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContractResourceStatusCodesEnumList(i : Integer) : TFhirContractResourceStatusCodesEnumList;
var
  aLoop : TFhirContractResourceStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirContractResourceStatusCodesEnum) to high(TFhirContractResourceStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirContributorTypeEnumListAsInteger(aSet : TFhirContributorTypeEnumList) : Integer;
var
  a : TFhirContributorTypeEnum;
begin
  result := 0;
  for a := low(TFhirContributorTypeEnum) to high(TFhirContributorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirContributorTypeEnumList(i : Integer) : TFhirContributorTypeEnumList;
var
  aLoop : TFhirContributorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirContributorTypeEnum) to high(TFhirContributorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirCriteriaNotExistsBehaviorEnumListAsInteger(aSet : TFhirCriteriaNotExistsBehaviorEnumList) : Integer;
var
  a : TFhirCriteriaNotExistsBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirCriteriaNotExistsBehaviorEnum) to high(TFhirCriteriaNotExistsBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirCriteriaNotExistsBehaviorEnumList(i : Integer) : TFhirCriteriaNotExistsBehaviorEnumList;
var
  aLoop : TFhirCriteriaNotExistsBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirCriteriaNotExistsBehaviorEnum) to high(TFhirCriteriaNotExistsBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDaysOfWeekEnumListAsInteger(aSet : TFhirDaysOfWeekEnumList) : Integer;
var
  a : TFhirDaysOfWeekEnum;
begin
  result := 0;
  for a := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDaysOfWeekEnumList(i : Integer) : TFhirDaysOfWeekEnumList;
var
  aLoop : TFhirDaysOfWeekEnum;
begin
  result := [];
  for aLoop := low(TFhirDaysOfWeekEnum) to high(TFhirDaysOfWeekEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDetectedIssueSeverityEnumListAsInteger(aSet : TFhirDetectedIssueSeverityEnumList) : Integer;
var
  a : TFhirDetectedIssueSeverityEnum;
begin
  result := 0;
  for a := low(TFhirDetectedIssueSeverityEnum) to high(TFhirDetectedIssueSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDetectedIssueSeverityEnumList(i : Integer) : TFhirDetectedIssueSeverityEnumList;
var
  aLoop : TFhirDetectedIssueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirDetectedIssueSeverityEnum) to high(TFhirDetectedIssueSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDetectedIssueStatusEnumListAsInteger(aSet : TFhirDetectedIssueStatusEnumList) : Integer;
var
  a : TFhirDetectedIssueStatusEnum;
begin
  result := 0;
  for a := low(TFhirDetectedIssueStatusEnum) to high(TFhirDetectedIssueStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDetectedIssueStatusEnumList(i : Integer) : TFhirDetectedIssueStatusEnumList;
var
  aLoop : TFhirDetectedIssueStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDetectedIssueStatusEnum) to high(TFhirDetectedIssueStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceCorrectiveActionScopeEnumListAsInteger(aSet : TFhirDeviceCorrectiveActionScopeEnumList) : Integer;
var
  a : TFhirDeviceCorrectiveActionScopeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceCorrectiveActionScopeEnum) to high(TFhirDeviceCorrectiveActionScopeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceCorrectiveActionScopeEnumList(i : Integer) : TFhirDeviceCorrectiveActionScopeEnumList;
var
  aLoop : TFhirDeviceCorrectiveActionScopeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceCorrectiveActionScopeEnum) to high(TFhirDeviceCorrectiveActionScopeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumListAsInteger(aSet : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList) : Integer;
var
  a : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum) to high(TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList(i : Integer) : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnumList;
var
  aLoop : TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum) to high(TFhirDeviceDefinitionRegulatoryIdentifierTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceDispenseStatusCodesEnumListAsInteger(aSet : TFhirDeviceDispenseStatusCodesEnumList) : Integer;
var
  a : TFhirDeviceDispenseStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirDeviceDispenseStatusCodesEnum) to high(TFhirDeviceDispenseStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceDispenseStatusCodesEnumList(i : Integer) : TFhirDeviceDispenseStatusCodesEnumList;
var
  aLoop : TFhirDeviceDispenseStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceDispenseStatusCodesEnum) to high(TFhirDeviceDispenseStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCalibrationStateEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationStateEnumList) : Integer;
var
  a : TFhirDeviceMetricCalibrationStateEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCalibrationStateEnum) to high(TFhirDeviceMetricCalibrationStateEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCalibrationStateEnumList(i : Integer) : TFhirDeviceMetricCalibrationStateEnumList;
var
  aLoop : TFhirDeviceMetricCalibrationStateEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCalibrationStateEnum) to high(TFhirDeviceMetricCalibrationStateEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCalibrationTypeEnumListAsInteger(aSet : TFhirDeviceMetricCalibrationTypeEnumList) : Integer;
var
  a : TFhirDeviceMetricCalibrationTypeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCalibrationTypeEnum) to high(TFhirDeviceMetricCalibrationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCalibrationTypeEnumList(i : Integer) : TFhirDeviceMetricCalibrationTypeEnumList;
var
  aLoop : TFhirDeviceMetricCalibrationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCalibrationTypeEnum) to high(TFhirDeviceMetricCalibrationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricCategoryEnumListAsInteger(aSet : TFhirDeviceMetricCategoryEnumList) : Integer;
var
  a : TFhirDeviceMetricCategoryEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricCategoryEnum) to high(TFhirDeviceMetricCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricCategoryEnumList(i : Integer) : TFhirDeviceMetricCategoryEnumList;
var
  aLoop : TFhirDeviceMetricCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricCategoryEnum) to high(TFhirDeviceMetricCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricColorEnumListAsInteger(aSet : TFhirDeviceMetricColorEnumList) : Integer;
var
  a : TFhirDeviceMetricColorEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricColorEnum) to high(TFhirDeviceMetricColorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricColorEnumList(i : Integer) : TFhirDeviceMetricColorEnumList;
var
  aLoop : TFhirDeviceMetricColorEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricColorEnum) to high(TFhirDeviceMetricColorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceMetricOperationalStatusEnumListAsInteger(aSet : TFhirDeviceMetricOperationalStatusEnumList) : Integer;
var
  a : TFhirDeviceMetricOperationalStatusEnum;
begin
  result := 0;
  for a := low(TFhirDeviceMetricOperationalStatusEnum) to high(TFhirDeviceMetricOperationalStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceMetricOperationalStatusEnumList(i : Integer) : TFhirDeviceMetricOperationalStatusEnumList;
var
  aLoop : TFhirDeviceMetricOperationalStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceMetricOperationalStatusEnum) to high(TFhirDeviceMetricOperationalStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceNameTypeEnumListAsInteger(aSet : TFhirDeviceNameTypeEnumList) : Integer;
var
  a : TFhirDeviceNameTypeEnum;
begin
  result := 0;
  for a := low(TFhirDeviceNameTypeEnum) to high(TFhirDeviceNameTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceNameTypeEnumList(i : Integer) : TFhirDeviceNameTypeEnumList;
var
  aLoop : TFhirDeviceNameTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceNameTypeEnum) to high(TFhirDeviceNameTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceProductionIdentifierInUDIEnumListAsInteger(aSet : TFhirDeviceProductionIdentifierInUDIEnumList) : Integer;
var
  a : TFhirDeviceProductionIdentifierInUDIEnum;
begin
  result := 0;
  for a := low(TFhirDeviceProductionIdentifierInUDIEnum) to high(TFhirDeviceProductionIdentifierInUDIEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceProductionIdentifierInUDIEnumList(i : Integer) : TFhirDeviceProductionIdentifierInUDIEnumList;
var
  aLoop : TFhirDeviceProductionIdentifierInUDIEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceProductionIdentifierInUDIEnum) to high(TFhirDeviceProductionIdentifierInUDIEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDeviceUsageStatusEnumListAsInteger(aSet : TFhirDeviceUsageStatusEnumList) : Integer;
var
  a : TFhirDeviceUsageStatusEnum;
begin
  result := 0;
  for a := low(TFhirDeviceUsageStatusEnum) to high(TFhirDeviceUsageStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDeviceUsageStatusEnumList(i : Integer) : TFhirDeviceUsageStatusEnumList;
var
  aLoop : TFhirDeviceUsageStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDeviceUsageStatusEnum) to high(TFhirDeviceUsageStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDiagnosticReportStatusEnumListAsInteger(aSet : TFhirDiagnosticReportStatusEnumList) : Integer;
var
  a : TFhirDiagnosticReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirDiagnosticReportStatusEnum) to high(TFhirDiagnosticReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiagnosticReportStatusEnumList(i : Integer) : TFhirDiagnosticReportStatusEnumList;
var
  aLoop : TFhirDiagnosticReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDiagnosticReportStatusEnum) to high(TFhirDiagnosticReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDiscriminatorTypeEnumListAsInteger(aSet : TFhirDiscriminatorTypeEnumList) : Integer;
var
  a : TFhirDiscriminatorTypeEnum;
begin
  result := 0;
  for a := low(TFhirDiscriminatorTypeEnum) to high(TFhirDiscriminatorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDiscriminatorTypeEnumList(i : Integer) : TFhirDiscriminatorTypeEnumList;
var
  aLoop : TFhirDiscriminatorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirDiscriminatorTypeEnum) to high(TFhirDiscriminatorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentModeEnumListAsInteger(aSet : TFhirDocumentModeEnumList) : Integer;
var
  a : TFhirDocumentModeEnum;
begin
  result := 0;
  for a := low(TFhirDocumentModeEnum) to high(TFhirDocumentModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentModeEnumList(i : Integer) : TFhirDocumentModeEnumList;
var
  aLoop : TFhirDocumentModeEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentModeEnum) to high(TFhirDocumentModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirDocumentReferenceStatusEnumListAsInteger(aSet : TFhirDocumentReferenceStatusEnumList) : Integer;
var
  a : TFhirDocumentReferenceStatusEnum;
begin
  result := 0;
  for a := low(TFhirDocumentReferenceStatusEnum) to high(TFhirDocumentReferenceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirDocumentReferenceStatusEnumList(i : Integer) : TFhirDocumentReferenceStatusEnumList;
var
  aLoop : TFhirDocumentReferenceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirDocumentReferenceStatusEnum) to high(TFhirDocumentReferenceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEligibilityOutcomeEnumListAsInteger(aSet : TFhirEligibilityOutcomeEnumList) : Integer;
var
  a : TFhirEligibilityOutcomeEnum;
begin
  result := 0;
  for a := low(TFhirEligibilityOutcomeEnum) to high(TFhirEligibilityOutcomeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEligibilityOutcomeEnumList(i : Integer) : TFhirEligibilityOutcomeEnumList;
var
  aLoop : TFhirEligibilityOutcomeEnum;
begin
  result := [];
  for aLoop := low(TFhirEligibilityOutcomeEnum) to high(TFhirEligibilityOutcomeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEligibilityRequestPurposeEnumListAsInteger(aSet : TFhirEligibilityRequestPurposeEnumList) : Integer;
var
  a : TFhirEligibilityRequestPurposeEnum;
begin
  result := 0;
  for a := low(TFhirEligibilityRequestPurposeEnum) to high(TFhirEligibilityRequestPurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEligibilityRequestPurposeEnumList(i : Integer) : TFhirEligibilityRequestPurposeEnumList;
var
  aLoop : TFhirEligibilityRequestPurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirEligibilityRequestPurposeEnum) to high(TFhirEligibilityRequestPurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEligibilityResponsePurposeEnumListAsInteger(aSet : TFhirEligibilityResponsePurposeEnumList) : Integer;
var
  a : TFhirEligibilityResponsePurposeEnum;
begin
  result := 0;
  for a := low(TFhirEligibilityResponsePurposeEnum) to high(TFhirEligibilityResponsePurposeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEligibilityResponsePurposeEnumList(i : Integer) : TFhirEligibilityResponsePurposeEnumList;
var
  aLoop : TFhirEligibilityResponsePurposeEnum;
begin
  result := [];
  for aLoop := low(TFhirEligibilityResponsePurposeEnum) to high(TFhirEligibilityResponsePurposeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEnableWhenBehaviorEnumListAsInteger(aSet : TFhirEnableWhenBehaviorEnumList) : Integer;
var
  a : TFhirEnableWhenBehaviorEnum;
begin
  result := 0;
  for a := low(TFhirEnableWhenBehaviorEnum) to high(TFhirEnableWhenBehaviorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEnableWhenBehaviorEnumList(i : Integer) : TFhirEnableWhenBehaviorEnumList;
var
  aLoop : TFhirEnableWhenBehaviorEnum;
begin
  result := [];
  for aLoop := low(TFhirEnableWhenBehaviorEnum) to high(TFhirEnableWhenBehaviorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEncounterLocationStatusEnumListAsInteger(aSet : TFhirEncounterLocationStatusEnumList) : Integer;
var
  a : TFhirEncounterLocationStatusEnum;
begin
  result := 0;
  for a := low(TFhirEncounterLocationStatusEnum) to high(TFhirEncounterLocationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterLocationStatusEnumList(i : Integer) : TFhirEncounterLocationStatusEnumList;
var
  aLoop : TFhirEncounterLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterLocationStatusEnum) to high(TFhirEncounterLocationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEncounterStatusEnumListAsInteger(aSet : TFhirEncounterStatusEnumList) : Integer;
var
  a : TFhirEncounterStatusEnum;
begin
  result := 0;
  for a := low(TFhirEncounterStatusEnum) to high(TFhirEncounterStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEncounterStatusEnumList(i : Integer) : TFhirEncounterStatusEnumList;
var
  aLoop : TFhirEncounterStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEncounterStatusEnum) to high(TFhirEncounterStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEndpointStatusEnumListAsInteger(aSet : TFhirEndpointStatusEnumList) : Integer;
var
  a : TFhirEndpointStatusEnum;
begin
  result := 0;
  for a := low(TFhirEndpointStatusEnum) to high(TFhirEndpointStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEndpointStatusEnumList(i : Integer) : TFhirEndpointStatusEnumList;
var
  aLoop : TFhirEndpointStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEndpointStatusEnum) to high(TFhirEndpointStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEnrollmentOutcomeEnumListAsInteger(aSet : TFhirEnrollmentOutcomeEnumList) : Integer;
var
  a : TFhirEnrollmentOutcomeEnum;
begin
  result := 0;
  for a := low(TFhirEnrollmentOutcomeEnum) to high(TFhirEnrollmentOutcomeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEnrollmentOutcomeEnumList(i : Integer) : TFhirEnrollmentOutcomeEnumList;
var
  aLoop : TFhirEnrollmentOutcomeEnum;
begin
  result := [];
  for aLoop := low(TFhirEnrollmentOutcomeEnum) to high(TFhirEnrollmentOutcomeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEpisodeOfCareStatusEnumListAsInteger(aSet : TFhirEpisodeOfCareStatusEnumList) : Integer;
var
  a : TFhirEpisodeOfCareStatusEnum;
begin
  result := 0;
  for a := low(TFhirEpisodeOfCareStatusEnum) to high(TFhirEpisodeOfCareStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEpisodeOfCareStatusEnumList(i : Integer) : TFhirEpisodeOfCareStatusEnumList;
var
  aLoop : TFhirEpisodeOfCareStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEpisodeOfCareStatusEnum) to high(TFhirEpisodeOfCareStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventCapabilityModeEnumListAsInteger(aSet : TFhirEventCapabilityModeEnumList) : Integer;
var
  a : TFhirEventCapabilityModeEnum;
begin
  result := 0;
  for a := low(TFhirEventCapabilityModeEnum) to high(TFhirEventCapabilityModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventCapabilityModeEnumList(i : Integer) : TFhirEventCapabilityModeEnumList;
var
  aLoop : TFhirEventCapabilityModeEnum;
begin
  result := [];
  for aLoop := low(TFhirEventCapabilityModeEnum) to high(TFhirEventCapabilityModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventStatusEnumListAsInteger(aSet : TFhirEventStatusEnumList) : Integer;
var
  a : TFhirEventStatusEnum;
begin
  result := 0;
  for a := low(TFhirEventStatusEnum) to high(TFhirEventStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventStatusEnumList(i : Integer) : TFhirEventStatusEnumList;
var
  aLoop : TFhirEventStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirEventStatusEnum) to high(TFhirEventStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEventTimingEnumListAsInteger(aSet : TFhirEventTimingEnumList) : Integer;
var
  a : TFhirEventTimingEnum;
begin
  result := 0;
  for a := low(TFhirEventTimingEnum) to high(TFhirEventTimingEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEventTimingEnumList(i : Integer) : TFhirEventTimingEnumList;
var
  aLoop : TFhirEventTimingEnum;
begin
  result := [];
  for aLoop := low(TFhirEventTimingEnum) to high(TFhirEventTimingEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirEvidenceVariableHandlingEnumListAsInteger(aSet : TFhirEvidenceVariableHandlingEnumList) : Integer;
var
  a : TFhirEvidenceVariableHandlingEnum;
begin
  result := 0;
  for a := low(TFhirEvidenceVariableHandlingEnum) to high(TFhirEvidenceVariableHandlingEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirEvidenceVariableHandlingEnumList(i : Integer) : TFhirEvidenceVariableHandlingEnumList;
var
  aLoop : TFhirEvidenceVariableHandlingEnum;
begin
  result := [];
  for aLoop := low(TFhirEvidenceVariableHandlingEnum) to high(TFhirEvidenceVariableHandlingEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExampleScenarioActorTypeEnumListAsInteger(aSet : TFhirExampleScenarioActorTypeEnumList) : Integer;
var
  a : TFhirExampleScenarioActorTypeEnum;
begin
  result := 0;
  for a := low(TFhirExampleScenarioActorTypeEnum) to high(TFhirExampleScenarioActorTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExampleScenarioActorTypeEnumList(i : Integer) : TFhirExampleScenarioActorTypeEnumList;
var
  aLoop : TFhirExampleScenarioActorTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirExampleScenarioActorTypeEnum) to high(TFhirExampleScenarioActorTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExplanationOfBenefitStatusEnumListAsInteger(aSet : TFhirExplanationOfBenefitStatusEnumList) : Integer;
var
  a : TFhirExplanationOfBenefitStatusEnum;
begin
  result := 0;
  for a := low(TFhirExplanationOfBenefitStatusEnum) to high(TFhirExplanationOfBenefitStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExplanationOfBenefitStatusEnumList(i : Integer) : TFhirExplanationOfBenefitStatusEnumList;
var
  aLoop : TFhirExplanationOfBenefitStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirExplanationOfBenefitStatusEnum) to high(TFhirExplanationOfBenefitStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirExtensionContextTypeEnumListAsInteger(aSet : TFhirExtensionContextTypeEnumList) : Integer;
var
  a : TFhirExtensionContextTypeEnum;
begin
  result := 0;
  for a := low(TFhirExtensionContextTypeEnum) to high(TFhirExtensionContextTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirExtensionContextTypeEnumList(i : Integer) : TFhirExtensionContextTypeEnumList;
var
  aLoop : TFhirExtensionContextTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirExtensionContextTypeEnum) to high(TFhirExtensionContextTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRDeviceStatusEnumListAsInteger(aSet : TFhirFHIRDeviceStatusEnumList) : Integer;
var
  a : TFhirFHIRDeviceStatusEnum;
begin
  result := 0;
  for a := low(TFhirFHIRDeviceStatusEnum) to high(TFhirFHIRDeviceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRDeviceStatusEnumList(i : Integer) : TFhirFHIRDeviceStatusEnumList;
var
  aLoop : TFhirFHIRDeviceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRDeviceStatusEnum) to high(TFhirFHIRDeviceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRSubstanceStatusEnumListAsInteger(aSet : TFhirFHIRSubstanceStatusEnumList) : Integer;
var
  a : TFhirFHIRSubstanceStatusEnum;
begin
  result := 0;
  for a := low(TFhirFHIRSubstanceStatusEnum) to high(TFhirFHIRSubstanceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRSubstanceStatusEnumList(i : Integer) : TFhirFHIRSubstanceStatusEnumList;
var
  aLoop : TFhirFHIRSubstanceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRSubstanceStatusEnum) to high(TFhirFHIRSubstanceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRTypesEnumListAsInteger(aSet : TFhirFHIRTypesEnumList) : Integer;
var
  a : TFhirFHIRTypesEnum;
begin
  result := 0;
  for a := low(TFhirFHIRTypesEnum) to high(TFhirFHIRTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRTypesEnumList(i : Integer) : TFhirFHIRTypesEnumList;
var
  aLoop : TFhirFHIRTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRTypesEnum) to high(TFhirFHIRTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFHIRVersionEnumListAsInteger(aSet : TFhirFHIRVersionEnumList) : Integer;
var
  a : TFhirFHIRVersionEnum;
begin
  result := 0;
  for a := low(TFhirFHIRVersionEnum) to high(TFhirFHIRVersionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFHIRVersionEnumList(i : Integer) : TFhirFHIRVersionEnumList;
var
  aLoop : TFhirFHIRVersionEnum;
begin
  result := [];
  for aLoop := low(TFhirFHIRVersionEnum) to high(TFhirFHIRVersionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFamilyHistoryStatusEnumListAsInteger(aSet : TFhirFamilyHistoryStatusEnumList) : Integer;
var
  a : TFhirFamilyHistoryStatusEnum;
begin
  result := 0;
  for a := low(TFhirFamilyHistoryStatusEnum) to high(TFhirFamilyHistoryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFamilyHistoryStatusEnumList(i : Integer) : TFhirFamilyHistoryStatusEnumList;
var
  aLoop : TFhirFamilyHistoryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFamilyHistoryStatusEnum) to high(TFhirFamilyHistoryStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFilterOperatorEnumListAsInteger(aSet : TFhirFilterOperatorEnumList) : Integer;
var
  a : TFhirFilterOperatorEnum;
begin
  result := 0;
  for a := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFilterOperatorEnumList(i : Integer) : TFhirFilterOperatorEnumList;
var
  aLoop : TFhirFilterOperatorEnum;
begin
  result := [];
  for aLoop := low(TFhirFilterOperatorEnum) to high(TFhirFilterOperatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFinancialResourceStatusCodesEnumListAsInteger(aSet : TFhirFinancialResourceStatusCodesEnumList) : Integer;
var
  a : TFhirFinancialResourceStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirFinancialResourceStatusCodesEnum) to high(TFhirFinancialResourceStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFinancialResourceStatusCodesEnumList(i : Integer) : TFhirFinancialResourceStatusCodesEnumList;
var
  aLoop : TFhirFinancialResourceStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirFinancialResourceStatusCodesEnum) to high(TFhirFinancialResourceStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFlagStatusEnumListAsInteger(aSet : TFhirFlagStatusEnumList) : Integer;
var
  a : TFhirFlagStatusEnum;
begin
  result := 0;
  for a := low(TFhirFlagStatusEnum) to high(TFhirFlagStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFlagStatusEnumList(i : Integer) : TFhirFlagStatusEnumList;
var
  aLoop : TFhirFlagStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirFlagStatusEnum) to high(TFhirFlagStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirFormularyItemStatusCodesEnumListAsInteger(aSet : TFhirFormularyItemStatusCodesEnumList) : Integer;
var
  a : TFhirFormularyItemStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirFormularyItemStatusCodesEnum) to high(TFhirFormularyItemStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirFormularyItemStatusCodesEnumList(i : Integer) : TFhirFormularyItemStatusCodesEnumList;
var
  aLoop : TFhirFormularyItemStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirFormularyItemStatusCodesEnum) to high(TFhirFormularyItemStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGoalLifecycleStatusEnumListAsInteger(aSet : TFhirGoalLifecycleStatusEnumList) : Integer;
var
  a : TFhirGoalLifecycleStatusEnum;
begin
  result := 0;
  for a := low(TFhirGoalLifecycleStatusEnum) to high(TFhirGoalLifecycleStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGoalLifecycleStatusEnumList(i : Integer) : TFhirGoalLifecycleStatusEnumList;
var
  aLoop : TFhirGoalLifecycleStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirGoalLifecycleStatusEnum) to high(TFhirGoalLifecycleStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGraphCompartmentRuleEnumListAsInteger(aSet : TFhirGraphCompartmentRuleEnumList) : Integer;
var
  a : TFhirGraphCompartmentRuleEnum;
begin
  result := 0;
  for a := low(TFhirGraphCompartmentRuleEnum) to high(TFhirGraphCompartmentRuleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGraphCompartmentRuleEnumList(i : Integer) : TFhirGraphCompartmentRuleEnumList;
var
  aLoop : TFhirGraphCompartmentRuleEnum;
begin
  result := [];
  for aLoop := low(TFhirGraphCompartmentRuleEnum) to high(TFhirGraphCompartmentRuleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGraphCompartmentUseEnumListAsInteger(aSet : TFhirGraphCompartmentUseEnumList) : Integer;
var
  a : TFhirGraphCompartmentUseEnum;
begin
  result := 0;
  for a := low(TFhirGraphCompartmentUseEnum) to high(TFhirGraphCompartmentUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGraphCompartmentUseEnumList(i : Integer) : TFhirGraphCompartmentUseEnumList;
var
  aLoop : TFhirGraphCompartmentUseEnum;
begin
  result := [];
  for aLoop := low(TFhirGraphCompartmentUseEnum) to high(TFhirGraphCompartmentUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGroupMembershipBasisEnumListAsInteger(aSet : TFhirGroupMembershipBasisEnumList) : Integer;
var
  a : TFhirGroupMembershipBasisEnum;
begin
  result := 0;
  for a := low(TFhirGroupMembershipBasisEnum) to high(TFhirGroupMembershipBasisEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupMembershipBasisEnumList(i : Integer) : TFhirGroupMembershipBasisEnumList;
var
  aLoop : TFhirGroupMembershipBasisEnum;
begin
  result := [];
  for aLoop := low(TFhirGroupMembershipBasisEnum) to high(TFhirGroupMembershipBasisEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGroupTypeEnumListAsInteger(aSet : TFhirGroupTypeEnumList) : Integer;
var
  a : TFhirGroupTypeEnum;
begin
  result := 0;
  for a := low(TFhirGroupTypeEnum) to high(TFhirGroupTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGroupTypeEnumList(i : Integer) : TFhirGroupTypeEnumList;
var
  aLoop : TFhirGroupTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirGroupTypeEnum) to high(TFhirGroupTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGuidanceResponseStatusEnumListAsInteger(aSet : TFhirGuidanceResponseStatusEnumList) : Integer;
var
  a : TFhirGuidanceResponseStatusEnum;
begin
  result := 0;
  for a := low(TFhirGuidanceResponseStatusEnum) to high(TFhirGuidanceResponseStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidanceResponseStatusEnumList(i : Integer) : TFhirGuidanceResponseStatusEnumList;
var
  aLoop : TFhirGuidanceResponseStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirGuidanceResponseStatusEnum) to high(TFhirGuidanceResponseStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirGuidePageGenerationEnumListAsInteger(aSet : TFhirGuidePageGenerationEnumList) : Integer;
var
  a : TFhirGuidePageGenerationEnum;
begin
  result := 0;
  for a := low(TFhirGuidePageGenerationEnum) to high(TFhirGuidePageGenerationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirGuidePageGenerationEnumList(i : Integer) : TFhirGuidePageGenerationEnumList;
var
  aLoop : TFhirGuidePageGenerationEnum;
begin
  result := [];
  for aLoop := low(TFhirGuidePageGenerationEnum) to high(TFhirGuidePageGenerationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirHTTPVerbEnumListAsInteger(aSet : TFhirHTTPVerbEnumList) : Integer;
var
  a : TFhirHTTPVerbEnum;
begin
  result := 0;
  for a := low(TFhirHTTPVerbEnum) to high(TFhirHTTPVerbEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirHTTPVerbEnumList(i : Integer) : TFhirHTTPVerbEnumList;
var
  aLoop : TFhirHTTPVerbEnum;
begin
  result := [];
  for aLoop := low(TFhirHTTPVerbEnum) to high(TFhirHTTPVerbEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIdentifierUseEnumListAsInteger(aSet : TFhirIdentifierUseEnumList) : Integer;
var
  a : TFhirIdentifierUseEnum;
begin
  result := 0;
  for a := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentifierUseEnumList(i : Integer) : TFhirIdentifierUseEnumList;
var
  aLoop : TFhirIdentifierUseEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentifierUseEnum) to high(TFhirIdentifierUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIdentityAssuranceLevelEnumListAsInteger(aSet : TFhirIdentityAssuranceLevelEnumList) : Integer;
var
  a : TFhirIdentityAssuranceLevelEnum;
begin
  result := 0;
  for a := low(TFhirIdentityAssuranceLevelEnum) to high(TFhirIdentityAssuranceLevelEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIdentityAssuranceLevelEnumList(i : Integer) : TFhirIdentityAssuranceLevelEnumList;
var
  aLoop : TFhirIdentityAssuranceLevelEnum;
begin
  result := [];
  for aLoop := low(TFhirIdentityAssuranceLevelEnum) to high(TFhirIdentityAssuranceLevelEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImagingSelection2DGraphicTypeEnumListAsInteger(aSet : TFhirImagingSelection2DGraphicTypeEnumList) : Integer;
var
  a : TFhirImagingSelection2DGraphicTypeEnum;
begin
  result := 0;
  for a := low(TFhirImagingSelection2DGraphicTypeEnum) to high(TFhirImagingSelection2DGraphicTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingSelection2DGraphicTypeEnumList(i : Integer) : TFhirImagingSelection2DGraphicTypeEnumList;
var
  aLoop : TFhirImagingSelection2DGraphicTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirImagingSelection2DGraphicTypeEnum) to high(TFhirImagingSelection2DGraphicTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImagingSelection3DGraphicTypeEnumListAsInteger(aSet : TFhirImagingSelection3DGraphicTypeEnumList) : Integer;
var
  a : TFhirImagingSelection3DGraphicTypeEnum;
begin
  result := 0;
  for a := low(TFhirImagingSelection3DGraphicTypeEnum) to high(TFhirImagingSelection3DGraphicTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingSelection3DGraphicTypeEnumList(i : Integer) : TFhirImagingSelection3DGraphicTypeEnumList;
var
  aLoop : TFhirImagingSelection3DGraphicTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirImagingSelection3DGraphicTypeEnum) to high(TFhirImagingSelection3DGraphicTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImagingSelectionStatusEnumListAsInteger(aSet : TFhirImagingSelectionStatusEnumList) : Integer;
var
  a : TFhirImagingSelectionStatusEnum;
begin
  result := 0;
  for a := low(TFhirImagingSelectionStatusEnum) to high(TFhirImagingSelectionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingSelectionStatusEnumList(i : Integer) : TFhirImagingSelectionStatusEnumList;
var
  aLoop : TFhirImagingSelectionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirImagingSelectionStatusEnum) to high(TFhirImagingSelectionStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImagingStudyStatusEnumListAsInteger(aSet : TFhirImagingStudyStatusEnumList) : Integer;
var
  a : TFhirImagingStudyStatusEnum;
begin
  result := 0;
  for a := low(TFhirImagingStudyStatusEnum) to high(TFhirImagingStudyStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImagingStudyStatusEnumList(i : Integer) : TFhirImagingStudyStatusEnumList;
var
  aLoop : TFhirImagingStudyStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirImagingStudyStatusEnum) to high(TFhirImagingStudyStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImmunizationEvaluationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationEvaluationStatusCodesEnumList) : Integer;
var
  a : TFhirImmunizationEvaluationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirImmunizationEvaluationStatusCodesEnum) to high(TFhirImmunizationEvaluationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImmunizationEvaluationStatusCodesEnumList(i : Integer) : TFhirImmunizationEvaluationStatusCodesEnumList;
var
  aLoop : TFhirImmunizationEvaluationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirImmunizationEvaluationStatusCodesEnum) to high(TFhirImmunizationEvaluationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirImmunizationStatusCodesEnumListAsInteger(aSet : TFhirImmunizationStatusCodesEnumList) : Integer;
var
  a : TFhirImmunizationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirImmunizationStatusCodesEnum) to high(TFhirImmunizationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirImmunizationStatusCodesEnumList(i : Integer) : TFhirImmunizationStatusCodesEnumList;
var
  aLoop : TFhirImmunizationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirImmunizationStatusCodesEnum) to high(TFhirImmunizationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIngredientManufacturerRoleEnumListAsInteger(aSet : TFhirIngredientManufacturerRoleEnumList) : Integer;
var
  a : TFhirIngredientManufacturerRoleEnum;
begin
  result := 0;
  for a := low(TFhirIngredientManufacturerRoleEnum) to high(TFhirIngredientManufacturerRoleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIngredientManufacturerRoleEnumList(i : Integer) : TFhirIngredientManufacturerRoleEnumList;
var
  aLoop : TFhirIngredientManufacturerRoleEnum;
begin
  result := [];
  for aLoop := low(TFhirIngredientManufacturerRoleEnum) to high(TFhirIngredientManufacturerRoleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInteractionTriggerEnumListAsInteger(aSet : TFhirInteractionTriggerEnumList) : Integer;
var
  a : TFhirInteractionTriggerEnum;
begin
  result := 0;
  for a := low(TFhirInteractionTriggerEnum) to high(TFhirInteractionTriggerEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInteractionTriggerEnumList(i : Integer) : TFhirInteractionTriggerEnumList;
var
  aLoop : TFhirInteractionTriggerEnum;
begin
  result := [];
  for aLoop := low(TFhirInteractionTriggerEnum) to high(TFhirInteractionTriggerEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInventoryCountTypeEnumListAsInteger(aSet : TFhirInventoryCountTypeEnumList) : Integer;
var
  a : TFhirInventoryCountTypeEnum;
begin
  result := 0;
  for a := low(TFhirInventoryCountTypeEnum) to high(TFhirInventoryCountTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInventoryCountTypeEnumList(i : Integer) : TFhirInventoryCountTypeEnumList;
var
  aLoop : TFhirInventoryCountTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirInventoryCountTypeEnum) to high(TFhirInventoryCountTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInventoryReportStatusEnumListAsInteger(aSet : TFhirInventoryReportStatusEnumList) : Integer;
var
  a : TFhirInventoryReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirInventoryReportStatusEnum) to high(TFhirInventoryReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInventoryReportStatusEnumList(i : Integer) : TFhirInventoryReportStatusEnumList;
var
  aLoop : TFhirInventoryReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirInventoryReportStatusEnum) to high(TFhirInventoryReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirInvoiceStatusEnumListAsInteger(aSet : TFhirInvoiceStatusEnumList) : Integer;
var
  a : TFhirInvoiceStatusEnum;
begin
  result := 0;
  for a := low(TFhirInvoiceStatusEnum) to high(TFhirInvoiceStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirInvoiceStatusEnumList(i : Integer) : TFhirInvoiceStatusEnumList;
var
  aLoop : TFhirInvoiceStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirInvoiceStatusEnum) to high(TFhirInvoiceStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIssueSeverityEnumListAsInteger(aSet : TFhirIssueSeverityEnumList) : Integer;
var
  a : TFhirIssueSeverityEnum;
begin
  result := 0;
  for a := low(TFhirIssueSeverityEnum) to high(TFhirIssueSeverityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueSeverityEnumList(i : Integer) : TFhirIssueSeverityEnumList;
var
  aLoop : TFhirIssueSeverityEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueSeverityEnum) to high(TFhirIssueSeverityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirIssueTypeEnumListAsInteger(aSet : TFhirIssueTypeEnumList) : Integer;
var
  a : TFhirIssueTypeEnum;
begin
  result := 0;
  for a := low(TFhirIssueTypeEnum) to high(TFhirIssueTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirIssueTypeEnumList(i : Integer) : TFhirIssueTypeEnumList;
var
  aLoop : TFhirIssueTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirIssueTypeEnum) to high(TFhirIssueTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirKindEnumListAsInteger(aSet : TFhirKindEnumList) : Integer;
var
  a : TFhirKindEnum;
begin
  result := 0;
  for a := low(TFhirKindEnum) to high(TFhirKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirKindEnumList(i : Integer) : TFhirKindEnumList;
var
  aLoop : TFhirKindEnum;
begin
  result := [];
  for aLoop := low(TFhirKindEnum) to high(TFhirKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLinkRelationTypesEnumListAsInteger(aSet : TFhirLinkRelationTypesEnumList) : Integer;
var
  a : TFhirLinkRelationTypesEnum;
begin
  result := 0;
  for a := low(TFhirLinkRelationTypesEnum) to high(TFhirLinkRelationTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkRelationTypesEnumList(i : Integer) : TFhirLinkRelationTypesEnumList;
var
  aLoop : TFhirLinkRelationTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkRelationTypesEnum) to high(TFhirLinkRelationTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLinkTypeEnumListAsInteger(aSet : TFhirLinkTypeEnumList) : Integer;
var
  a : TFhirLinkTypeEnum;
begin
  result := 0;
  for a := low(TFhirLinkTypeEnum) to high(TFhirLinkTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkTypeEnumList(i : Integer) : TFhirLinkTypeEnumList;
var
  aLoop : TFhirLinkTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkTypeEnum) to high(TFhirLinkTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLinkageTypeEnumListAsInteger(aSet : TFhirLinkageTypeEnumList) : Integer;
var
  a : TFhirLinkageTypeEnum;
begin
  result := 0;
  for a := low(TFhirLinkageTypeEnum) to high(TFhirLinkageTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLinkageTypeEnumList(i : Integer) : TFhirLinkageTypeEnumList;
var
  aLoop : TFhirLinkageTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirLinkageTypeEnum) to high(TFhirLinkageTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirListModeEnumListAsInteger(aSet : TFhirListModeEnumList) : Integer;
var
  a : TFhirListModeEnum;
begin
  result := 0;
  for a := low(TFhirListModeEnum) to high(TFhirListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListModeEnumList(i : Integer) : TFhirListModeEnumList;
var
  aLoop : TFhirListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirListModeEnum) to high(TFhirListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirListStatusEnumListAsInteger(aSet : TFhirListStatusEnumList) : Integer;
var
  a : TFhirListStatusEnum;
begin
  result := 0;
  for a := low(TFhirListStatusEnum) to high(TFhirListStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirListStatusEnumList(i : Integer) : TFhirListStatusEnumList;
var
  aLoop : TFhirListStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirListStatusEnum) to high(TFhirListStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLocationModeEnumListAsInteger(aSet : TFhirLocationModeEnumList) : Integer;
var
  a : TFhirLocationModeEnum;
begin
  result := 0;
  for a := low(TFhirLocationModeEnum) to high(TFhirLocationModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationModeEnumList(i : Integer) : TFhirLocationModeEnumList;
var
  aLoop : TFhirLocationModeEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationModeEnum) to high(TFhirLocationModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirLocationStatusEnumListAsInteger(aSet : TFhirLocationStatusEnumList) : Integer;
var
  a : TFhirLocationStatusEnum;
begin
  result := 0;
  for a := low(TFhirLocationStatusEnum) to high(TFhirLocationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirLocationStatusEnumList(i : Integer) : TFhirLocationStatusEnumList;
var
  aLoop : TFhirLocationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirLocationStatusEnum) to high(TFhirLocationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMeasureReportStatusEnumListAsInteger(aSet : TFhirMeasureReportStatusEnumList) : Integer;
var
  a : TFhirMeasureReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirMeasureReportStatusEnum) to high(TFhirMeasureReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasureReportStatusEnumList(i : Integer) : TFhirMeasureReportStatusEnumList;
var
  aLoop : TFhirMeasureReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMeasureReportStatusEnum) to high(TFhirMeasureReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMeasureReportTypeEnumListAsInteger(aSet : TFhirMeasureReportTypeEnumList) : Integer;
var
  a : TFhirMeasureReportTypeEnum;
begin
  result := 0;
  for a := low(TFhirMeasureReportTypeEnum) to high(TFhirMeasureReportTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMeasureReportTypeEnumList(i : Integer) : TFhirMeasureReportTypeEnumList;
var
  aLoop : TFhirMeasureReportTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirMeasureReportTypeEnum) to high(TFhirMeasureReportTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationAdministrationStatusCodesEnumListAsInteger(aSet : TFhirMedicationAdministrationStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationAdministrationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationAdministrationStatusCodesEnum) to high(TFhirMedicationAdministrationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationAdministrationStatusCodesEnumList(i : Integer) : TFhirMedicationAdministrationStatusCodesEnumList;
var
  aLoop : TFhirMedicationAdministrationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationAdministrationStatusCodesEnum) to high(TFhirMedicationAdministrationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationDispenseStatusCodesEnumListAsInteger(aSet : TFhirMedicationDispenseStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationDispenseStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationDispenseStatusCodesEnum) to high(TFhirMedicationDispenseStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationDispenseStatusCodesEnumList(i : Integer) : TFhirMedicationDispenseStatusCodesEnumList;
var
  aLoop : TFhirMedicationDispenseStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationDispenseStatusCodesEnum) to high(TFhirMedicationDispenseStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationKnowledgeStatusCodesEnumListAsInteger(aSet : TFhirMedicationKnowledgeStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationKnowledgeStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationKnowledgeStatusCodesEnum) to high(TFhirMedicationKnowledgeStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationKnowledgeStatusCodesEnumList(i : Integer) : TFhirMedicationKnowledgeStatusCodesEnumList;
var
  aLoop : TFhirMedicationKnowledgeStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationKnowledgeStatusCodesEnum) to high(TFhirMedicationKnowledgeStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationRequestIntentEnumListAsInteger(aSet : TFhirMedicationRequestIntentEnumList) : Integer;
var
  a : TFhirMedicationRequestIntentEnum;
begin
  result := 0;
  for a := low(TFhirMedicationRequestIntentEnum) to high(TFhirMedicationRequestIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationRequestIntentEnumList(i : Integer) : TFhirMedicationRequestIntentEnumList;
var
  aLoop : TFhirMedicationRequestIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationRequestIntentEnum) to high(TFhirMedicationRequestIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationStatusCodesEnumListAsInteger(aSet : TFhirMedicationStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationStatusCodesEnum) to high(TFhirMedicationStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationStatusCodesEnumList(i : Integer) : TFhirMedicationStatusCodesEnumList;
var
  aLoop : TFhirMedicationStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationStatusCodesEnum) to high(TFhirMedicationStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationUsageStatusCodesEnumListAsInteger(aSet : TFhirMedicationUsageStatusCodesEnumList) : Integer;
var
  a : TFhirMedicationUsageStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirMedicationUsageStatusCodesEnum) to high(TFhirMedicationUsageStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationUsageStatusCodesEnumList(i : Integer) : TFhirMedicationUsageStatusCodesEnumList;
var
  aLoop : TFhirMedicationUsageStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationUsageStatusCodesEnum) to high(TFhirMedicationUsageStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMedicationrequestStatusEnumListAsInteger(aSet : TFhirMedicationrequestStatusEnumList) : Integer;
var
  a : TFhirMedicationrequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirMedicationrequestStatusEnum) to high(TFhirMedicationrequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMedicationrequestStatusEnumList(i : Integer) : TFhirMedicationrequestStatusEnumList;
var
  aLoop : TFhirMedicationrequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirMedicationrequestStatusEnum) to high(TFhirMedicationrequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMessageSignificanceCategoryEnumListAsInteger(aSet : TFhirMessageSignificanceCategoryEnumList) : Integer;
var
  a : TFhirMessageSignificanceCategoryEnum;
begin
  result := 0;
  for a := low(TFhirMessageSignificanceCategoryEnum) to high(TFhirMessageSignificanceCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageSignificanceCategoryEnumList(i : Integer) : TFhirMessageSignificanceCategoryEnumList;
var
  aLoop : TFhirMessageSignificanceCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageSignificanceCategoryEnum) to high(TFhirMessageSignificanceCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirMessageheaderResponseRequestEnumListAsInteger(aSet : TFhirMessageheaderResponseRequestEnumList) : Integer;
var
  a : TFhirMessageheaderResponseRequestEnum;
begin
  result := 0;
  for a := low(TFhirMessageheaderResponseRequestEnum) to high(TFhirMessageheaderResponseRequestEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirMessageheaderResponseRequestEnumList(i : Integer) : TFhirMessageheaderResponseRequestEnumList;
var
  aLoop : TFhirMessageheaderResponseRequestEnum;
begin
  result := [];
  for aLoop := low(TFhirMessageheaderResponseRequestEnum) to high(TFhirMessageheaderResponseRequestEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNameUseEnumListAsInteger(aSet : TFhirNameUseEnumList) : Integer;
var
  a : TFhirNameUseEnum;
begin
  result := 0;
  for a := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNameUseEnumList(i : Integer) : TFhirNameUseEnumList;
var
  aLoop : TFhirNameUseEnum;
begin
  result := [];
  for aLoop := low(TFhirNameUseEnum) to high(TFhirNameUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNamingSystemIdentifierTypeEnumListAsInteger(aSet : TFhirNamingSystemIdentifierTypeEnumList) : Integer;
var
  a : TFhirNamingSystemIdentifierTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingSystemIdentifierTypeEnum) to high(TFhirNamingSystemIdentifierTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingSystemIdentifierTypeEnumList(i : Integer) : TFhirNamingSystemIdentifierTypeEnumList;
var
  aLoop : TFhirNamingSystemIdentifierTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingSystemIdentifierTypeEnum) to high(TFhirNamingSystemIdentifierTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNamingSystemTypeEnumListAsInteger(aSet : TFhirNamingSystemTypeEnumList) : Integer;
var
  a : TFhirNamingSystemTypeEnum;
begin
  result := 0;
  for a := low(TFhirNamingSystemTypeEnum) to high(TFhirNamingSystemTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNamingSystemTypeEnumList(i : Integer) : TFhirNamingSystemTypeEnumList;
var
  aLoop : TFhirNamingSystemTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNamingSystemTypeEnum) to high(TFhirNamingSystemTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNarrativeStatusEnumListAsInteger(aSet : TFhirNarrativeStatusEnumList) : Integer;
var
  a : TFhirNarrativeStatusEnum;
begin
  result := 0;
  for a := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNarrativeStatusEnumList(i : Integer) : TFhirNarrativeStatusEnumList;
var
  aLoop : TFhirNarrativeStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNarrativeStatusEnum) to high(TFhirNarrativeStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNoteTypeEnumListAsInteger(aSet : TFhirNoteTypeEnumList) : Integer;
var
  a : TFhirNoteTypeEnum;
begin
  result := 0;
  for a := low(TFhirNoteTypeEnum) to high(TFhirNoteTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNoteTypeEnumList(i : Integer) : TFhirNoteTypeEnumList;
var
  aLoop : TFhirNoteTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirNoteTypeEnum) to high(TFhirNoteTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirNutritionProductStatusEnumListAsInteger(aSet : TFhirNutritionProductStatusEnumList) : Integer;
var
  a : TFhirNutritionProductStatusEnum;
begin
  result := 0;
  for a := low(TFhirNutritionProductStatusEnum) to high(TFhirNutritionProductStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirNutritionProductStatusEnumList(i : Integer) : TFhirNutritionProductStatusEnumList;
var
  aLoop : TFhirNutritionProductStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirNutritionProductStatusEnum) to high(TFhirNutritionProductStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationDataTypeEnumListAsInteger(aSet : TFhirObservationDataTypeEnumList) : Integer;
var
  a : TFhirObservationDataTypeEnum;
begin
  result := 0;
  for a := low(TFhirObservationDataTypeEnum) to high(TFhirObservationDataTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationDataTypeEnumList(i : Integer) : TFhirObservationDataTypeEnumList;
var
  aLoop : TFhirObservationDataTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationDataTypeEnum) to high(TFhirObservationDataTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationRangeCategoryEnumListAsInteger(aSet : TFhirObservationRangeCategoryEnumList) : Integer;
var
  a : TFhirObservationRangeCategoryEnum;
begin
  result := 0;
  for a := low(TFhirObservationRangeCategoryEnum) to high(TFhirObservationRangeCategoryEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationRangeCategoryEnumList(i : Integer) : TFhirObservationRangeCategoryEnumList;
var
  aLoop : TFhirObservationRangeCategoryEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationRangeCategoryEnum) to high(TFhirObservationRangeCategoryEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirObservationStatusEnumListAsInteger(aSet : TFhirObservationStatusEnumList) : Integer;
var
  a : TFhirObservationStatusEnum;
begin
  result := 0;
  for a := low(TFhirObservationStatusEnum) to high(TFhirObservationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirObservationStatusEnumList(i : Integer) : TFhirObservationStatusEnumList;
var
  aLoop : TFhirObservationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirObservationStatusEnum) to high(TFhirObservationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOperationKindEnumListAsInteger(aSet : TFhirOperationKindEnumList) : Integer;
var
  a : TFhirOperationKindEnum;
begin
  result := 0;
  for a := low(TFhirOperationKindEnum) to high(TFhirOperationKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationKindEnumList(i : Integer) : TFhirOperationKindEnumList;
var
  aLoop : TFhirOperationKindEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationKindEnum) to high(TFhirOperationKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOperationParameterScopeEnumListAsInteger(aSet : TFhirOperationParameterScopeEnumList) : Integer;
var
  a : TFhirOperationParameterScopeEnum;
begin
  result := 0;
  for a := low(TFhirOperationParameterScopeEnum) to high(TFhirOperationParameterScopeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterScopeEnumList(i : Integer) : TFhirOperationParameterScopeEnumList;
var
  aLoop : TFhirOperationParameterScopeEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterScopeEnum) to high(TFhirOperationParameterScopeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOperationParameterUseEnumListAsInteger(aSet : TFhirOperationParameterUseEnumList) : Integer;
var
  a : TFhirOperationParameterUseEnum;
begin
  result := 0;
  for a := low(TFhirOperationParameterUseEnum) to high(TFhirOperationParameterUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOperationParameterUseEnumList(i : Integer) : TFhirOperationParameterUseEnumList;
var
  aLoop : TFhirOperationParameterUseEnum;
begin
  result := [];
  for aLoop := low(TFhirOperationParameterUseEnum) to high(TFhirOperationParameterUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirOrientationTypeEnumListAsInteger(aSet : TFhirOrientationTypeEnumList) : Integer;
var
  a : TFhirOrientationTypeEnum;
begin
  result := 0;
  for a := low(TFhirOrientationTypeEnum) to high(TFhirOrientationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirOrientationTypeEnumList(i : Integer) : TFhirOrientationTypeEnumList;
var
  aLoop : TFhirOrientationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirOrientationTypeEnum) to high(TFhirOrientationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirParticipationStatusEnumListAsInteger(aSet : TFhirParticipationStatusEnumList) : Integer;
var
  a : TFhirParticipationStatusEnum;
begin
  result := 0;
  for a := low(TFhirParticipationStatusEnum) to high(TFhirParticipationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirParticipationStatusEnumList(i : Integer) : TFhirParticipationStatusEnumList;
var
  aLoop : TFhirParticipationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirParticipationStatusEnum) to high(TFhirParticipationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPaymentOutcomeEnumListAsInteger(aSet : TFhirPaymentOutcomeEnumList) : Integer;
var
  a : TFhirPaymentOutcomeEnum;
begin
  result := 0;
  for a := low(TFhirPaymentOutcomeEnum) to high(TFhirPaymentOutcomeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPaymentOutcomeEnumList(i : Integer) : TFhirPaymentOutcomeEnumList;
var
  aLoop : TFhirPaymentOutcomeEnum;
begin
  result := [];
  for aLoop := low(TFhirPaymentOutcomeEnum) to high(TFhirPaymentOutcomeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPermissionRuleCombiningEnumListAsInteger(aSet : TFhirPermissionRuleCombiningEnumList) : Integer;
var
  a : TFhirPermissionRuleCombiningEnum;
begin
  result := 0;
  for a := low(TFhirPermissionRuleCombiningEnum) to high(TFhirPermissionRuleCombiningEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPermissionRuleCombiningEnumList(i : Integer) : TFhirPermissionRuleCombiningEnumList;
var
  aLoop : TFhirPermissionRuleCombiningEnum;
begin
  result := [];
  for aLoop := low(TFhirPermissionRuleCombiningEnum) to high(TFhirPermissionRuleCombiningEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPermissionStatusEnumListAsInteger(aSet : TFhirPermissionStatusEnumList) : Integer;
var
  a : TFhirPermissionStatusEnum;
begin
  result := 0;
  for a := low(TFhirPermissionStatusEnum) to high(TFhirPermissionStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPermissionStatusEnumList(i : Integer) : TFhirPermissionStatusEnumList;
var
  aLoop : TFhirPermissionStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirPermissionStatusEnum) to high(TFhirPermissionStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPriceComponentTypeEnumListAsInteger(aSet : TFhirPriceComponentTypeEnumList) : Integer;
var
  a : TFhirPriceComponentTypeEnum;
begin
  result := 0;
  for a := low(TFhirPriceComponentTypeEnum) to high(TFhirPriceComponentTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPriceComponentTypeEnumList(i : Integer) : TFhirPriceComponentTypeEnumList;
var
  aLoop : TFhirPriceComponentTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirPriceComponentTypeEnum) to high(TFhirPriceComponentTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPropertyRepresentationEnumListAsInteger(aSet : TFhirPropertyRepresentationEnumList) : Integer;
var
  a : TFhirPropertyRepresentationEnum;
begin
  result := 0;
  for a := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPropertyRepresentationEnumList(i : Integer) : TFhirPropertyRepresentationEnumList;
var
  aLoop : TFhirPropertyRepresentationEnum;
begin
  result := [];
  for aLoop := low(TFhirPropertyRepresentationEnum) to high(TFhirPropertyRepresentationEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirProvenanceEntityRoleEnumListAsInteger(aSet : TFhirProvenanceEntityRoleEnumList) : Integer;
var
  a : TFhirProvenanceEntityRoleEnum;
begin
  result := 0;
  for a := low(TFhirProvenanceEntityRoleEnum) to high(TFhirProvenanceEntityRoleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirProvenanceEntityRoleEnumList(i : Integer) : TFhirProvenanceEntityRoleEnumList;
var
  aLoop : TFhirProvenanceEntityRoleEnum;
begin
  result := [];
  for aLoop := low(TFhirProvenanceEntityRoleEnum) to high(TFhirProvenanceEntityRoleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirPublicationStatusEnumListAsInteger(aSet : TFhirPublicationStatusEnumList) : Integer;
var
  a : TFhirPublicationStatusEnum;
begin
  result := 0;
  for a := low(TFhirPublicationStatusEnum) to high(TFhirPublicationStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirPublicationStatusEnumList(i : Integer) : TFhirPublicationStatusEnumList;
var
  aLoop : TFhirPublicationStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirPublicationStatusEnum) to high(TFhirPublicationStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuantityComparatorEnumListAsInteger(aSet : TFhirQuantityComparatorEnumList) : Integer;
var
  a : TFhirQuantityComparatorEnum;
begin
  result := 0;
  for a := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuantityComparatorEnumList(i : Integer) : TFhirQuantityComparatorEnumList;
var
  aLoop : TFhirQuantityComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirQuantityComparatorEnum) to high(TFhirQuantityComparatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireAnswerConstraintEnumListAsInteger(aSet : TFhirQuestionnaireAnswerConstraintEnumList) : Integer;
var
  a : TFhirQuestionnaireAnswerConstraintEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireAnswerConstraintEnum) to high(TFhirQuestionnaireAnswerConstraintEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireAnswerConstraintEnumList(i : Integer) : TFhirQuestionnaireAnswerConstraintEnumList;
var
  aLoop : TFhirQuestionnaireAnswerConstraintEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireAnswerConstraintEnum) to high(TFhirQuestionnaireAnswerConstraintEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireItemDisabledDisplayEnumListAsInteger(aSet : TFhirQuestionnaireItemDisabledDisplayEnumList) : Integer;
var
  a : TFhirQuestionnaireItemDisabledDisplayEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireItemDisabledDisplayEnum) to high(TFhirQuestionnaireItemDisabledDisplayEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireItemDisabledDisplayEnumList(i : Integer) : TFhirQuestionnaireItemDisabledDisplayEnumList;
var
  aLoop : TFhirQuestionnaireItemDisabledDisplayEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireItemDisabledDisplayEnum) to high(TFhirQuestionnaireItemDisabledDisplayEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireItemOperatorEnumListAsInteger(aSet : TFhirQuestionnaireItemOperatorEnumList) : Integer;
var
  a : TFhirQuestionnaireItemOperatorEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireItemOperatorEnum) to high(TFhirQuestionnaireItemOperatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireItemOperatorEnumList(i : Integer) : TFhirQuestionnaireItemOperatorEnumList;
var
  aLoop : TFhirQuestionnaireItemOperatorEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireItemOperatorEnum) to high(TFhirQuestionnaireItemOperatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireItemTypeEnumListAsInteger(aSet : TFhirQuestionnaireItemTypeEnumList) : Integer;
var
  a : TFhirQuestionnaireItemTypeEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireItemTypeEnum) to high(TFhirQuestionnaireItemTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireItemTypeEnumList(i : Integer) : TFhirQuestionnaireItemTypeEnumList;
var
  aLoop : TFhirQuestionnaireItemTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireItemTypeEnum) to high(TFhirQuestionnaireItemTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirQuestionnaireResponseStatusEnumListAsInteger(aSet : TFhirQuestionnaireResponseStatusEnumList) : Integer;
var
  a : TFhirQuestionnaireResponseStatusEnum;
begin
  result := 0;
  for a := low(TFhirQuestionnaireResponseStatusEnum) to high(TFhirQuestionnaireResponseStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirQuestionnaireResponseStatusEnumList(i : Integer) : TFhirQuestionnaireResponseStatusEnumList;
var
  aLoop : TFhirQuestionnaireResponseStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirQuestionnaireResponseStatusEnum) to high(TFhirQuestionnaireResponseStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReferenceHandlingPolicyEnumListAsInteger(aSet : TFhirReferenceHandlingPolicyEnumList) : Integer;
var
  a : TFhirReferenceHandlingPolicyEnum;
begin
  result := 0;
  for a := low(TFhirReferenceHandlingPolicyEnum) to high(TFhirReferenceHandlingPolicyEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferenceHandlingPolicyEnumList(i : Integer) : TFhirReferenceHandlingPolicyEnumList;
var
  aLoop : TFhirReferenceHandlingPolicyEnum;
begin
  result := [];
  for aLoop := low(TFhirReferenceHandlingPolicyEnum) to high(TFhirReferenceHandlingPolicyEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReferenceVersionRulesEnumListAsInteger(aSet : TFhirReferenceVersionRulesEnumList) : Integer;
var
  a : TFhirReferenceVersionRulesEnum;
begin
  result := 0;
  for a := low(TFhirReferenceVersionRulesEnum) to high(TFhirReferenceVersionRulesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReferenceVersionRulesEnumList(i : Integer) : TFhirReferenceVersionRulesEnumList;
var
  aLoop : TFhirReferenceVersionRulesEnum;
begin
  result := [];
  for aLoop := low(TFhirReferenceVersionRulesEnum) to high(TFhirReferenceVersionRulesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRelatedArtifactTypeEnumListAsInteger(aSet : TFhirRelatedArtifactTypeEnumList) : Integer;
var
  a : TFhirRelatedArtifactTypeEnum;
begin
  result := 0;
  for a := low(TFhirRelatedArtifactTypeEnum) to high(TFhirRelatedArtifactTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRelatedArtifactTypeEnumList(i : Integer) : TFhirRelatedArtifactTypeEnumList;
var
  aLoop : TFhirRelatedArtifactTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirRelatedArtifactTypeEnum) to high(TFhirRelatedArtifactTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRelatedArtifactTypeExpandedEnumListAsInteger(aSet : TFhirRelatedArtifactTypeExpandedEnumList) : Integer;
var
  a : TFhirRelatedArtifactTypeExpandedEnum;
begin
  result := 0;
  for a := low(TFhirRelatedArtifactTypeExpandedEnum) to high(TFhirRelatedArtifactTypeExpandedEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRelatedArtifactTypeExpandedEnumList(i : Integer) : TFhirRelatedArtifactTypeExpandedEnumList;
var
  aLoop : TFhirRelatedArtifactTypeExpandedEnum;
begin
  result := [];
  for aLoop := low(TFhirRelatedArtifactTypeExpandedEnum) to high(TFhirRelatedArtifactTypeExpandedEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirReportRelationshipTypeEnumListAsInteger(aSet : TFhirReportRelationshipTypeEnumList) : Integer;
var
  a : TFhirReportRelationshipTypeEnum;
begin
  result := 0;
  for a := low(TFhirReportRelationshipTypeEnum) to high(TFhirReportRelationshipTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirReportRelationshipTypeEnumList(i : Integer) : TFhirReportRelationshipTypeEnumList;
var
  aLoop : TFhirReportRelationshipTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirReportRelationshipTypeEnum) to high(TFhirReportRelationshipTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestIntentEnumListAsInteger(aSet : TFhirRequestIntentEnumList) : Integer;
var
  a : TFhirRequestIntentEnum;
begin
  result := 0;
  for a := low(TFhirRequestIntentEnum) to high(TFhirRequestIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestIntentEnumList(i : Integer) : TFhirRequestIntentEnumList;
var
  aLoop : TFhirRequestIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestIntentEnum) to high(TFhirRequestIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestPriorityEnumListAsInteger(aSet : TFhirRequestPriorityEnumList) : Integer;
var
  a : TFhirRequestPriorityEnum;
begin
  result := 0;
  for a := low(TFhirRequestPriorityEnum) to high(TFhirRequestPriorityEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestPriorityEnumList(i : Integer) : TFhirRequestPriorityEnumList;
var
  aLoop : TFhirRequestPriorityEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestPriorityEnum) to high(TFhirRequestPriorityEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestResourceTypesEnumListAsInteger(aSet : TFhirRequestResourceTypesEnumList) : Integer;
var
  a : TFhirRequestResourceTypesEnum;
begin
  result := 0;
  for a := low(TFhirRequestResourceTypesEnum) to high(TFhirRequestResourceTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestResourceTypesEnumList(i : Integer) : TFhirRequestResourceTypesEnumList;
var
  aLoop : TFhirRequestResourceTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestResourceTypesEnum) to high(TFhirRequestResourceTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRequestStatusEnumListAsInteger(aSet : TFhirRequestStatusEnumList) : Integer;
var
  a : TFhirRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirRequestStatusEnum) to high(TFhirRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRequestStatusEnumList(i : Integer) : TFhirRequestStatusEnumList;
var
  aLoop : TFhirRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirRequestStatusEnum) to high(TFhirRequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResourceTypesEnumListAsInteger(aSet : TFhirResourceTypesEnumList) : Integer;
var
  a : TFhirResourceTypesEnum;
begin
  result := 0;
  for a := low(TFhirResourceTypesEnum) to high(TFhirResourceTypesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceTypesEnumList(i : Integer) : TFhirResourceTypesEnumList;
var
  aLoop : TFhirResourceTypesEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceTypesEnum) to high(TFhirResourceTypesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResourceVersionPolicyEnumListAsInteger(aSet : TFhirResourceVersionPolicyEnumList) : Integer;
var
  a : TFhirResourceVersionPolicyEnum;
begin
  result := 0;
  for a := low(TFhirResourceVersionPolicyEnum) to high(TFhirResourceVersionPolicyEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResourceVersionPolicyEnumList(i : Integer) : TFhirResourceVersionPolicyEnumList;
var
  aLoop : TFhirResourceVersionPolicyEnum;
begin
  result := [];
  for aLoop := low(TFhirResourceVersionPolicyEnum) to high(TFhirResourceVersionPolicyEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirResponseTypeEnumListAsInteger(aSet : TFhirResponseTypeEnumList) : Integer;
var
  a : TFhirResponseTypeEnum;
begin
  result := 0;
  for a := low(TFhirResponseTypeEnum) to high(TFhirResponseTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirResponseTypeEnumList(i : Integer) : TFhirResponseTypeEnumList;
var
  aLoop : TFhirResponseTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirResponseTypeEnum) to high(TFhirResponseTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirRestfulCapabilityModeEnumListAsInteger(aSet : TFhirRestfulCapabilityModeEnumList) : Integer;
var
  a : TFhirRestfulCapabilityModeEnum;
begin
  result := 0;
  for a := low(TFhirRestfulCapabilityModeEnum) to high(TFhirRestfulCapabilityModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirRestfulCapabilityModeEnumList(i : Integer) : TFhirRestfulCapabilityModeEnumList;
var
  aLoop : TFhirRestfulCapabilityModeEnum;
begin
  result := [];
  for aLoop := low(TFhirRestfulCapabilityModeEnum) to high(TFhirRestfulCapabilityModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchComparatorEnumListAsInteger(aSet : TFhirSearchComparatorEnumList) : Integer;
var
  a : TFhirSearchComparatorEnum;
begin
  result := 0;
  for a := low(TFhirSearchComparatorEnum) to high(TFhirSearchComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchComparatorEnumList(i : Integer) : TFhirSearchComparatorEnumList;
var
  aLoop : TFhirSearchComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchComparatorEnum) to high(TFhirSearchComparatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchEntryModeEnumListAsInteger(aSet : TFhirSearchEntryModeEnumList) : Integer;
var
  a : TFhirSearchEntryModeEnum;
begin
  result := 0;
  for a := low(TFhirSearchEntryModeEnum) to high(TFhirSearchEntryModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchEntryModeEnumList(i : Integer) : TFhirSearchEntryModeEnumList;
var
  aLoop : TFhirSearchEntryModeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchEntryModeEnum) to high(TFhirSearchEntryModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchModifierCodeEnumListAsInteger(aSet : TFhirSearchModifierCodeEnumList) : Integer;
var
  a : TFhirSearchModifierCodeEnum;
begin
  result := 0;
  for a := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchModifierCodeEnumList(i : Integer) : TFhirSearchModifierCodeEnumList;
var
  aLoop : TFhirSearchModifierCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchModifierCodeEnum) to high(TFhirSearchModifierCodeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchParamTypeEnumListAsInteger(aSet : TFhirSearchParamTypeEnumList) : Integer;
var
  a : TFhirSearchParamTypeEnum;
begin
  result := 0;
  for a := low(TFhirSearchParamTypeEnum) to high(TFhirSearchParamTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchParamTypeEnumList(i : Integer) : TFhirSearchParamTypeEnumList;
var
  aLoop : TFhirSearchParamTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchParamTypeEnum) to high(TFhirSearchParamTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSearchProcessingModeTypeEnumListAsInteger(aSet : TFhirSearchProcessingModeTypeEnumList) : Integer;
var
  a : TFhirSearchProcessingModeTypeEnum;
begin
  result := 0;
  for a := low(TFhirSearchProcessingModeTypeEnum) to high(TFhirSearchProcessingModeTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSearchProcessingModeTypeEnumList(i : Integer) : TFhirSearchProcessingModeTypeEnumList;
var
  aLoop : TFhirSearchProcessingModeTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSearchProcessingModeTypeEnum) to high(TFhirSearchProcessingModeTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSequenceTypeEnumListAsInteger(aSet : TFhirSequenceTypeEnumList) : Integer;
var
  a : TFhirSequenceTypeEnum;
begin
  result := 0;
  for a := low(TFhirSequenceTypeEnum) to high(TFhirSequenceTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSequenceTypeEnumList(i : Integer) : TFhirSequenceTypeEnumList;
var
  aLoop : TFhirSequenceTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSequenceTypeEnum) to high(TFhirSequenceTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSlicingRulesEnumListAsInteger(aSet : TFhirSlicingRulesEnumList) : Integer;
var
  a : TFhirSlicingRulesEnum;
begin
  result := 0;
  for a := low(TFhirSlicingRulesEnum) to high(TFhirSlicingRulesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlicingRulesEnumList(i : Integer) : TFhirSlicingRulesEnumList;
var
  aLoop : TFhirSlicingRulesEnum;
begin
  result := [];
  for aLoop := low(TFhirSlicingRulesEnum) to high(TFhirSlicingRulesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSlotStatusEnumListAsInteger(aSet : TFhirSlotStatusEnumList) : Integer;
var
  a : TFhirSlotStatusEnum;
begin
  result := 0;
  for a := low(TFhirSlotStatusEnum) to high(TFhirSlotStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSlotStatusEnumList(i : Integer) : TFhirSlotStatusEnumList;
var
  aLoop : TFhirSlotStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSlotStatusEnum) to high(TFhirSlotStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSortDirectionEnumListAsInteger(aSet : TFhirSortDirectionEnumList) : Integer;
var
  a : TFhirSortDirectionEnum;
begin
  result := 0;
  for a := low(TFhirSortDirectionEnum) to high(TFhirSortDirectionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSortDirectionEnumList(i : Integer) : TFhirSortDirectionEnumList;
var
  aLoop : TFhirSortDirectionEnum;
begin
  result := [];
  for aLoop := low(TFhirSortDirectionEnum) to high(TFhirSortDirectionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSpecimenCombinedEnumListAsInteger(aSet : TFhirSpecimenCombinedEnumList) : Integer;
var
  a : TFhirSpecimenCombinedEnum;
begin
  result := 0;
  for a := low(TFhirSpecimenCombinedEnum) to high(TFhirSpecimenCombinedEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenCombinedEnumList(i : Integer) : TFhirSpecimenCombinedEnumList;
var
  aLoop : TFhirSpecimenCombinedEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenCombinedEnum) to high(TFhirSpecimenCombinedEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSpecimenContainedPreferenceEnumListAsInteger(aSet : TFhirSpecimenContainedPreferenceEnumList) : Integer;
var
  a : TFhirSpecimenContainedPreferenceEnum;
begin
  result := 0;
  for a := low(TFhirSpecimenContainedPreferenceEnum) to high(TFhirSpecimenContainedPreferenceEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenContainedPreferenceEnumList(i : Integer) : TFhirSpecimenContainedPreferenceEnumList;
var
  aLoop : TFhirSpecimenContainedPreferenceEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenContainedPreferenceEnum) to high(TFhirSpecimenContainedPreferenceEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSpecimenStatusEnumListAsInteger(aSet : TFhirSpecimenStatusEnumList) : Integer;
var
  a : TFhirSpecimenStatusEnum;
begin
  result := 0;
  for a := low(TFhirSpecimenStatusEnum) to high(TFhirSpecimenStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSpecimenStatusEnumList(i : Integer) : TFhirSpecimenStatusEnumList;
var
  aLoop : TFhirSpecimenStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSpecimenStatusEnum) to high(TFhirSpecimenStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStatusEnumListAsInteger(aSet : TFhirStatusEnumList) : Integer;
var
  a : TFhirStatusEnum;
begin
  result := 0;
  for a := low(TFhirStatusEnum) to high(TFhirStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStatusEnumList(i : Integer) : TFhirStatusEnumList;
var
  aLoop : TFhirStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirStatusEnum) to high(TFhirStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStrandTypeEnumListAsInteger(aSet : TFhirStrandTypeEnumList) : Integer;
var
  a : TFhirStrandTypeEnum;
begin
  result := 0;
  for a := low(TFhirStrandTypeEnum) to high(TFhirStrandTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStrandTypeEnumList(i : Integer) : TFhirStrandTypeEnumList;
var
  aLoop : TFhirStrandTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirStrandTypeEnum) to high(TFhirStrandTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureDefinitionKindEnumListAsInteger(aSet : TFhirStructureDefinitionKindEnumList) : Integer;
var
  a : TFhirStructureDefinitionKindEnum;
begin
  result := 0;
  for a := low(TFhirStructureDefinitionKindEnum) to high(TFhirStructureDefinitionKindEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureDefinitionKindEnumList(i : Integer) : TFhirStructureDefinitionKindEnumList;
var
  aLoop : TFhirStructureDefinitionKindEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureDefinitionKindEnum) to high(TFhirStructureDefinitionKindEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapGroupTypeModeEnumListAsInteger(aSet : TFhirStructureMapGroupTypeModeEnumList) : Integer;
var
  a : TFhirStructureMapGroupTypeModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapGroupTypeModeEnum) to high(TFhirStructureMapGroupTypeModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapGroupTypeModeEnumList(i : Integer) : TFhirStructureMapGroupTypeModeEnumList;
var
  aLoop : TFhirStructureMapGroupTypeModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapGroupTypeModeEnum) to high(TFhirStructureMapGroupTypeModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapInputModeEnumListAsInteger(aSet : TFhirStructureMapInputModeEnumList) : Integer;
var
  a : TFhirStructureMapInputModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapInputModeEnum) to high(TFhirStructureMapInputModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapInputModeEnumList(i : Integer) : TFhirStructureMapInputModeEnumList;
var
  aLoop : TFhirStructureMapInputModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapInputModeEnum) to high(TFhirStructureMapInputModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapModelModeEnumListAsInteger(aSet : TFhirStructureMapModelModeEnumList) : Integer;
var
  a : TFhirStructureMapModelModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapModelModeEnum) to high(TFhirStructureMapModelModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapModelModeEnumList(i : Integer) : TFhirStructureMapModelModeEnumList;
var
  aLoop : TFhirStructureMapModelModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapModelModeEnum) to high(TFhirStructureMapModelModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapSourceListModeEnumListAsInteger(aSet : TFhirStructureMapSourceListModeEnumList) : Integer;
var
  a : TFhirStructureMapSourceListModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapSourceListModeEnum) to high(TFhirStructureMapSourceListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapSourceListModeEnumList(i : Integer) : TFhirStructureMapSourceListModeEnumList;
var
  aLoop : TFhirStructureMapSourceListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapSourceListModeEnum) to high(TFhirStructureMapSourceListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapTargetListModeEnumListAsInteger(aSet : TFhirStructureMapTargetListModeEnumList) : Integer;
var
  a : TFhirStructureMapTargetListModeEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapTargetListModeEnum) to high(TFhirStructureMapTargetListModeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapTargetListModeEnumList(i : Integer) : TFhirStructureMapTargetListModeEnumList;
var
  aLoop : TFhirStructureMapTargetListModeEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapTargetListModeEnum) to high(TFhirStructureMapTargetListModeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirStructureMapTransformEnumListAsInteger(aSet : TFhirStructureMapTransformEnumList) : Integer;
var
  a : TFhirStructureMapTransformEnum;
begin
  result := 0;
  for a := low(TFhirStructureMapTransformEnum) to high(TFhirStructureMapTransformEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirStructureMapTransformEnumList(i : Integer) : TFhirStructureMapTransformEnumList;
var
  aLoop : TFhirStructureMapTransformEnum;
begin
  result := [];
  for aLoop := low(TFhirStructureMapTransformEnum) to high(TFhirStructureMapTransformEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubmitDataUpdateTypeEnumListAsInteger(aSet : TFhirSubmitDataUpdateTypeEnumList) : Integer;
var
  a : TFhirSubmitDataUpdateTypeEnum;
begin
  result := 0;
  for a := low(TFhirSubmitDataUpdateTypeEnum) to high(TFhirSubmitDataUpdateTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubmitDataUpdateTypeEnumList(i : Integer) : TFhirSubmitDataUpdateTypeEnumList;
var
  aLoop : TFhirSubmitDataUpdateTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSubmitDataUpdateTypeEnum) to high(TFhirSubmitDataUpdateTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionNotificationTypeEnumListAsInteger(aSet : TFhirSubscriptionNotificationTypeEnumList) : Integer;
var
  a : TFhirSubscriptionNotificationTypeEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionNotificationTypeEnum) to high(TFhirSubscriptionNotificationTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionNotificationTypeEnumList(i : Integer) : TFhirSubscriptionNotificationTypeEnumList;
var
  aLoop : TFhirSubscriptionNotificationTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionNotificationTypeEnum) to high(TFhirSubscriptionNotificationTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionPayloadContentEnumListAsInteger(aSet : TFhirSubscriptionPayloadContentEnumList) : Integer;
var
  a : TFhirSubscriptionPayloadContentEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionPayloadContentEnum) to high(TFhirSubscriptionPayloadContentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionPayloadContentEnumList(i : Integer) : TFhirSubscriptionPayloadContentEnumList;
var
  aLoop : TFhirSubscriptionPayloadContentEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionPayloadContentEnum) to high(TFhirSubscriptionPayloadContentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionSearchModifierEnumListAsInteger(aSet : TFhirSubscriptionSearchModifierEnumList) : Integer;
var
  a : TFhirSubscriptionSearchModifierEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionSearchModifierEnum) to high(TFhirSubscriptionSearchModifierEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionSearchModifierEnumList(i : Integer) : TFhirSubscriptionSearchModifierEnumList;
var
  aLoop : TFhirSubscriptionSearchModifierEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionSearchModifierEnum) to high(TFhirSubscriptionSearchModifierEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSubscriptionStatusCodesEnumListAsInteger(aSet : TFhirSubscriptionStatusCodesEnumList) : Integer;
var
  a : TFhirSubscriptionStatusCodesEnum;
begin
  result := 0;
  for a := low(TFhirSubscriptionStatusCodesEnum) to high(TFhirSubscriptionStatusCodesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSubscriptionStatusCodesEnumList(i : Integer) : TFhirSubscriptionStatusCodesEnumList;
var
  aLoop : TFhirSubscriptionStatusCodesEnum;
begin
  result := [];
  for aLoop := low(TFhirSubscriptionStatusCodesEnum) to high(TFhirSubscriptionStatusCodesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSupplyDeliveryStatusEnumListAsInteger(aSet : TFhirSupplyDeliveryStatusEnumList) : Integer;
var
  a : TFhirSupplyDeliveryStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplyDeliveryStatusEnum) to high(TFhirSupplyDeliveryStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyDeliveryStatusEnumList(i : Integer) : TFhirSupplyDeliveryStatusEnumList;
var
  aLoop : TFhirSupplyDeliveryStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplyDeliveryStatusEnum) to high(TFhirSupplyDeliveryStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSupplyRequestStatusEnumListAsInteger(aSet : TFhirSupplyRequestStatusEnumList) : Integer;
var
  a : TFhirSupplyRequestStatusEnum;
begin
  result := 0;
  for a := low(TFhirSupplyRequestStatusEnum) to high(TFhirSupplyRequestStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSupplyRequestStatusEnumList(i : Integer) : TFhirSupplyRequestStatusEnumList;
var
  aLoop : TFhirSupplyRequestStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirSupplyRequestStatusEnum) to high(TFhirSupplyRequestStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirSystemRestfulInteractionEnumListAsInteger(aSet : TFhirSystemRestfulInteractionEnumList) : Integer;
var
  a : TFhirSystemRestfulInteractionEnum;
begin
  result := 0;
  for a := low(TFhirSystemRestfulInteractionEnum) to high(TFhirSystemRestfulInteractionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirSystemRestfulInteractionEnumList(i : Integer) : TFhirSystemRestfulInteractionEnumList;
var
  aLoop : TFhirSystemRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirSystemRestfulInteractionEnum) to high(TFhirSystemRestfulInteractionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTaskIntentEnumListAsInteger(aSet : TFhirTaskIntentEnumList) : Integer;
var
  a : TFhirTaskIntentEnum;
begin
  result := 0;
  for a := low(TFhirTaskIntentEnum) to high(TFhirTaskIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTaskIntentEnumList(i : Integer) : TFhirTaskIntentEnumList;
var
  aLoop : TFhirTaskIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirTaskIntentEnum) to high(TFhirTaskIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTaskStatusEnumListAsInteger(aSet : TFhirTaskStatusEnumList) : Integer;
var
  a : TFhirTaskStatusEnum;
begin
  result := 0;
  for a := low(TFhirTaskStatusEnum) to high(TFhirTaskStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTaskStatusEnumList(i : Integer) : TFhirTaskStatusEnumList;
var
  aLoop : TFhirTaskStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirTaskStatusEnum) to high(TFhirTaskStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportActionResultEnumListAsInteger(aSet : TFhirTestReportActionResultEnumList) : Integer;
var
  a : TFhirTestReportActionResultEnum;
begin
  result := 0;
  for a := low(TFhirTestReportActionResultEnum) to high(TFhirTestReportActionResultEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportActionResultEnumList(i : Integer) : TFhirTestReportActionResultEnumList;
var
  aLoop : TFhirTestReportActionResultEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportActionResultEnum) to high(TFhirTestReportActionResultEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportParticipantTypeEnumListAsInteger(aSet : TFhirTestReportParticipantTypeEnumList) : Integer;
var
  a : TFhirTestReportParticipantTypeEnum;
begin
  result := 0;
  for a := low(TFhirTestReportParticipantTypeEnum) to high(TFhirTestReportParticipantTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportParticipantTypeEnumList(i : Integer) : TFhirTestReportParticipantTypeEnumList;
var
  aLoop : TFhirTestReportParticipantTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportParticipantTypeEnum) to high(TFhirTestReportParticipantTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportResultEnumListAsInteger(aSet : TFhirTestReportResultEnumList) : Integer;
var
  a : TFhirTestReportResultEnum;
begin
  result := 0;
  for a := low(TFhirTestReportResultEnum) to high(TFhirTestReportResultEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportResultEnumList(i : Integer) : TFhirTestReportResultEnumList;
var
  aLoop : TFhirTestReportResultEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportResultEnum) to high(TFhirTestReportResultEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestReportStatusEnumListAsInteger(aSet : TFhirTestReportStatusEnumList) : Integer;
var
  a : TFhirTestReportStatusEnum;
begin
  result := 0;
  for a := low(TFhirTestReportStatusEnum) to high(TFhirTestReportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestReportStatusEnumList(i : Integer) : TFhirTestReportStatusEnumList;
var
  aLoop : TFhirTestReportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirTestReportStatusEnum) to high(TFhirTestReportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTestScriptRequestMethodCodeEnumListAsInteger(aSet : TFhirTestScriptRequestMethodCodeEnumList) : Integer;
var
  a : TFhirTestScriptRequestMethodCodeEnum;
begin
  result := 0;
  for a := low(TFhirTestScriptRequestMethodCodeEnum) to high(TFhirTestScriptRequestMethodCodeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTestScriptRequestMethodCodeEnumList(i : Integer) : TFhirTestScriptRequestMethodCodeEnumList;
var
  aLoop : TFhirTestScriptRequestMethodCodeEnum;
begin
  result := [];
  for aLoop := low(TFhirTestScriptRequestMethodCodeEnum) to high(TFhirTestScriptRequestMethodCodeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTransportIntentEnumListAsInteger(aSet : TFhirTransportIntentEnumList) : Integer;
var
  a : TFhirTransportIntentEnum;
begin
  result := 0;
  for a := low(TFhirTransportIntentEnum) to high(TFhirTransportIntentEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTransportIntentEnumList(i : Integer) : TFhirTransportIntentEnumList;
var
  aLoop : TFhirTransportIntentEnum;
begin
  result := [];
  for aLoop := low(TFhirTransportIntentEnum) to high(TFhirTransportIntentEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTransportStatusEnumListAsInteger(aSet : TFhirTransportStatusEnumList) : Integer;
var
  a : TFhirTransportStatusEnum;
begin
  result := 0;
  for a := low(TFhirTransportStatusEnum) to high(TFhirTransportStatusEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTransportStatusEnumList(i : Integer) : TFhirTransportStatusEnumList;
var
  aLoop : TFhirTransportStatusEnum;
begin
  result := [];
  for aLoop := low(TFhirTransportStatusEnum) to high(TFhirTransportStatusEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTriggerTypeEnumListAsInteger(aSet : TFhirTriggerTypeEnumList) : Integer;
var
  a : TFhirTriggerTypeEnum;
begin
  result := 0;
  for a := low(TFhirTriggerTypeEnum) to high(TFhirTriggerTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTriggerTypeEnumList(i : Integer) : TFhirTriggerTypeEnumList;
var
  aLoop : TFhirTriggerTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirTriggerTypeEnum) to high(TFhirTriggerTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTriggeredBytypeEnumListAsInteger(aSet : TFhirTriggeredBytypeEnumList) : Integer;
var
  a : TFhirTriggeredBytypeEnum;
begin
  result := 0;
  for a := low(TFhirTriggeredBytypeEnum) to high(TFhirTriggeredBytypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTriggeredBytypeEnumList(i : Integer) : TFhirTriggeredBytypeEnumList;
var
  aLoop : TFhirTriggeredBytypeEnum;
begin
  result := [];
  for aLoop := low(TFhirTriggeredBytypeEnum) to high(TFhirTriggeredBytypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTypeDerivationRuleEnumListAsInteger(aSet : TFhirTypeDerivationRuleEnumList) : Integer;
var
  a : TFhirTypeDerivationRuleEnum;
begin
  result := 0;
  for a := low(TFhirTypeDerivationRuleEnum) to high(TFhirTypeDerivationRuleEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeDerivationRuleEnumList(i : Integer) : TFhirTypeDerivationRuleEnumList;
var
  aLoop : TFhirTypeDerivationRuleEnum;
begin
  result := [];
  for aLoop := low(TFhirTypeDerivationRuleEnum) to high(TFhirTypeDerivationRuleEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirTypeRestfulInteractionEnumListAsInteger(aSet : TFhirTypeRestfulInteractionEnumList) : Integer;
var
  a : TFhirTypeRestfulInteractionEnum;
begin
  result := 0;
  for a := low(TFhirTypeRestfulInteractionEnum) to high(TFhirTypeRestfulInteractionEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirTypeRestfulInteractionEnumList(i : Integer) : TFhirTypeRestfulInteractionEnumList;
var
  aLoop : TFhirTypeRestfulInteractionEnum;
begin
  result := [];
  for aLoop := low(TFhirTypeRestfulInteractionEnum) to high(TFhirTypeRestfulInteractionEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUDIEntryTypeEnumListAsInteger(aSet : TFhirUDIEntryTypeEnumList) : Integer;
var
  a : TFhirUDIEntryTypeEnum;
begin
  result := 0;
  for a := low(TFhirUDIEntryTypeEnum) to high(TFhirUDIEntryTypeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUDIEntryTypeEnumList(i : Integer) : TFhirUDIEntryTypeEnumList;
var
  aLoop : TFhirUDIEntryTypeEnum;
begin
  result := [];
  for aLoop := low(TFhirUDIEntryTypeEnum) to high(TFhirUDIEntryTypeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUnitsOfTimeEnumListAsInteger(aSet : TFhirUnitsOfTimeEnumList) : Integer;
var
  a : TFhirUnitsOfTimeEnum;
begin
  result := 0;
  for a := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUnitsOfTimeEnumList(i : Integer) : TFhirUnitsOfTimeEnumList;
var
  aLoop : TFhirUnitsOfTimeEnum;
begin
  result := [];
  for aLoop := low(TFhirUnitsOfTimeEnum) to high(TFhirUnitsOfTimeEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirUseEnumListAsInteger(aSet : TFhirUseEnumList) : Integer;
var
  a : TFhirUseEnum;
begin
  result := 0;
  for a := low(TFhirUseEnum) to high(TFhirUseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirUseEnumList(i : Integer) : TFhirUseEnumList;
var
  aLoop : TFhirUseEnum;
begin
  result := [];
  for aLoop := low(TFhirUseEnum) to high(TFhirUseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirValueFilterComparatorEnumListAsInteger(aSet : TFhirValueFilterComparatorEnumList) : Integer;
var
  a : TFhirValueFilterComparatorEnum;
begin
  result := 0;
  for a := low(TFhirValueFilterComparatorEnum) to high(TFhirValueFilterComparatorEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirValueFilterComparatorEnumList(i : Integer) : TFhirValueFilterComparatorEnumList;
var
  aLoop : TFhirValueFilterComparatorEnum;
begin
  result := [];
  for aLoop := low(TFhirValueFilterComparatorEnum) to high(TFhirValueFilterComparatorEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirVisionBaseEnumListAsInteger(aSet : TFhirVisionBaseEnumList) : Integer;
var
  a : TFhirVisionBaseEnum;
begin
  result := 0;
  for a := low(TFhirVisionBaseEnum) to high(TFhirVisionBaseEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionBaseEnumList(i : Integer) : TFhirVisionBaseEnumList;
var
  aLoop : TFhirVisionBaseEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionBaseEnum) to high(TFhirVisionBaseEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;

function TFhirVisionEyesEnumListAsInteger(aSet : TFhirVisionEyesEnumList) : Integer;
var
  a : TFhirVisionEyesEnum;
begin
  result := 0;
  for a := low(TFhirVisionEyesEnum) to high(TFhirVisionEyesEnum) do
  begin
    assert(ord(a) < 32);
    if (a in aSet) then
      result := result + 1 shl (ord(a));
  end;
end;

function IntegerAsTFhirVisionEyesEnumList(i : Integer) : TFhirVisionEyesEnumList;
var
  aLoop : TFhirVisionEyesEnum;
begin
  result := [];
  for aLoop := low(TFhirVisionEyesEnum) to high(TFhirVisionEyesEnum) do
  begin
    assert(ord(aLoop) < 32);
    if (i and (1 shl (ord(aLoop))) > 0) then
      result := result + [aLoop];
  end;
end;



end.

