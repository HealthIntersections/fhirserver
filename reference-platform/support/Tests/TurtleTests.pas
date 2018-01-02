unit TurtleTests;

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

Uses
  Sysutils, Classes,
  IdGlobalProtocols,
  TextUtilities, ShellSupport,
  TurtleParser, RDFUtilities,
  FHIRResources, FHIRParserBase, FHIRParser, FHIRBase,
  DUnitX.TestFramework;

Type
  [TextFixture]
  TTurtleTests = Class (TObject)
  Private
    procedure parseTTl(filename : String; ok : boolean);
  Published
    [TestCase] procedure test_double_lower_case_e1;
    [TestCase] procedure test_double_lower_case_e2();
    [TestCase] procedure test_empty_collection1();
    [TestCase] procedure test_empty_collection2();
    [TestCase] procedure test_first1();
//    procedure test_first2();
    [TestCase] procedure test_HYPHEN_MINUS_in_localNameNT();
    [TestCase] procedure test_HYPHEN_MINUS_in_localName();
    [TestCase] procedure test_IRI_spoNT();
    [TestCase] procedure test_IRI_subject();
    [TestCase] procedure test_IRI_with_all_punctuationNT();
    [TestCase] procedure test_IRI_with_all_punctuation();
    [TestCase] procedure test_IRI_with_eight_digit_numeric_escape();
    [TestCase] procedure test_IRI_with_four_digit_numeric_escape();
    [TestCase] procedure test_IRIREF_datatypeNT();
    [TestCase] procedure test_IRIREF_datatype();
    [TestCase] procedure test_labeled_blank_node_objectNT();
    [TestCase] procedure test_labeled_blank_node_object();
    [TestCase] procedure test_labeled_blank_node_subjectNT();
    [TestCase] procedure test_labeled_blank_node_subject();
    [TestCase] procedure test_labeled_blank_node_with_leading_digit();
    [TestCase] procedure test_labeled_blank_node_with_leading_underscore();
    [TestCase] procedure test_labeled_blank_node_with_non_leading_extras();
    [TestCase] procedure test_labeled_blank_node_with_PN_CHARS_BASE_character_boundaries();
    [TestCase] procedure test_langtagged_LONG();
    [TestCase] procedure test_langtagged_LONG_with_subtagNT();
    [TestCase] procedure test_langtagged_LONG_with_subtag();
    [TestCase] procedure test_langtagged_non_LONGNT();
    [TestCase] procedure test_langtagged_non_LONG();
    [TestCase] procedure test_lantag_with_subtagNT();
    [TestCase] procedure test_lantag_with_subtag();
    [TestCase] procedure test_lastNT();
    [TestCase] procedure test_last();
    [TestCase] procedure test_literal_falseNT();
    [TestCase] procedure test_literal_false();
    [TestCase] procedure test_LITERAL_LONG1();
    [TestCase] procedure test_LITERAL_LONG1_ascii_boundariesNT();
    [TestCase] procedure test_LITERAL_LONG1_ascii_boundaries();
    [TestCase] procedure test_LITERAL_LONG1_with_1_squoteNT();
    [TestCase] procedure test_LITERAL_LONG1_with_1_squote();
    [TestCase] procedure test_LITERAL_LONG1_with_2_squotesNT();
    [TestCase] procedure test_LITERAL_LONG1_with_2_squotes();
    [TestCase] procedure test_LITERAL_LONG1_with_UTF8_boundaries();
    [TestCase] procedure test_LITERAL_LONG2();
    [TestCase] procedure test_LITERAL_LONG2_ascii_boundariesNT();
    [TestCase] procedure test_LITERAL_LONG2_ascii_boundaries();
    [TestCase] procedure test_LITERAL_LONG2_with_1_squoteNT();
    [TestCase] procedure test_LITERAL_LONG2_with_1_squote();
    [TestCase] procedure test_LITERAL_LONG2_with_2_squotesNT();
    [TestCase] procedure test_LITERAL_LONG2_with_2_squotes();
    [TestCase] procedure test_LITERAL_LONG2_with_REVERSE_SOLIDUSNT();
    [TestCase] procedure test_LITERAL_LONG2_with_REVERSE_SOLIDUS();
    [TestCase] procedure test_LITERAL_LONG2_with_UTF8_boundaries();
    [TestCase] procedure test_literal_trueNT();
    [TestCase] procedure test_literal_true();
    [TestCase] procedure test_literal_with_BACKSPACENT();
    [TestCase] procedure test_literal_with_BACKSPACE();
    [TestCase] procedure test_literal_with_CARRIAGE_RETURNNT();
    [TestCase] procedure test_literal_with_CARRIAGE_RETURN();
    [TestCase] procedure test_literal_with_CHARACTER_TABULATIONNT();
    [TestCase] procedure test_literal_with_CHARACTER_TABULATION();
    [TestCase] procedure test_literal_with_escaped_BACKSPACE();
    [TestCase] procedure test_literal_with_escaped_CARRIAGE_RETURN();
    [TestCase] procedure test_literal_with_escaped_CHARACTER_TABULATION();
    [TestCase] procedure test_literal_with_escaped_FORM_FEED();
    [TestCase] procedure test_literal_with_escaped_LINE_FEED();
//    procedure test_literal_with_FORM_FEEDNT();
    [TestCase] procedure test_literal_with_FORM_FEED();
    [TestCase] procedure test_literal_with_LINE_FEEDNT();
    [TestCase] procedure test_literal_with_LINE_FEED();
    [TestCase] procedure test_literal_with_numeric_escape4NT();
    [TestCase] procedure test_literal_with_numeric_escape4();
    [TestCase] procedure test_literal_with_numeric_escape8();
    [TestCase] procedure test_literal_with_REVERSE_SOLIDUSNT();
    [TestCase] procedure test_literal_with_REVERSE_SOLIDUS();
    [TestCase] procedure test_LITERAL_with_UTF8_boundariesNT();
    [TestCase] procedure test_LITERAL1NT();
    [TestCase] procedure test_LITERAL1();
    [TestCase] procedure test_LITERAL1_all_controlsNT();
    [TestCase] procedure test_LITERAL1_all_controls();
    [TestCase] procedure test_LITERAL1_all_punctuationNT();
    [TestCase] procedure test_LITERAL1_all_punctuation();
//    procedure test_LITERAL1_ascii_boundariesNT();
    [TestCase] procedure test_LITERAL1_ascii_boundaries();
    [TestCase] procedure test_LITERAL1_with_UTF8_boundaries();
    [TestCase] procedure test_LITERAL2();
    [TestCase] procedure test_LITERAL2_ascii_boundariesNT();
    [TestCase] procedure test_LITERAL2_ascii_boundaries();
    [TestCase] procedure test_LITERAL2_with_UTF8_boundaries();
    [TestCase] procedure test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundariesNT();
    [TestCase] procedure test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries();
    [TestCase] procedure test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundariesNT();
    [TestCase] procedure test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries();
//    procedure test_localname_with_COLONNT();
//    procedure test_localname_with_COLON();
    [TestCase] procedure test_localName_with_leading_digitNT();
    [TestCase] procedure test_localName_with_leading_digit();
    [TestCase] procedure test_localName_with_leading_underscoreNT();
    [TestCase] procedure test_localName_with_leading_underscore();
    [TestCase] procedure test_localName_with_nfc_PN_CHARS_BASE_character_boundariesNT();
    [TestCase] procedure test_localName_with_nfc_PN_CHARS_BASE_character_boundaries();
    [TestCase] procedure test_localName_with_non_leading_extrasNT();
    [TestCase] procedure test_localName_with_non_leading_extras();
    [TestCase] procedure test_negative_numericNT();
    [TestCase] procedure test_negative_numeric();
    [TestCase] procedure test_nested_blankNodePropertyListsNT();
    [TestCase] procedure test_nested_blankNodePropertyLists();
    [TestCase] procedure test_nested_collectionNT();
    [TestCase] procedure test_nested_collection();
    [TestCase] procedure test_number_sign_following_localNameNT();
//    procedure test_number_sign_following_localName();
    [TestCase] procedure test_number_sign_following_PNAME_NSNT();
//    procedure test_number_sign_following_PNAME_NS();
    [TestCase] procedure test_numeric_with_leading_0NT();
    [TestCase] procedure test_numeric_with_leading_0();
    [TestCase] procedure test_objectList_with_two_objectsNT();
    [TestCase] procedure test_objectList_with_two_objects();
    [TestCase] procedure test_old_style_base();
    [TestCase] procedure test_old_style_prefix();
    [TestCase] procedure test_percent_escaped_localNameNT();
//    procedure test_percent_escaped_localName();
    [TestCase] procedure test_positive_numericNT();
    [TestCase] procedure test_positive_numeric();
    [TestCase] procedure test_predicateObjectList_with_two_objectListsNT();
    [TestCase] procedure test_predicateObjectList_with_two_objectLists();
//    procedure test_prefix_only_IRI();
    [TestCase] procedure test_prefix_reassigned_and_usedNT();
    [TestCase] procedure test_prefix_reassigned_and_used();
    [TestCase] procedure test_prefix_with_non_leading_extras();
    [TestCase] procedure test_prefix_with_PN_CHARS_BASE_character_boundaries();
    [TestCase] procedure test_prefixed_IRI_object();
    [TestCase] procedure test_prefixed_IRI_predicate();
    [TestCase] procedure test_prefixed_name_datatype();
    [TestCase] procedure test_repeated_semis_at_end();
    [TestCase] procedure test_repeated_semis_not_at_endNT();
    [TestCase] procedure test_repeated_semis_not_at_end();
    [TestCase] procedure test_reserved_escaped_localNameNT();
//    procedure test_reserved_escaped_localName();
    [TestCase] procedure test_sole_blankNodePropertyList();
    [TestCase] procedure test_SPARQL_style_base();
    [TestCase] procedure test_SPARQL_style_prefix();
    [TestCase] procedure test_turtle_eval_bad_01();
    [TestCase] procedure test_turtle_eval_bad_02();
    [TestCase] procedure test_turtle_eval_bad_03();
//    procedure test_turtle_eval_bad_04();
    [TestCase] procedure test_turtle_eval_struct_01NT();
    [TestCase] procedure test_turtle_eval_struct_01();
    [TestCase] procedure test_turtle_eval_struct_02NT();
    [TestCase] procedure test_turtle_eval_struct_02();
    [TestCase] procedure test_turtle_subm_01NT();
    [TestCase] procedure test_turtle_subm_01();
    [TestCase] procedure test_turtle_subm_02NT();
    [TestCase] procedure test_turtle_subm_02();
    [TestCase] procedure test_turtle_subm_03NT();
    [TestCase] procedure test_turtle_subm_03();
    [TestCase] procedure test_turtle_subm_04NT();
    [TestCase] procedure test_turtle_subm_04();
    [TestCase] procedure test_turtle_subm_05NT();
    [TestCase] procedure test_turtle_subm_05();
    [TestCase] procedure test_turtle_subm_06NT();
    [TestCase] procedure test_turtle_subm_06();
    [TestCase] procedure test_turtle_subm_07NT();
    [TestCase] procedure test_turtle_subm_07();
    [TestCase] procedure test_NT();
    [TestCase] procedure test_turtle_subm_08();
    [TestCase] procedure test_turtle_subm_09NT();
    [TestCase] procedure test_turtle_subm_09();
    [TestCase] procedure test_turtle_subm_10NT();
    [TestCase] procedure test_turtle_subm_10();
    [TestCase] procedure test_turtle_subm_11NT();
    [TestCase] procedure test_turtle_subm_11();
    [TestCase] procedure test_turtle_subm_12NT();
    [TestCase] procedure test_turtle_subm_12();
    [TestCase] procedure test_turtle_subm_13NT();
    [TestCase] procedure test_turtle_subm_13();
    [TestCase] procedure test_turtle_subm_14NT();
    [TestCase] procedure test_turtle_subm_14();
    [TestCase] procedure test_turtle_subm_15NT();
    [TestCase] procedure test_turtle_subm_15();
    [TestCase] procedure test_turtle_subm_16NT();
    [TestCase] procedure test_turtle_subm_16();
    [TestCase] procedure test_turtle_subm_17NT();
    [TestCase] procedure test_turtle_subm_17();
    [TestCase] procedure test_turtle_subm_18NT();
    [TestCase] procedure test_turtle_subm_18();
    [TestCase] procedure test_turtle_subm_19NT();
    [TestCase] procedure test_turtle_subm_19();
    [TestCase] procedure test_turtle_subm_20NT();
    [TestCase] procedure test_turtle_subm_20();
    [TestCase] procedure test_turtle_subm_21NT();
    [TestCase] procedure test_turtle_subm_21();
    [TestCase] procedure test_turtle_subm_22NT();
    [TestCase] procedure test_turtle_subm_22();
    [TestCase] procedure test_turtle_subm_23NT();
    [TestCase] procedure test_turtle_subm_23();
    [TestCase] procedure test_turtle_subm_24NT();
    [TestCase] procedure test_turtle_subm_24();
    [TestCase] procedure test_turtle_subm_25NT();
    [TestCase] procedure test_turtle_subm_25();
    [TestCase] procedure test_turtle_subm_26NT();
    [TestCase] procedure test_turtle_subm_26();
    [TestCase] procedure test_turtle_subm_27NT();
    [TestCase] procedure test_turtle_subm_27();
    [TestCase] procedure test_turtle_syntax_bad_base_01();
    [TestCase] procedure test_turtle_syntax_bad_base_02();
    [TestCase] procedure test_turtle_syntax_bad_base_03();
    [TestCase] procedure test_turtle_syntax_bad_esc_01();
    [TestCase] procedure test_turtle_syntax_bad_esc_02();
    [TestCase] procedure test_turtle_syntax_bad_esc_03();
    [TestCase] procedure test_turtle_syntax_bad_esc_04();
    [TestCase] procedure test_turtle_syntax_bad_kw_01();
    [TestCase] procedure test_turtle_syntax_bad_kw_02();
    [TestCase] procedure test_turtle_syntax_bad_kw_03();
    [TestCase] procedure test_turtle_syntax_bad_kw_04();
    [TestCase] procedure test_turtle_syntax_bad_kw_05();
    [TestCase] procedure test_turtle_syntax_bad_lang_01();
    [TestCase] procedure test_turtle_syntax_bad_LITERAL2_with_langtag_and_datatype();
    [TestCase] procedure test_turtle_syntax_bad_ln_dash_start();
    [TestCase] procedure test_turtle_syntax_bad_ln_escape();
    [TestCase] procedure test_turtle_syntax_bad_ln_escape_start();
    [TestCase] procedure test_turtle_syntax_bad_missing_ns_dot_end();
    [TestCase] procedure test_turtle_syntax_bad_missing_ns_dot_start();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_01();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_02();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_03();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_04();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_05();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_07();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_08();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_09();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_10();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_11();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_12();
    [TestCase] procedure test_turtle_syntax_bad_n3_extras_13();
    [TestCase] procedure test_turtle_syntax_bad_ns_dot_end();
    [TestCase] procedure test_turtle_syntax_bad_ns_dot_start();
    [TestCase] procedure test_turtle_syntax_bad_num_01();
    [TestCase] procedure test_turtle_syntax_bad_num_02();
    [TestCase] procedure test_turtle_syntax_bad_num_03();
    [TestCase] procedure test_turtle_syntax_bad_num_04();
    [TestCase] procedure test_turtle_syntax_bad_num_05();
    [TestCase] procedure test_turtle_syntax_bad_number_dot_in_anon();
    [TestCase] procedure test_turtle_syntax_bad_pname_01();
    [TestCase] procedure test_turtle_syntax_bad_pname_02();
    [TestCase] procedure test_turtle_syntax_bad_prefix_01();
    [TestCase] procedure test_turtle_syntax_bad_prefix_02();
    [TestCase] procedure test_turtle_syntax_bad_prefix_03();
    [TestCase] procedure test_turtle_syntax_bad_prefix_04();
    [TestCase] procedure test_turtle_syntax_bad_prefix_05();
    [TestCase] procedure test_turtle_syntax_bad_string_01();
    [TestCase] procedure test_turtle_syntax_bad_string_02();
    [TestCase] procedure test_turtle_syntax_bad_string_03();
    [TestCase] procedure test_turtle_syntax_bad_string_04();
    [TestCase] procedure test_turtle_syntax_bad_string_05();
    [TestCase] procedure test_turtle_syntax_bad_string_06();
    [TestCase] procedure test_turtle_syntax_bad_string_07();
    [TestCase] procedure test_turtle_syntax_bad_struct_01();
    [TestCase] procedure test_turtle_syntax_bad_struct_02();
    [TestCase] procedure test_turtle_syntax_bad_struct_03();
    [TestCase] procedure test_turtle_syntax_bad_struct_04();
    [TestCase] procedure test_turtle_syntax_bad_struct_05();
    [TestCase] procedure test_turtle_syntax_bad_struct_06();
    [TestCase] procedure test_turtle_syntax_bad_struct_07();
    [TestCase] procedure test_turtle_syntax_bad_struct_08();
    [TestCase] procedure test_turtle_syntax_bad_struct_09();
    [TestCase] procedure test_turtle_syntax_bad_struct_10();
    [TestCase] procedure test_turtle_syntax_bad_struct_11();
    [TestCase] procedure test_turtle_syntax_bad_struct_12();
    [TestCase] procedure test_turtle_syntax_bad_struct_13();
    [TestCase] procedure test_turtle_syntax_bad_struct_14();
    [TestCase] procedure test_turtle_syntax_bad_struct_15();
    [TestCase] procedure test_turtle_syntax_bad_struct_16();
    [TestCase] procedure test_turtle_syntax_bad_struct_17();
    [TestCase] procedure test_turtle_syntax_bad_uri_02();
    [TestCase] procedure test_turtle_syntax_bad_uri_03();
    [TestCase] procedure test_turtle_syntax_base_01();
    [TestCase] procedure test_turtle_syntax_base_02();
    [TestCase] procedure test_turtle_syntax_base_03();
    [TestCase] procedure test_turtle_syntax_base_04();
    [TestCase] procedure test_turtle_syntax_blank_label();
    [TestCase] procedure test_turtle_syntax_bnode_01();
    [TestCase] procedure test_turtle_syntax_bnode_02();
    [TestCase] procedure test_turtle_syntax_bnode_03();
    [TestCase] procedure test_turtle_syntax_bnode_04();
    [TestCase] procedure test_turtle_syntax_bnode_05();
    [TestCase] procedure test_turtle_syntax_bnode_06();
    [TestCase] procedure test_turtle_syntax_bnode_07();
    [TestCase] procedure test_turtle_syntax_bnode_08();
    [TestCase] procedure test_turtle_syntax_bnode_09();
    [TestCase] procedure test_turtle_syntax_bnode_10();
    [TestCase] procedure test_turtle_syntax_datatypes_01();
    [TestCase] procedure test_turtle_syntax_datatypes_02();
    [TestCase] procedure test_turtle_syntax_file_01();
    [TestCase] procedure test_turtle_syntax_file_02();
    [TestCase] procedure test_turtle_syntax_file_03();
    [TestCase] procedure test_turtle_syntax_kw_01();
    [TestCase] procedure test_turtle_syntax_kw_02();
    [TestCase] procedure test_turtle_syntax_kw_03();
    [TestCase] procedure test_turtle_syntax_lists_01();
    [TestCase] procedure test_turtle_syntax_lists_02();
    [TestCase] procedure test_turtle_syntax_ln_dots();
    [TestCase] procedure test_turtle_syntax_ns_dots();
    [TestCase] procedure test_turtle_syntax_number_01();
    [TestCase] procedure test_turtle_syntax_number_02();
    [TestCase] procedure test_turtle_syntax_number_03();
    [TestCase] procedure test_turtle_syntax_number_04();
    [TestCase] procedure test_turtle_syntax_number_06();
    [TestCase] procedure test_turtle_syntax_number_07();
    [TestCase] procedure test_turtle_syntax_number_09();
    [TestCase] procedure test_turtle_syntax_number_10();
    [TestCase] procedure test_turtle_syntax_number_11();
    [TestCase] procedure test_turtle_syntax_pname_esc_01();
    [TestCase] procedure test_turtle_syntax_pname_esc_02();
    [TestCase] procedure test_turtle_syntax_pname_esc_03();
    [TestCase] procedure test_turtle_syntax_prefix_01();
    [TestCase] procedure test_turtle_syntax_prefix_03();
    [TestCase] procedure test_turtle_syntax_prefix_04();
    [TestCase] procedure test_turtle_syntax_prefix_07();
    [TestCase] procedure test_turtle_syntax_prefix_08();
    [TestCase] procedure test_turtle_syntax_prefix_09();
    [TestCase] procedure test_turtle_syntax_str_esc_01();
    [TestCase] procedure test_turtle_syntax_str_esc_02();
    [TestCase] procedure test_turtle_syntax_str_esc_03();
    [TestCase] procedure test_turtle_syntax_string_01();
    [TestCase] procedure test_turtle_syntax_string_02();
    [TestCase] procedure test_turtle_syntax_string_03();
    [TestCase] procedure test_turtle_syntax_string_04();
    [TestCase] procedure test_turtle_syntax_string_05();
    [TestCase] procedure test_turtle_syntax_string_06();
    [TestCase] procedure test_turtle_syntax_string_07();
    [TestCase] procedure test_turtle_syntax_string_08();
    [TestCase] procedure test_turtle_syntax_string_09();
    [TestCase] procedure test_turtle_syntax_string_10();
    [TestCase] procedure test_turtle_syntax_string_11();
    [TestCase] procedure test_turtle_syntax_struct_01();
    [TestCase] procedure test_turtle_syntax_struct_02();
    [TestCase] procedure test_turtle_syntax_struct_03();
    [TestCase] procedure test_turtle_syntax_struct_04();
    [TestCase] procedure test_turtle_syntax_struct_05();
    [TestCase] procedure test_turtle_syntax_uri_01();
    [TestCase] procedure test_turtle_syntax_uri_02();
    [TestCase] procedure test_turtle_syntax_uri_03();
    [TestCase] procedure test_turtle_syntax_uri_04();
    [TestCase] procedure test_two_LITERAL_LONG2sNT();
    [TestCase] procedure test_two_LITERAL_LONG2s();
    [TestCase] procedure test_underscore_in_localNameNT();
    [TestCase] procedure test_underscore_in_localName();
    [TestCase] procedure test_anonymous_blank_node_object();
    [TestCase] procedure test_anonymous_blank_node_subject();
    [TestCase] procedure test_bareword_a_predicateNT();
    [TestCase] procedure test_bareword_a_predicate();
    [TestCase] procedure test_bareword_decimalNT();
    [TestCase] procedure test_bareword_decimal();
    [TestCase] procedure test_bareword_doubleNT();
    [TestCase] procedure test_bareword_double();
    [TestCase] procedure test_bareword_integer();
    [TestCase] procedure test_blankNodePropertyList_as_objectNT();
    [TestCase] procedure test_blankNodePropertyList_as_object();
    [TestCase] procedure test_blankNodePropertyList_as_subjectNT();
    [TestCase] procedure test_blankNodePropertyList_containing_collectionNT();
    [TestCase] procedure test_blankNodePropertyList_containing_collection();
    [TestCase] procedure test_blankNodePropertyList_with_multiple_triplesNT();
    [TestCase] procedure test_blankNodePropertyList_with_multiple_triples();
    [TestCase] procedure test_collection_objectNT();
    [TestCase] procedure test_collection_object();
    [TestCase] procedure test_collection_subjectNT();
    [TestCase] procedure test_collection_subject();
    [TestCase] procedure test_comment_following_localName();
    [TestCase] procedure test_comment_following_PNAME_NSNT();
    [TestCase] procedure test_comment_following_PNAME_NS();
    [TestCase] procedure test__default_namespace_IRI();
  End;

  [TextFixture]
  TTurtleResourceTests = Class (TObject)
  Private
    procedure parseResource(filename : String);
  Published
    [TestCase] procedure test_audit_event_example_pixQuery();
    [TestCase] procedure test_audit_event_example_media();
    [TestCase] procedure test_audit_event_example_logout();
    [TestCase] procedure test_audit_event_example_login();
    [TestCase] procedure test_appointmentresponse_example();
    [TestCase] procedure test_appointmentresponse_example_req();
    [TestCase] procedure test_appointment_example2doctors();
    [TestCase] procedure test_appointment_example();
    [TestCase] procedure test_appointment_example_request();
    [TestCase] procedure test_allergyintolerance_medication();
    [TestCase] procedure test_allergyintolerance_fishallergy();
    [TestCase] procedure test_allergyintolerance_example();
    [TestCase] procedure test_account_example();
    [TestCase] procedure test_xds_example();
    [TestCase] procedure test_visionprescription_example();
    [TestCase] procedure test_visionprescription_example_1();
    [TestCase] procedure test_valueset_ucum_common();
    [TestCase] procedure test_valueset_nhin_purposeofuse();
    [TestCase] procedure test_valueset_example();
    [TestCase] procedure test_valueset_example_yesnodontknow();
    [TestCase] procedure test_valueset_example_intensional();
    [TestCase] procedure test_valueset_example_expansion();
    [TestCase] procedure test_valueset_cpt_all();
    [TestCase] procedure test_testscript_example();
    [TestCase] procedure test_testscript_example_rule();
    [TestCase] procedure test_supplydelivery_example();
    [TestCase] procedure test_substance_example();
    [TestCase] procedure test_substance_example_silver_nitrate_product();
    [TestCase] procedure test_substance_example_f203_potassium();
    [TestCase] procedure test_substance_example_f202_staphylococcus();
    [TestCase] procedure test_substance_example_f201_dust();
    [TestCase] procedure test_substance_example_amoxicillin_clavulanate();
    [TestCase] procedure test_subscription_example();
    [TestCase] procedure test_subscription_example_error();
    [TestCase] procedure test_structuremap_example();
    [TestCase] procedure test_structuredefinition_example();
    [TestCase] procedure test_specimen_example();
    [TestCase] procedure test_specimen_example_urine();
    [TestCase] procedure test_specimen_example_isolate();
    [TestCase] procedure test_slot_example();
    [TestCase] procedure test_slot_example_unavailable();
    [TestCase] procedure test_slot_example_tentative();
    [TestCase] procedure test_slot_example_busy();
    [TestCase] procedure test_sequence_example();
    [TestCase] procedure test_searchparameter_example();
    [TestCase] procedure test_searchparameter_example_extension();
    [TestCase] procedure test_schedule_example();
    [TestCase] procedure test_riskassessment_example();
    [TestCase] procedure test_riskassessment_example_prognosis();
    [TestCase] procedure test_riskassessment_example_population();
    [TestCase] procedure test_riskassessment_example_cardiac();
    [TestCase] procedure test_relatedperson_example();
    [TestCase] procedure test_relatedperson_example_peter();
    [TestCase] procedure test_relatedperson_example_f002_ariadne();
    [TestCase] procedure test_relatedperson_example_f001_sarah();
    [TestCase] procedure test_provenance_example();
    [TestCase] procedure test_provenance_example_sig();
    [TestCase] procedure test_processresponse_example();
    [TestCase] procedure test_processrequest_example();
    [TestCase] procedure test_processrequest_example_status();
    [TestCase] procedure test_processrequest_example_reverse();
    [TestCase] procedure test_processrequest_example_reprocess();
    [TestCase] procedure test_processrequest_example_poll_specific();
    [TestCase] procedure test_processrequest_example_poll_payrec();
    [TestCase] procedure test_processrequest_example_poll_inclusive();
    [TestCase] procedure test_processrequest_example_poll_exclusive();
    [TestCase] procedure test_processrequest_example_poll_eob();
    [TestCase] procedure test_procedure_example();
    [TestCase] procedure test_procedure_example_implant();
    [TestCase] procedure test_procedure_example_f201_tpf();
    [TestCase] procedure test_procedure_example_f004_tracheotomy();
    [TestCase] procedure test_procedure_example_f003_abscess();
    [TestCase] procedure test_procedure_example_f002_lung();
    [TestCase] procedure test_procedure_example_f001_heart();
    [TestCase] procedure test_procedure_example_biopsy();
    [TestCase] procedure test_practitionerrole_example();
    [TestCase] procedure test_practitioner_examples_general();
    [TestCase] procedure test_practitioner_example();
    [TestCase] procedure test_practitioner_example_xcda1();
    [TestCase] procedure test_practitioner_example_xcda_author();
    [TestCase] procedure test_practitioner_example_f204_ce();
    [TestCase] procedure test_practitioner_example_f203_jvg();
    [TestCase] procedure test_practitioner_example_f202_lm();
    [TestCase] procedure test_practitioner_example_f201_ab();
    [TestCase] procedure test_practitioner_example_f007_sh();
    [TestCase] procedure test_practitioner_example_f006_rvdb();
    [TestCase] procedure test_practitioner_example_f005_al();
    [TestCase] procedure test_practitioner_example_f004_rb();
    [TestCase] procedure test_practitioner_example_f003_mv();
    [TestCase] procedure test_practitioner_example_f002_pv();
    [TestCase] procedure test_practitioner_example_f001_evdb();
    [TestCase] procedure test_person_provider_directory();
    [TestCase] procedure test_person_patient_portal();
    [TestCase] procedure test_person_grahame();
    [TestCase] procedure test_person_example();
    [TestCase] procedure test_person_example_f002_ariadne();
    [TestCase] procedure test_paymentreconciliation_example();
    [TestCase] procedure test_paymentnotice_example();
    [TestCase] procedure test_patient_glossy_example();
    [TestCase] procedure test_patient_examples_general();
    [TestCase] procedure test_patient_examples_cypress_template();
    [TestCase] procedure test_patient_example();
    [TestCase] procedure test_patient_example_xds();
    [TestCase] procedure test_patient_example_xcda();
    [TestCase] procedure test_patient_example_proband();
    [TestCase] procedure test_patient_example_ihe_pcd();
    [TestCase] procedure test_patient_example_f201_roel();
    [TestCase] procedure test_patient_example_f001_pieter();
    [TestCase] procedure test_patient_example_dicom();
    [TestCase] procedure test_patient_example_d();
    [TestCase] procedure test_patient_example_c();
    [TestCase] procedure test_patient_example_b();
    [TestCase] procedure test_patient_example_animal();
    [TestCase] procedure test_patient_example_a();
    [TestCase] procedure test_parameters_example();
    [TestCase] procedure test_organization_example();
    [TestCase] procedure test_organization_example_lab();
    [TestCase] procedure test_organization_example_insurer();
    [TestCase] procedure test_organization_example_good_health_care();
    [TestCase] procedure test_organization_example_gastro();
    [TestCase] procedure test_organization_example_f203_bumc();
    [TestCase] procedure test_organization_example_f201_aumc();
    [TestCase] procedure test_organization_example_f003_burgers_ENT();
    [TestCase] procedure test_organization_example_f002_burgers_card();
    [TestCase] procedure test_organization_example_f001_burgers();
    [TestCase] procedure test_operationoutcome_example();
    [TestCase] procedure test_operationoutcome_example_validationfail();
    [TestCase] procedure test_operationoutcome_example_searchfail();
    [TestCase] procedure test_operationoutcome_example_exception();
    [TestCase] procedure test_operationoutcome_example_break_the_glass();
    [TestCase] procedure test_operationoutcome_example_allok();
    [TestCase] procedure test_operationdefinition_example();
    [TestCase] procedure test_observation_example();
    [TestCase] procedure test_observation_example_unsat();
    [TestCase] procedure test_observation_example_satO2();
    [TestCase] procedure test_observation_example_sample_data();
    [TestCase] procedure test_observation_example_glasgow();
    [TestCase] procedure test_observation_example_glasgow_qa();
    [TestCase] procedure test_observation_example_genetics_5();
    [TestCase] procedure test_observation_example_genetics_4();
    [TestCase] procedure test_observation_example_genetics_3();
    [TestCase] procedure test_observation_example_genetics_2();
    [TestCase] procedure test_observation_example_genetics_1();
    [TestCase] procedure test_observation_example_f206_staphylococcus();
    [TestCase] procedure test_observation_example_f205_egfr();
    [TestCase] procedure test_observation_example_f204_creatinine();
    [TestCase] procedure test_observation_example_f203_bicarbonate();
    [TestCase] procedure test_observation_example_f202_temperature();
    [TestCase] procedure test_observation_example_f005_hemoglobin();
    [TestCase] procedure test_observation_example_f004_erythrocyte();
    [TestCase] procedure test_observation_example_f003_co2();
    [TestCase] procedure test_observation_example_f002_excess();
    [TestCase] procedure test_observation_example_f001_glucose();
    [TestCase] procedure test_observation_example_bloodpressure();
    [TestCase] procedure test_observation_example_bloodpressure_cancel();
    [TestCase] procedure test_nutritionorder_example_texture_modified();
    [TestCase] procedure test_nutritionorder_example_renaldiet();
    [TestCase] procedure test_nutritionorder_example_pureeddiet();
    [TestCase] procedure test_nutritionorder_example_pureeddiet_simple();
    [TestCase] procedure test_nutritionorder_example_proteinsupplement();
    [TestCase] procedure test_nutritionorder_example_infantenteral();
    [TestCase] procedure test_nutritionorder_example_fiberrestricteddiet();
    [TestCase] procedure test_nutritionorder_example_enteralcontinuous();
    [TestCase] procedure test_nutritionorder_example_enteralbolus();
    [TestCase] procedure test_nutritionorder_example_energysupplement();
    [TestCase] procedure test_nutritionorder_example_diabeticsupplement();
    [TestCase] procedure test_nutritionorder_example_diabeticdiet();
    [TestCase] procedure test_nutritionorder_example_cardiacdiet();
    [TestCase] procedure test_namingsystem_registry();
    [TestCase] procedure test_namingsystem_example();
    [TestCase] procedure test_namingsystem_example_id();
    [TestCase] procedure test_messageheader_example();
    [TestCase] procedure test_message_response_link();
    [TestCase] procedure test_message_request_link();
    [TestCase] procedure test_medicationstatementexample7();
    [TestCase] procedure test_medicationstatementexample6();
    [TestCase] procedure test_medicationstatementexample5();
    [TestCase] procedure test_medicationstatementexample4();
    [TestCase] procedure test_medicationstatementexample2();
    [TestCase] procedure test_medicationstatementexample1();
    [TestCase] procedure test_medicationrequestexample2();
    [TestCase] procedure test_medicationrequestexample1();
    [TestCase] procedure test_medicationexample15();
    [TestCase] procedure test_medicationexample1();
    [TestCase] procedure test_medicationdispenseexample8();
    [TestCase] procedure test_medicationadministrationexample3();
    [TestCase] procedure test_medication_example_f203_paracetamol();
    [TestCase] procedure test_media_example();
    [TestCase] procedure test_media_example_sound();
    [TestCase] procedure test_media_example_dicom();
    [TestCase] procedure test_measurereport_cms146_cat3_example();
    [TestCase] procedure test_measurereport_cms146_cat2_example();
    [TestCase] procedure test_measurereport_cms146_cat1_example();
    [TestCase] procedure test_measure_exclusive_breastfeeding();
    [TestCase] procedure test_location_example();
    [TestCase] procedure test_location_example_ukpharmacy();
    [TestCase] procedure test_location_example_room();
    [TestCase] procedure test_location_example_patients_home();
    [TestCase] procedure test_location_example_hl7hq();
    [TestCase] procedure test_location_example_ambulance();
    [TestCase] procedure test_list_example();
    [TestCase] procedure test_list_example_medlist();
    [TestCase] procedure test_list_example_familyhistory_f201_roel();
    [TestCase] procedure test_list_example_empty();
    [TestCase] procedure test_list_example_allergies();
    [TestCase] procedure test_linkage_example();
    [TestCase] procedure test_library_exclusive_breastfeeding_cqm_logic();
    [TestCase] procedure test_library_exclusive_breastfeeding_cds_logic();
    [TestCase] procedure test_library_example();
    [TestCase] procedure test_library_cms146_example();
    [TestCase] procedure test_implementationguide_example();
    [TestCase] procedure test_immunizationrecommendation_example();
    [TestCase] procedure test_immunization_example();
    [TestCase] procedure test_immunization_example_refused();
    [TestCase] procedure test_imagingstudy_example();
    [TestCase] procedure test_healthcareservice_example();
    [TestCase] procedure test_guidanceresponse_example();
    [TestCase] procedure test_group_example();
    [TestCase] procedure test_group_example_member();
    [TestCase] procedure test_goal_example();
    [TestCase] procedure test_flag_example();
    [TestCase] procedure test_flag_example_encounter();
    [TestCase] procedure test_familymemberhistory_example();
    [TestCase] procedure test_familymemberhistory_example_mother();
    [TestCase] procedure test_explanationofbenefit_example();
    [TestCase] procedure test_episodeofcare_example();
    [TestCase] procedure test_enrollmentresponse_example();
    [TestCase] procedure test_enrollmentrequest_example();
    [TestCase] procedure test_endpoint_example();
    [TestCase] procedure test_encounter_example();
    [TestCase] procedure test_encounter_example_xcda();
    [TestCase] procedure test_encounter_example_home();
    [TestCase] procedure test_encounter_example_f203_20130311();
    [TestCase] procedure test_encounter_example_f202_20130128();
    [TestCase] procedure test_encounter_example_f201_20130404();
    [TestCase] procedure test_encounter_example_f003_abscess();
    [TestCase] procedure test_encounter_example_f002_lung();
    [TestCase] procedure test_encounter_example_f001_heart();
    [TestCase] procedure test_eligibilityresponse_example();
    [TestCase] procedure test_eligibilityrequest_example();
    [TestCase] procedure test_documentreference_example();
    [TestCase] procedure test_documentmanifest_fm_attachment();
    [TestCase] procedure test_document_example_dischargesummary();
    [TestCase] procedure test_diagnosticreport_micro1();
    [TestCase] procedure test_diagnosticreport_hla_genetics_results_example();
    [TestCase] procedure test_diagnosticreport_genetics_comprehensive_bone_marrow_report();
    [TestCase] procedure test_diagnosticreport_examples_general();
    [TestCase] procedure test_diagnosticreport_example_ultrasound();
    [TestCase] procedure test_diagnosticreport_example_lipids();
    [TestCase] procedure test_diagnosticreport_example_ghp();
    [TestCase] procedure test_diagnosticreport_example_f202_bloodculture();
    [TestCase] procedure test_diagnosticreport_example_f201_brainct();
    [TestCase] procedure test_diagnosticreport_example_f001_bloodexam();
    [TestCase] procedure test_diagnosticreport_example_dxa();
    [TestCase] procedure test_deviceusestatement_example();
    [TestCase] procedure test_devicemetric_example();
    [TestCase] procedure test_devicecomponent_example();
    [TestCase] procedure test_devicecomponent_example_prodspec();
    [TestCase] procedure test_device_example();
    [TestCase] procedure test_device_example_udi1();
    [TestCase] procedure test_device_example_software();
    [TestCase] procedure test_device_example_pacemaker();
    [TestCase] procedure test_device_example_ihe_pcd();
    [TestCase] procedure test_device_example_f001_feedingtube();
    [TestCase] procedure test_detectedissue_example();
    [TestCase] procedure test_detectedissue_example_lab();
    [TestCase] procedure test_detectedissue_example_dup();
    [TestCase] procedure test_detectedissue_example_allergy();
    [TestCase] procedure test_coverage_example();
    [TestCase] procedure test_coverage_example_2();
    [TestCase] procedure test_contract_example();
    [TestCase] procedure test_condition_example2();
    [TestCase] procedure test_condition_example();
    [TestCase] procedure test_condition_example_stroke();
    [TestCase] procedure test_condition_example_f205_infection();
    [TestCase] procedure test_condition_example_f204_renal();
    [TestCase] procedure test_condition_example_f203_sepsis();
    [TestCase] procedure test_condition_example_f202_malignancy();
    [TestCase] procedure test_condition_example_f201_fever();
    [TestCase] procedure test_condition_example_f003_abscess();
    [TestCase] procedure test_condition_example_f002_lung();
    [TestCase] procedure test_condition_example_f001_heart();
    [TestCase] procedure test_conceptmap_example();
    [TestCase] procedure test_conceptmap_example_specimen_type();
    [TestCase] procedure test_conceptmap_103();
    [TestCase] procedure test_composition_example();
    [TestCase] procedure test_communicationrequest_example();
    [TestCase] procedure test_communication_example();
    [TestCase] procedure test_codesystem_nhin_purposeofuse();
    [TestCase] procedure test_codesystem_example();
    [TestCase] procedure test_clinicalimpression_example();
    [TestCase] procedure test_claimresponse_example();
    [TestCase] procedure test_claim_example();
    [TestCase] procedure test_claim_example_vision();
    [TestCase] procedure test_claim_example_vision_glasses();
    [TestCase] procedure test_claim_example_professional();
    [TestCase] procedure test_claim_example_pharmacy();
    [TestCase] procedure test_claim_example_oral_orthoplan();
    [TestCase] procedure test_claim_example_oral_identifier();
    [TestCase] procedure test_claim_example_oral_contained();
    [TestCase] procedure test_claim_example_oral_contained_identifier();
    [TestCase] procedure test_claim_example_oral_average();
    [TestCase] procedure test_claim_example_institutional();
    [TestCase] procedure test_careteam_example();
    [TestCase] procedure test_careplan_example();
    [TestCase] procedure test_careplan_example_pregnancy();
    [TestCase] procedure test_careplan_example_integrated();
    [TestCase] procedure test_careplan_example_GPVisit();
    [TestCase] procedure test_careplan_example_f203_sepsis();
    [TestCase] procedure test_careplan_example_f202_malignancy();
    [TestCase] procedure test_careplan_example_f201_renal();
    [TestCase] procedure test_careplan_example_f003_pharynx();
    [TestCase] procedure test_careplan_example_f002_lung();
    [TestCase] procedure test_careplan_example_f001_heart();
    [TestCase] procedure test_bundle_transaction();
    [TestCase] procedure test_bundle_response();
    [TestCase] procedure test_bundle_example();
    [TestCase] procedure test_binary_f006();
    [TestCase] procedure test_binary_example();
    [TestCase] procedure test_basic_example2();
    [TestCase] procedure test_basic_example();
    [TestCase] procedure test_basic_example_narrative();
    [TestCase] procedure test_auditevent_example();
    [TestCase] procedure test_auditevent_example_disclosure();
    [TestCase] procedure test_audit_event_example_vread();
    [TestCase] procedure test_audit_event_example_search();
  End;

function CheckTurtleIsSame(src1, src2 : String; var msg : string) : boolean;

implementation

{ TTurtleTests }

procedure TTurtleTests.parseTtl(filename: String; ok: boolean);
var
  s{, p, m} : String;
  ttl : TTurtleDocument;
begin
  ttl := nil;
  s := fileToString('C:\work\org.hl7.fhir\build\tests\turtle\'+filename, TEncoding.UTF8);
  try
    ttl := TTurtleParser.parse(s);
    try
      Assert.IsNotNull(ttl);
      Assert.IsTrue(ok);
    finally
      ttl.Free;
    end;
  except
    Assert.IsTrue(not ok);
  end;
end;

procedure TTurtleTests.test_double_lower_case_e1;
begin
  parseTtl('double_lower_case_e.nt', true);
end;

procedure TTurtleTests.test_double_lower_case_e2();
begin
  parseTtl('double_lower_case_e.ttl', true);
end;

procedure TTurtleTests.test_empty_collection1();
begin
  parseTtl('empty_collection.nt', true);
end;

procedure TTurtleTests.test_empty_collection2();
begin
  parseTtl('empty_collection.ttl', true);
end;

procedure TTurtleTests.test_first1();
begin
  parseTtl('first.nt', true);
end;

//procedure TTurtleTests.test_first2();
//begin
////     parseTtl('first.ttl', true);
//end;

procedure TTurtleTests.test_HYPHEN_MINUS_in_localNameNT();
begin
  parseTtl('HYPHEN_MINUS_in_localName.nt', true);
end;

procedure TTurtleTests.test_HYPHEN_MINUS_in_localName();
begin
  parseTtl('HYPHEN_MINUS_in_localName.ttl', true);
end;

procedure TTurtleTests.test_IRI_spoNT();
begin
  parseTtl('IRI_spo.nt', true);
end;

procedure TTurtleTests.test_IRI_subject();
begin
  parseTtl('IRI_subject.ttl', true);
end;

procedure TTurtleTests.test_IRI_with_all_punctuationNT();
begin
  parseTtl('IRI_with_all_punctuation.nt', true);
end;

procedure TTurtleTests.test_IRI_with_all_punctuation();
begin
  parseTtl('IRI_with_all_punctuation.ttl', true);
end;

procedure TTurtleTests.test_IRI_with_eight_digit_numeric_escape();
begin
  parseTtl('IRI_with_eight_digit_numeric_escape.ttl', true);
end;

procedure TTurtleTests.test_IRI_with_four_digit_numeric_escape();
begin
  parseTtl('IRI_with_four_digit_numeric_escape.ttl', true);
end;

procedure TTurtleTests.test_IRIREF_datatypeNT();
begin
  parseTtl('IRIREF_datatype.nt', true);
end;

procedure TTurtleTests.test_IRIREF_datatype();
begin
  parseTtl('IRIREF_datatype.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_objectNT();
begin
  parseTtl('labeled_blank_node_object.nt', true);
end;

procedure TTurtleTests.test_labeled_blank_node_object();
begin
  parseTtl('labeled_blank_node_object.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_subjectNT();
begin
  parseTtl('labeled_blank_node_subject.nt', true);
end;

procedure TTurtleTests.test_labeled_blank_node_subject();
begin
  parseTtl('labeled_blank_node_subject.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_with_leading_digit();
begin
  parseTtl('labeled_blank_node_with_leading_digit.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_with_leading_underscore();
begin
  parseTtl('labeled_blank_node_with_leading_underscore.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_with_non_leading_extras();
begin
  parseTtl('labeled_blank_node_with_non_leading_extras.ttl', true);
end;

procedure TTurtleTests.test_labeled_blank_node_with_PN_CHARS_BASE_character_boundaries();
begin
  parseTtl('labeled_blank_node_with_PN_CHARS_BASE_character_boundaries.ttl', false);
end;

procedure TTurtleTests.test_langtagged_LONG();
begin
  parseTtl('langtagged_LONG.ttl', true);
end;

procedure TTurtleTests.test_langtagged_LONG_with_subtagNT();
begin
  parseTtl('langtagged_LONG_with_subtag.nt', true);
end;

procedure TTurtleTests.test_langtagged_LONG_with_subtag();
begin
  parseTtl('langtagged_LONG_with_subtag.ttl', true);
end;

procedure TTurtleTests.test_langtagged_non_LONGNT();
begin
  parseTtl('langtagged_non_LONG.nt', true);
end;

procedure TTurtleTests.test_langtagged_non_LONG();
begin
  parseTtl('langtagged_non_LONG.ttl', true);
end;

procedure TTurtleTests.test_lantag_with_subtagNT();
begin
  parseTtl('lantag_with_subtag.nt', true);
end;

procedure TTurtleTests.test_lantag_with_subtag();
begin
  parseTtl('lantag_with_subtag.ttl', true);
end;

procedure TTurtleTests.test_lastNT();
begin
  parseTtl('last.nt', true);
end;

procedure TTurtleTests.test_last();
begin
  parseTtl('last.ttl', false);
end;

procedure TTurtleTests.test_literal_falseNT();
begin
  parseTtl('literal_false.nt', true);
end;

procedure TTurtleTests.test_literal_false();
begin
  parseTtl('literal_false.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1();
begin
  parseTtl('LITERAL_LONG1.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_ascii_boundariesNT();
begin
  parseTtl('LITERAL_LONG1_ascii_boundaries.nt', false);
end;

procedure TTurtleTests.test_LITERAL_LONG1_ascii_boundaries();
begin
  parseTtl('LITERAL_LONG1_ascii_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_with_1_squoteNT();
begin
  parseTtl('LITERAL_LONG1_with_1_squote.nt', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_with_1_squote();
begin
  parseTtl('LITERAL_LONG1_with_1_squote.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_with_2_squotesNT();
begin
  parseTtl('LITERAL_LONG1_with_2_squotes.nt', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_with_2_squotes();
begin
  parseTtl('LITERAL_LONG1_with_2_squotes.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG1_with_UTF8_boundaries();
begin
  parseTtl('LITERAL_LONG1_with_UTF8_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2();
begin
  parseTtl('LITERAL_LONG2.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_ascii_boundariesNT();
begin
  parseTtl('LITERAL_LONG2_ascii_boundaries.nt', false);
end;

procedure TTurtleTests.test_LITERAL_LONG2_ascii_boundaries();
begin
  parseTtl('LITERAL_LONG2_ascii_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_1_squoteNT();
begin
  parseTtl('LITERAL_LONG2_with_1_squote.nt', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_1_squote();
begin
  parseTtl('LITERAL_LONG2_with_1_squote.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_2_squotesNT();
begin
  parseTtl('LITERAL_LONG2_with_2_squotes.nt', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_2_squotes();
begin
  parseTtl('LITERAL_LONG2_with_2_squotes.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_REVERSE_SOLIDUSNT();
begin
  parseTtl('LITERAL_LONG2_with_REVERSE_SOLIDUS.nt', false);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_REVERSE_SOLIDUS();
begin
  parseTtl('LITERAL_LONG2_with_REVERSE_SOLIDUS.ttl', false);
end;

procedure TTurtleTests.test_LITERAL_LONG2_with_UTF8_boundaries();
begin
  parseTtl('LITERAL_LONG2_with_UTF8_boundaries.ttl', true);
end;

procedure TTurtleTests.test_literal_trueNT();
begin
  parseTtl('literal_true.nt', true);
end;

procedure TTurtleTests.test_literal_true();
begin
  parseTtl('literal_true.ttl', true);
end;

procedure TTurtleTests.test_literal_with_BACKSPACENT();
begin
  parseTtl('literal_with_BACKSPACE.nt', false);
end;

procedure TTurtleTests.test_literal_with_BACKSPACE();
begin
  parseTtl('literal_with_BACKSPACE.ttl', true);
end;

procedure TTurtleTests.test_literal_with_CARRIAGE_RETURNNT();
begin
  parseTtl('literal_with_CARRIAGE_RETURN.nt', true);
end;

procedure TTurtleTests.test_literal_with_CARRIAGE_RETURN();
begin
  parseTtl('literal_with_CARRIAGE_RETURN.ttl', true);
end;

procedure TTurtleTests.test_literal_with_CHARACTER_TABULATIONNT();
begin
  parseTtl('literal_with_CHARACTER_TABULATION.nt', true);
end;

procedure TTurtleTests.test_literal_with_CHARACTER_TABULATION();
begin
  parseTtl('literal_with_CHARACTER_TABULATION.ttl', true);
end;

procedure TTurtleTests.test_literal_with_escaped_BACKSPACE();
begin
  parseTtl('literal_with_escaped_BACKSPACE.ttl', false);
end;

procedure TTurtleTests.test_literal_with_escaped_CARRIAGE_RETURN();
begin
  parseTtl('literal_with_escaped_CARRIAGE_RETURN.ttl', true);
end;

procedure TTurtleTests.test_literal_with_escaped_CHARACTER_TABULATION();
begin
  parseTtl('literal_with_escaped_CHARACTER_TABULATION.ttl', true);
end;

procedure TTurtleTests.test_literal_with_escaped_FORM_FEED();
begin
  parseTtl('literal_with_escaped_FORM_FEED.ttl', true);
end;

procedure TTurtleTests.test_literal_with_escaped_LINE_FEED();
begin
  parseTtl('literal_with_escaped_LINE_FEED.ttl', true);
end;

//procedure TTurtleTests.test_literal_with_FORM_FEEDNT();
//begin
////     parseTtl('literal_with_FORM_FEED.nt', true);
//end;

procedure TTurtleTests.test_literal_with_FORM_FEED();
begin
  parseTtl('literal_with_FORM_FEED.ttl', true);
end;

procedure TTurtleTests.test_literal_with_LINE_FEEDNT();
begin
  parseTtl('literal_with_LINE_FEED.nt', true);
end;

procedure TTurtleTests.test_literal_with_LINE_FEED();
begin
  parseTtl('literal_with_LINE_FEED.ttl', true);
end;

procedure TTurtleTests.test_literal_with_numeric_escape4NT();
begin
  parseTtl('literal_with_numeric_escape4.nt', true);
end;

procedure TTurtleTests.test_literal_with_numeric_escape4();
begin
  parseTtl('literal_with_numeric_escape4.ttl', true);
end;

procedure TTurtleTests.test_literal_with_numeric_escape8();
begin
  parseTtl('literal_with_numeric_escape8.ttl', true);
end;

procedure TTurtleTests.test_literal_with_REVERSE_SOLIDUSNT();
begin
  parseTtl('literal_with_REVERSE_SOLIDUS.nt', false);
end;

procedure TTurtleTests.test_literal_with_REVERSE_SOLIDUS();
begin
  parseTtl('literal_with_REVERSE_SOLIDUS.ttl', true);
end;

procedure TTurtleTests.test_LITERAL_with_UTF8_boundariesNT();
begin
  parseTtl('LITERAL_with_UTF8_boundaries.nt', true);
end;

procedure TTurtleTests.test_LITERAL1NT();
begin
  parseTtl('LITERAL1.nt', true);
end;

procedure TTurtleTests.test_LITERAL1();
begin
  parseTtl('LITERAL1.ttl', true);
end;

procedure TTurtleTests.test_LITERAL1_all_controlsNT();
begin
  parseTtl('LITERAL1_all_controls.nt', false);
end;

procedure TTurtleTests.test_LITERAL1_all_controls();
begin
  parseTtl('LITERAL1_all_controls.ttl', true);
end;

procedure TTurtleTests.test_LITERAL1_all_punctuationNT();
begin
  parseTtl('LITERAL1_all_punctuation.nt', true);
end;

procedure TTurtleTests.test_LITERAL1_all_punctuation();
begin
  parseTtl('LITERAL1_all_punctuation.ttl', true);
end;

//procedure TTurtleTests.test_LITERAL1_ascii_boundariesNT();
//begin
////     parseTtl('LITERAL1_ascii_boundaries.nt', true);
//end;

procedure TTurtleTests.test_LITERAL1_ascii_boundaries();
begin
  parseTtl('LITERAL1_ascii_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL1_with_UTF8_boundaries();
begin
  parseTtl('LITERAL1_with_UTF8_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL2();
begin
  parseTtl('LITERAL2.ttl', true);
end;

procedure TTurtleTests.test_LITERAL2_ascii_boundariesNT();
begin
  parseTtl('LITERAL2_ascii_boundaries.nt', false);
end;

procedure TTurtleTests.test_LITERAL2_ascii_boundaries();
begin
  parseTtl('LITERAL2_ascii_boundaries.ttl', true);
end;

procedure TTurtleTests.test_LITERAL2_with_UTF8_boundaries();
begin
  parseTtl('LITERAL2_with_UTF8_boundaries.ttl', true);
end;

procedure TTurtleTests.test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundariesNT();
begin
  parseTtl('localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries.nt', true);
end;

procedure TTurtleTests.test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries();
begin
  parseTtl('localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries.ttl', true);
end;

procedure TTurtleTests.test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundariesNT();
begin
  parseTtl('localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries.nt', true);
end;

procedure TTurtleTests.test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries();
begin
  parseTtl('localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries.ttl', false);
end;
// don't need to support property names with ':'

//procedure TTurtleTests.test_localname_with_COLONNT();
//begin
////     parseTtl('localname_with_COLON.nt', true);
//end;

//procedure TTurtleTests.test_localname_with_COLON();
//begin
////     parseTtl('localname_with_COLON.ttl', true);
//end;

procedure TTurtleTests.test_localName_with_leading_digitNT();
begin
  parseTtl('localName_with_leading_digit.nt', true);
end;

procedure TTurtleTests.test_localName_with_leading_digit();
begin
  parseTtl('localName_with_leading_digit.ttl', true);
end;

procedure TTurtleTests.test_localName_with_leading_underscoreNT();
begin
  parseTtl('localName_with_leading_underscore.nt', true);
end;

procedure TTurtleTests.test_localName_with_leading_underscore();
begin
  parseTtl('localName_with_leading_underscore.ttl', true);
end;

procedure TTurtleTests.test_localName_with_nfc_PN_CHARS_BASE_character_boundariesNT();
begin
  parseTtl('localName_with_nfc_PN_CHARS_BASE_character_boundaries.nt', true);
end;

procedure TTurtleTests.test_localName_with_nfc_PN_CHARS_BASE_character_boundaries();
begin
  parseTtl('localName_with_nfc_PN_CHARS_BASE_character_boundaries.ttl', false);
end;

procedure TTurtleTests.test_localName_with_non_leading_extrasNT();
begin
  parseTtl('localName_with_non_leading_extras.nt', true);
end;

procedure TTurtleTests.test_localName_with_non_leading_extras();
begin
  parseTtl('localName_with_non_leading_extras.ttl', true);
end;

procedure TTurtleTests.test_negative_numericNT();
begin
  parseTtl('negative_numeric.nt', true);
end;

procedure TTurtleTests.test_negative_numeric();
begin
  parseTtl('negative_numeric.ttl', true);
end;

procedure TTurtleTests.test_nested_blankNodePropertyListsNT();
begin
  parseTtl('nested_blankNodePropertyLists.nt', true);
end;

procedure TTurtleTests.test_nested_blankNodePropertyLists();
begin
  parseTtl('nested_blankNodePropertyLists.ttl', true);
end;

procedure TTurtleTests.test_nested_collectionNT();
begin
  parseTtl('nested_collection.nt', true);
end;

procedure TTurtleTests.test_nested_collection();
begin
  parseTtl('nested_collection.ttl', false);
end;

procedure TTurtleTests.test_number_sign_following_localNameNT();
begin
  parseTtl('number_sign_following_localName.nt', true);
end;

//procedure TTurtleTests.test_number_sign_following_localName();
//begin
////     parseTtl('number_sign_following_localName.ttl', true);
//end;

procedure TTurtleTests.test_number_sign_following_PNAME_NSNT();
begin
  parseTtl('number_sign_following_PNAME_NS.nt', true);
end;

//procedure TTurtleTests.test_number_sign_following_PNAME_NS();
//begin
////     parseTtl('number_sign_following_PNAME_NS.ttl', true);
//end;

procedure TTurtleTests.test_numeric_with_leading_0NT();
begin
  parseTtl('numeric_with_leading_0.nt', true);
end;

procedure TTurtleTests.test_numeric_with_leading_0();
begin
  parseTtl('numeric_with_leading_0.ttl', true);
end;

procedure TTurtleTests.test_objectList_with_two_objectsNT();
begin
  parseTtl('objectList_with_two_objects.nt', true);
end;

procedure TTurtleTests.test_objectList_with_two_objects();
begin
  parseTtl('objectList_with_two_objects.ttl', true);
end;

procedure TTurtleTests.test_old_style_base();
begin
  parseTtl('old_style_base.ttl', true);
end;

procedure TTurtleTests.test_old_style_prefix();
begin
  parseTtl('old_style_prefix.ttl', true);
end;

procedure TTurtleTests.test_percent_escaped_localNameNT();
begin
  parseTtl('percent_escaped_localName.nt', true);
end;

//procedure TTurtleTests.test_percent_escaped_localName();
//begin
////     parseTtl('percent_escaped_localName.ttl', true);
//end;

procedure TTurtleTests.test_positive_numericNT();
begin
  parseTtl('positive_numeric.nt', true);
end;

procedure TTurtleTests.test_positive_numeric();
begin
  parseTtl('positive_numeric.ttl', true);
end;

procedure TTurtleTests.test_predicateObjectList_with_two_objectListsNT();
begin
  parseTtl('predicateObjectList_with_two_objectLists.nt', true);
end;

procedure TTurtleTests.test_predicateObjectList_with_two_objectLists();
begin
  parseTtl('predicateObjectList_with_two_objectLists.ttl', true);
end;

//procedure TTurtleTests.test_prefix_only_IRI();
//begin
////     parseTtl('prefix_only_IRI.ttl', true);
//end;

procedure TTurtleTests.test_prefix_reassigned_and_usedNT();
begin
  parseTtl('prefix_reassigned_and_used.nt', true);
end;

procedure TTurtleTests.test_prefix_reassigned_and_used();
begin
  parseTtl('prefix_reassigned_and_used.ttl', true);
end;

procedure TTurtleTests.test_prefix_with_non_leading_extras();
begin
  parseTtl('prefix_with_non_leading_extras.ttl', true);
end;

procedure TTurtleTests.test_prefix_with_PN_CHARS_BASE_character_boundaries();
begin
  parseTtl('prefix_with_PN_CHARS_BASE_character_boundaries.ttl', true);
end;

procedure TTurtleTests.test_prefixed_IRI_object();
begin
  parseTtl('prefixed_IRI_object.ttl', true);
end;

procedure TTurtleTests.test_prefixed_IRI_predicate();
begin
  parseTtl('prefixed_IRI_predicate.ttl', true);
end;

procedure TTurtleTests.test_prefixed_name_datatype();
begin
  parseTtl('prefixed_name_datatype.ttl', true);
end;

procedure TTurtleTests.test_repeated_semis_at_end();
begin
  parseTtl('repeated_semis_at_end.ttl', true);
end;

procedure TTurtleTests.test_repeated_semis_not_at_endNT();
begin
  parseTtl('repeated_semis_not_at_end.nt', true);
end;

procedure TTurtleTests.test_repeated_semis_not_at_end();
begin
  parseTtl('repeated_semis_not_at_end.ttl', true);
end;

procedure TTurtleTests.test_reserved_escaped_localNameNT();
begin
  parseTtl('reserved_escaped_localName.nt', true);
end;

//procedure TTurtleTests.test_reserved_escaped_localName();
//begin
////     parseTtl('reserved_escaped_localName.ttl', true);
//end;

procedure TTurtleTests.test_sole_blankNodePropertyList();
begin
  parseTtl('sole_blankNodePropertyList.ttl', true);
end;

procedure TTurtleTests.test_SPARQL_style_base();
begin
  parseTtl('SPARQL_style_base.ttl', true);
end;

procedure TTurtleTests.test_SPARQL_style_prefix();
begin
  parseTtl('SPARQL_style_prefix.ttl', true);
end;

procedure TTurtleTests.test_turtle_eval_bad_01();
begin
  parseTtl('turtle-eval-bad-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_eval_bad_02();
begin
  parseTtl('turtle-eval-bad-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_eval_bad_03();
begin
  parseTtl('turtle-eval-bad-03.ttl', false);
end;

//procedure TTurtleTests.test_turtle_eval_bad_04();
//begin
////     parseTtl('turtle-eval-bad-04.ttl', false);
//end;

procedure TTurtleTests.test_turtle_eval_struct_01NT();
begin
  parseTtl('turtle-eval-struct-01.nt', true);
end;

procedure TTurtleTests.test_turtle_eval_struct_01();
begin
  parseTtl('turtle-eval-struct-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_eval_struct_02NT();
begin
  parseTtl('turtle-eval-struct-02.nt', true);
end;

procedure TTurtleTests.test_turtle_eval_struct_02();
begin
  parseTtl('turtle-eval-struct-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_01NT();
begin
  parseTtl('turtle-subm-01.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_01();
begin
  parseTtl('turtle-subm-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_02NT();
begin
  parseTtl('turtle-subm-02.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_02();
begin
  parseTtl('turtle-subm-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_03NT();
begin
  parseTtl('turtle-subm-03.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_03();
begin
  parseTtl('turtle-subm-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_subm_04NT();
begin
  parseTtl('turtle-subm-04.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_04();
begin
  parseTtl('turtle-subm-04.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_05NT();
begin
  parseTtl('turtle-subm-05.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_05();
begin
  parseTtl('turtle-subm-05.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_06NT();
begin
  parseTtl('turtle-subm-06.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_06();
begin
  parseTtl('turtle-subm-06.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_07NT();
begin
  parseTtl('turtle-subm-07.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_07();
begin
  parseTtl('turtle-subm-07.ttl', false);
end;

procedure TTurtleTests.test_NT();
begin
  parseTtl('turtle-subm-08.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_08();
begin
  parseTtl('turtle-subm-08.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_09NT();
begin
  parseTtl('turtle-subm-09.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_09();
begin
  parseTtl('turtle-subm-09.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_10NT();
begin
  parseTtl('turtle-subm-10.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_10();
begin
  parseTtl('turtle-subm-10.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_11NT();
begin
  parseTtl('turtle-subm-11.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_11();
begin
  parseTtl('turtle-subm-11.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_12NT();
begin
  parseTtl('turtle-subm-12.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_12();
begin
  parseTtl('turtle-subm-12.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_13NT();
begin
  parseTtl('turtle-subm-13.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_13();
begin
  parseTtl('turtle-subm-13.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_14NT();
begin
  parseTtl('turtle-subm-14.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_14();
begin
  parseTtl('turtle-subm-14.ttl', false);
end;

procedure TTurtleTests.test_turtle_subm_15NT();
begin
  parseTtl('turtle-subm-15.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_15();
begin
  parseTtl('turtle-subm-15.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_16NT();
begin
  parseTtl('turtle-subm-16.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_16();
begin
  parseTtl('turtle-subm-16.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_17NT();
begin
  parseTtl('turtle-subm-17.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_17();
begin
  parseTtl('turtle-subm-17.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_18NT();
begin
  parseTtl('turtle-subm-18.nt', false);
end;

procedure TTurtleTests.test_turtle_subm_18();
begin
  parseTtl('turtle-subm-18.ttl', false);
end;

procedure TTurtleTests.test_turtle_subm_19NT();
begin
  parseTtl('turtle-subm-19.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_19();
begin
  parseTtl('turtle-subm-19.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_20NT();
begin
  parseTtl('turtle-subm-20.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_20();
begin
  parseTtl('turtle-subm-20.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_21NT();
begin
  parseTtl('turtle-subm-21.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_21();
begin
  parseTtl('turtle-subm-21.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_22NT();
begin
  parseTtl('turtle-subm-22.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_22();
begin
  parseTtl('turtle-subm-22.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_23NT();
begin
  parseTtl('turtle-subm-23.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_23();
begin
  parseTtl('turtle-subm-23.ttl', false);
end;

procedure TTurtleTests.test_turtle_subm_24NT();
begin
  parseTtl('turtle-subm-24.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_24();
begin
  parseTtl('turtle-subm-24.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_25NT();
begin
  parseTtl('turtle-subm-25.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_25();
begin
  parseTtl('turtle-subm-25.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_26NT();
begin
  parseTtl('turtle-subm-26.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_26();
begin
  parseTtl('turtle-subm-26.ttl', true);
end;

procedure TTurtleTests.test_turtle_subm_27NT();
begin
  parseTtl('turtle-subm-27.nt', true);
end;

procedure TTurtleTests.test_turtle_subm_27();
begin
  parseTtl('turtle-subm-27.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_base_01();
begin
  parseTtl('turtle-syntax-bad-base-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_base_02();
begin
  parseTtl('turtle-syntax-bad-base-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_base_03();
begin
  parseTtl('turtle-syntax-bad-base-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_esc_01();
begin
  parseTtl('turtle-syntax-bad-esc-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_esc_02();
begin
  parseTtl('turtle-syntax-bad-esc-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_esc_03();
begin
  parseTtl('turtle-syntax-bad-esc-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_esc_04();
begin
  parseTtl('turtle-syntax-bad-esc-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_kw_01();
begin
  parseTtl('turtle-syntax-bad-kw-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_kw_02();
begin
  parseTtl('turtle-syntax-bad-kw-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_kw_03();
begin
  parseTtl('turtle-syntax-bad-kw-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_kw_04();
begin
  parseTtl('turtle-syntax-bad-kw-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_kw_05();
begin
  parseTtl('turtle-syntax-bad-kw-05.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_lang_01();
begin
  parseTtl('turtle-syntax-bad-lang-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_LITERAL2_with_langtag_and_datatype();
begin
  parseTtl('turtle-syntax-bad-LITERAL2_with_langtag_and_datatype.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_ln_dash_start();
begin
  parseTtl('turtle-syntax-bad-ln-dash-start.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bad_ln_escape();
begin
  parseTtl('turtle-syntax-bad-ln-escape.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_ln_escape_start();
begin
  parseTtl('turtle-syntax-bad-ln-escape-start.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_missing_ns_dot_end();
begin
  parseTtl('turtle-syntax-bad-missing-ns-dot-end.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_missing_ns_dot_start();
begin
  parseTtl('turtle-syntax-bad-missing-ns-dot-start.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_01();
begin
  parseTtl('turtle-syntax-bad-n3-extras-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_02();
begin
  parseTtl('turtle-syntax-bad-n3-extras-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_03();
begin
  parseTtl('turtle-syntax-bad-n3-extras-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_04();
begin
  parseTtl('turtle-syntax-bad-n3-extras-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_05();
begin
  parseTtl('turtle-syntax-bad-n3-extras-05.ttl', false);
end;


procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_07();
begin
  parseTtl('turtle-syntax-bad-n3-extras-07.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_08();
begin
  parseTtl('turtle-syntax-bad-n3-extras-08.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_09();
begin
  parseTtl('turtle-syntax-bad-n3-extras-09.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_10();
begin
  parseTtl('turtle-syntax-bad-n3-extras-10.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_11();
begin
  parseTtl('turtle-syntax-bad-n3-extras-11.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_12();
begin
  parseTtl('turtle-syntax-bad-n3-extras-12.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_n3_extras_13();
begin
  parseTtl('turtle-syntax-bad-n3-extras-13.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_ns_dot_end();
begin
  parseTtl('turtle-syntax-bad-ns-dot-end.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bad_ns_dot_start();
begin
  parseTtl('turtle-syntax-bad-ns-dot-start.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_num_01();
begin
  parseTtl('turtle-syntax-bad-num-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_num_02();
begin
  parseTtl('turtle-syntax-bad-num-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_num_03();
begin
  parseTtl('turtle-syntax-bad-num-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_num_04();
begin
  parseTtl('turtle-syntax-bad-num-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_num_05();
begin
  parseTtl('turtle-syntax-bad-num-05.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_number_dot_in_anon();
begin
  parseTtl('turtle-syntax-bad-number-dot-in-anon.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bad_pname_01();
begin
  parseTtl('turtle-syntax-bad-pname-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_pname_02();
begin
  parseTtl('turtle-syntax-bad-pname-02.ttl', false);
end;


procedure TTurtleTests.test_turtle_syntax_bad_prefix_01();
begin
  parseTtl('turtle-syntax-bad-prefix-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_prefix_02();
begin
  parseTtl('turtle-syntax-bad-prefix-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_prefix_03();
begin
  parseTtl('turtle-syntax-bad-prefix-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_prefix_04();
begin
  parseTtl('turtle-syntax-bad-prefix-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_prefix_05();
begin
  parseTtl('turtle-syntax-bad-prefix-05.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_01();
begin
  parseTtl('turtle-syntax-bad-string-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_02();
begin
  parseTtl('turtle-syntax-bad-string-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_03();
begin
  parseTtl('turtle-syntax-bad-string-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_04();
begin
  parseTtl('turtle-syntax-bad-string-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_05();
begin
  parseTtl('turtle-syntax-bad-string-05.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_06();
begin
  parseTtl('turtle-syntax-bad-string-06.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_string_07();
begin
  parseTtl('turtle-syntax-bad-string-07.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_01();
begin
  parseTtl('turtle-syntax-bad-struct-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_02();
begin
  parseTtl('turtle-syntax-bad-struct-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_03();
begin
  parseTtl('turtle-syntax-bad-struct-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_04();
begin
  parseTtl('turtle-syntax-bad-struct-04.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_05();
begin
  parseTtl('turtle-syntax-bad-struct-05.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_06();
begin
  parseTtl('turtle-syntax-bad-struct-06.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_07();
begin
  parseTtl('turtle-syntax-bad-struct-07.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_08();
begin
  parseTtl('turtle-syntax-bad-struct-08.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_09();
begin
  parseTtl('turtle-syntax-bad-struct-09.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_10();
begin
  parseTtl('turtle-syntax-bad-struct-10.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_11();
begin
  parseTtl('turtle-syntax-bad-struct-11.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_12();
begin
  parseTtl('turtle-syntax-bad-struct-12.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_13();
begin
  parseTtl('turtle-syntax-bad-struct-13.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_14();
begin
  parseTtl('turtle-syntax-bad-struct-14.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_15();
begin
  parseTtl('turtle-syntax-bad-struct-15.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_16();
begin
  parseTtl('turtle-syntax-bad-struct-16.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_struct_17();
begin
  parseTtl('turtle-syntax-bad-struct-17.ttl', true);
end;


procedure TTurtleTests.test_turtle_syntax_bad_uri_02();
begin
  parseTtl('turtle-syntax-bad-uri-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_bad_uri_03();
begin
  parseTtl('turtle-syntax-bad-uri-03.ttl', false);
end;


procedure TTurtleTests.test_turtle_syntax_base_01();
begin
  parseTtl('turtle-syntax-base-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_base_02();
begin
  parseTtl('turtle-syntax-base-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_base_03();
begin
  parseTtl('turtle-syntax-base-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_base_04();
begin
  parseTtl('turtle-syntax-base-04.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_blank_label();
begin
  parseTtl('turtle-syntax-blank-label.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_01();
begin
  parseTtl('turtle-syntax-bnode-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_02();
begin
  parseTtl('turtle-syntax-bnode-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_03();
begin
  parseTtl('turtle-syntax-bnode-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_04();
begin
  parseTtl('turtle-syntax-bnode-04.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_05();
begin
  parseTtl('turtle-syntax-bnode-05.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_06();
begin
  parseTtl('turtle-syntax-bnode-06.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_07();
begin
  parseTtl('turtle-syntax-bnode-07.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_08();
begin
  parseTtl('turtle-syntax-bnode-08.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_09();
begin
  parseTtl('turtle-syntax-bnode-09.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_bnode_10();
begin
  parseTtl('turtle-syntax-bnode-10.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_datatypes_01();
begin
  parseTtl('turtle-syntax-datatypes-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_datatypes_02();
begin
  parseTtl('turtle-syntax-datatypes-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_file_01();
begin
  parseTtl('turtle-syntax-file-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_file_02();
begin
  parseTtl('turtle-syntax-file-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_file_03();
begin
  parseTtl('turtle-syntax-file-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_kw_01();
begin
  parseTtl('turtle-syntax-kw-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_kw_02();
begin
  parseTtl('turtle-syntax-kw-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_kw_03();
begin
  parseTtl('turtle-syntax-kw-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_lists_01();
begin
  parseTtl('turtle-syntax-lists-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_lists_02();
begin
  parseTtl('turtle-syntax-lists-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_ln_dots();
begin
  parseTtl('turtle-syntax-ln-dots.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_ns_dots();
begin
  parseTtl('turtle-syntax-ns-dots.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_01();
begin
  parseTtl('turtle-syntax-number-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_02();
begin
  parseTtl('turtle-syntax-number-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_03();
begin
  parseTtl('turtle-syntax-number-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_04();
begin
  parseTtl('turtle-syntax-number-04.ttl', true);
end;


procedure TTurtleTests.test_turtle_syntax_number_06();
begin
  parseTtl('turtle-syntax-number-06.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_07();
begin
  parseTtl('turtle-syntax-number-07.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_09();
begin
  parseTtl('turtle-syntax-number-09.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_10();
begin
  parseTtl('turtle-syntax-number-10.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_number_11();
begin
  parseTtl('turtle-syntax-number-11.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_pname_esc_01();
begin
  parseTtl('turtle-syntax-pname-esc-01.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_pname_esc_02();
begin
  parseTtl('turtle-syntax-pname-esc-02.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_pname_esc_03();
begin
  parseTtl('turtle-syntax-pname-esc-03.ttl', false);
end;

procedure TTurtleTests.test_turtle_syntax_prefix_01();
begin
  parseTtl('turtle-syntax-prefix-01.ttl', true);
end;


procedure TTurtleTests.test_turtle_syntax_prefix_03();
begin
  parseTtl('turtle-syntax-prefix-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_prefix_04();
begin
  parseTtl('turtle-syntax-prefix-04.ttl', true);
end;



procedure TTurtleTests.test_turtle_syntax_prefix_07();
begin
  parseTtl('turtle-syntax-prefix-07.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_prefix_08();
begin
  parseTtl('turtle-syntax-prefix-08.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_prefix_09();
begin
  parseTtl('turtle-syntax-prefix-09.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_str_esc_01();
begin
  parseTtl('turtle-syntax-str-esc-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_str_esc_02();
begin
  parseTtl('turtle-syntax-str-esc-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_str_esc_03();
begin
  parseTtl('turtle-syntax-str-esc-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_01();
begin
  parseTtl('turtle-syntax-string-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_02();
begin
  parseTtl('turtle-syntax-string-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_03();
begin
  parseTtl('turtle-syntax-string-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_04();
begin
  parseTtl('turtle-syntax-string-04.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_05();
begin
  parseTtl('turtle-syntax-string-05.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_06();
begin
  parseTtl('turtle-syntax-string-06.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_07();
begin
  parseTtl('turtle-syntax-string-07.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_08();
begin
  parseTtl('turtle-syntax-string-08.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_09();
begin
  parseTtl('turtle-syntax-string-09.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_10();
begin
  parseTtl('turtle-syntax-string-10.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_string_11();
begin
  parseTtl('turtle-syntax-string-11.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_struct_01();
begin
  parseTtl('turtle-syntax-struct-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_struct_02();
begin
  parseTtl('turtle-syntax-struct-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_struct_03();
begin
  parseTtl('turtle-syntax-struct-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_struct_04();
begin
  parseTtl('turtle-syntax-struct-04.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_struct_05();
begin
  parseTtl('turtle-syntax-struct-05.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_uri_01();
begin
  parseTtl('turtle-syntax-uri-01.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_uri_02();
begin
  parseTtl('turtle-syntax-uri-02.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_uri_03();
begin
  parseTtl('turtle-syntax-uri-03.ttl', true);
end;

procedure TTurtleTests.test_turtle_syntax_uri_04();
begin
  parseTtl('turtle-syntax-uri-04.ttl', true);
end;

procedure TTurtleTests.test_two_LITERAL_LONG2sNT();
begin
  parseTtl('two_LITERAL_LONG2s.nt', true);
end;

procedure TTurtleTests.test_two_LITERAL_LONG2s();
begin
  parseTtl('two_LITERAL_LONG2s.ttl', true);
end;

procedure TTurtleTests.test_underscore_in_localNameNT();
begin
  parseTtl('underscore_in_localName.nt', true);
end;

procedure TTurtleTests.test_underscore_in_localName();
begin
  parseTtl('underscore_in_localName.ttl', true);
end;

procedure TTurtleTests.test_anonymous_blank_node_object();
begin
  parseTtl('anonymous_blank_node_object.ttl', true);
end;

procedure TTurtleTests.test_anonymous_blank_node_subject();
begin
  parseTtl('anonymous_blank_node_subject.ttl', true);
end;

procedure TTurtleTests.test_bareword_a_predicateNT();
begin
  parseTtl('bareword_a_predicate.nt', true);
end;

procedure TTurtleTests.test_bareword_a_predicate();
begin
  parseTtl('bareword_a_predicate.ttl', true);
end;

procedure TTurtleTests.test_bareword_decimalNT();
begin
  parseTtl('bareword_decimal.nt', true);
end;

procedure TTurtleTests.test_bareword_decimal();
begin
  parseTtl('bareword_decimal.ttl', true);
end;

procedure TTurtleTests.test_bareword_doubleNT();
begin
  parseTtl('bareword_double.nt', true);
end;

procedure TTurtleTests.test_bareword_double();
begin
  parseTtl('bareword_double.ttl', true);
end;

procedure TTurtleTests.test_bareword_integer();
begin
  parseTtl('bareword_integer.ttl', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_as_objectNT();
begin
  parseTtl('blankNodePropertyList_as_object.nt', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_as_object();
begin
  parseTtl('blankNodePropertyList_as_object.ttl', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_as_subjectNT();
begin
  parseTtl('blankNodePropertyList_as_subject.nt', true);
end;



procedure TTurtleTests.test_blankNodePropertyList_containing_collectionNT();
begin
  parseTtl('blankNodePropertyList_containing_collection.nt', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_containing_collection();
begin
  parseTtl('blankNodePropertyList_containing_collection.ttl', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_with_multiple_triplesNT();
begin
  parseTtl('blankNodePropertyList_with_multiple_triples.nt', true);
end;

procedure TTurtleTests.test_blankNodePropertyList_with_multiple_triples();
begin
  parseTtl('blankNodePropertyList_with_multiple_triples.ttl', true);
end;

procedure TTurtleTests.test_collection_objectNT();
begin
  parseTtl('collection_object.nt', true);
end;

procedure TTurtleTests.test_collection_object();
begin
  parseTtl('collection_object.ttl', true);
end;

procedure TTurtleTests.test_collection_subjectNT();
begin
  parseTtl('collection_subject.nt', true);
end;

procedure TTurtleTests.test_collection_subject();
begin
  parseTtl('collection_subject.ttl', false);
end;

procedure TTurtleTests.test_comment_following_localName();
begin
  parseTtl('comment_following_localName.ttl', true);
end;

procedure TTurtleTests.test_comment_following_PNAME_NSNT();
begin
  parseTtl('comment_following_PNAME_NS.nt', true);
end;

procedure TTurtleTests.test_comment_following_PNAME_NS();
begin
  parseTtl('comment_following_PNAME_NS.ttl', false);
end;

procedure TTurtleTests.test__default_namespace_IRI();
begin
  parseTtl('default_namespace_IRI.ttl', true);
end;
//

procedure TTurtleResourceTests.parseResource(filename: String);
var
  i, o{, m} : string;
  p : TFHIRParser;
  c : TFHIRComposer;
  s : TStringStream;
begin
  i := FileToString('C:\work\org.hl7.fhir\build\publish\'+filename, TEncoding.UTF8);
  p := TFHIRTurtleParser.Create(nil, 'en');
  try
    p.source := TStringStream.Create(i, TENcoding.UTF8);
    try
      p.Parse;
      c := TFHIRTurtleComposer.Create(nil, OutputStyleNormal, 'en');
      try
        s := TStringStream.Create;
        try
          c.Compose(s, p.resource);
          o := s.DataString;
//          if not CheckTurtleIsSame(i, o, m) then
//            Assert.Fail(m)
//          else
            Assert.Pass();
        finally
          s.Free;
        end;
      finally
        c.Free;
      end;
    finally
      p.source.Free;
    end;
  finally
    p.free;
  end;
end;

procedure TTurtleResourceTests.test_audit_event_example_pixQuery();
begin
  parseResource('audit-event-example-pixQuery.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_media();
begin
  parseResource('audit-event-example-media.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_logout();
begin
  parseResource('audit-event-example-logout.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_login();
begin
  parseResource('audit-event-example-login.ttl');
end;

procedure TTurtleResourceTests.test_appointmentresponse_example();
begin
  parseResource('appointmentresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_appointmentresponse_example_req();
begin
  parseResource('appointmentresponse-example-req.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example2doctors();
begin
  parseResource('appointment-example2doctors.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example();
begin
  parseResource('appointment-example.ttl');
end;

procedure TTurtleResourceTests.test_appointment_example_request();
begin
  parseResource('appointment-example-request.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_medication();
begin
  parseResource('allergyintolerance-medication.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_fishallergy();
begin
  parseResource('allergyintolerance-fishallergy.ttl');
end;

procedure TTurtleResourceTests.test_allergyintolerance_example();
begin
  parseResource('allergyintolerance-example.ttl');
end;

procedure TTurtleResourceTests.test_account_example();
begin
  parseResource('account-example.ttl');
end;

procedure TTurtleResourceTests.test_xds_example();
begin
  parseResource('xds-example.ttl');
end;

procedure TTurtleResourceTests.test_visionprescription_example();
begin
  parseResource('visionprescription-example.ttl');
end;

procedure TTurtleResourceTests.test_visionprescription_example_1();
begin
  parseResource('visionprescription-example-1.ttl');
end;

procedure TTurtleResourceTests.test_valueset_ucum_common();
begin
  parseResource('valueset-ucum-common.ttl');
end;

procedure TTurtleResourceTests.test_valueset_nhin_purposeofuse();
begin
  parseResource('valueset-nhin-purposeofuse.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example();
begin
  parseResource('valueset-example.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_yesnodontknow();
begin
  parseResource('valueset-example-yesnodontknow.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_intensional();
begin
  parseResource('valueset-example-intensional.ttl');
end;

procedure TTurtleResourceTests.test_valueset_example_expansion();
begin
  parseResource('valueset-example-expansion.ttl');
end;

procedure TTurtleResourceTests.test_valueset_cpt_all();
begin
  parseResource('valueset-cpt-all.ttl');
end;

procedure TTurtleResourceTests.test_testscript_example();
begin
  parseResource('testscript-example.ttl');
end;

procedure TTurtleResourceTests.test_testscript_example_rule();
begin
  parseResource('testscript-example-rule.ttl');
end;

procedure TTurtleResourceTests.test_supplydelivery_example();
begin
  parseResource('supplydelivery-example.ttl');
end;

procedure TTurtleResourceTests.test_substance_example();
begin
  parseResource('substance-example.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_silver_nitrate_product();
begin
  parseResource('substance-example-silver-nitrate-product.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f203_potassium();
begin
  parseResource('substance-example-f203-potassium.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f202_staphylococcus();
begin
  parseResource('substance-example-f202-staphylococcus.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_f201_dust();
begin
  parseResource('substance-example-f201-dust.ttl');
end;

procedure TTurtleResourceTests.test_substance_example_amoxicillin_clavulanate();
begin
  parseResource('substance-example-amoxicillin-clavulanate.ttl');
end;

procedure TTurtleResourceTests.test_subscription_example();
begin
  parseResource('subscription-example.ttl');
end;

procedure TTurtleResourceTests.test_subscription_example_error();
begin
  parseResource('subscription-example-error.ttl');
end;

procedure TTurtleResourceTests.test_structuremap_example();
begin
  parseResource('structuremap-example.ttl');
end;

procedure TTurtleResourceTests.test_structuredefinition_example();
begin
  parseResource('structuredefinition-example.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example();
begin
  parseResource('specimen-example.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example_urine();
begin
  parseResource('specimen-example-urine.ttl');
end;

procedure TTurtleResourceTests.test_specimen_example_isolate();
begin
  parseResource('specimen-example-isolate.ttl');
end;

procedure TTurtleResourceTests.test_slot_example();
begin
  parseResource('slot-example.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_unavailable();
begin
  parseResource('slot-example-unavailable.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_tentative();
begin
  parseResource('slot-example-tentative.ttl');
end;

procedure TTurtleResourceTests.test_slot_example_busy();
begin
  parseResource('slot-example-busy.ttl');
end;

procedure TTurtleResourceTests.test_sequence_example();
begin
  parseResource('sequence-example.ttl');
end;

procedure TTurtleResourceTests.test_searchparameter_example();
begin
  parseResource('searchparameter-example.ttl');
end;

procedure TTurtleResourceTests.test_searchparameter_example_extension();
begin
  parseResource('searchparameter-example-extension.ttl');
end;

procedure TTurtleResourceTests.test_schedule_example();
begin
  parseResource('schedule-example.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example();
begin
  parseResource('riskassessment-example.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_prognosis();
begin
  parseResource('riskassessment-example-prognosis.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_population();
begin
  parseResource('riskassessment-example-population.ttl');
end;

procedure TTurtleResourceTests.test_riskassessment_example_cardiac();
begin
  parseResource('riskassessment-example-cardiac.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example();
begin
  parseResource('relatedperson-example.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_peter();
begin
  parseResource('relatedperson-example-peter.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_f002_ariadne();
begin
  parseResource('relatedperson-example-f002-ariadne.ttl');
end;

procedure TTurtleResourceTests.test_relatedperson_example_f001_sarah();
begin
  parseResource('relatedperson-example-f001-sarah.ttl');
end;

procedure TTurtleResourceTests.test_provenance_example();
begin
  parseResource('provenance-example.ttl');
end;

procedure TTurtleResourceTests.test_provenance_example_sig();
begin
  parseResource('provenance-example-sig.ttl');
end;

procedure TTurtleResourceTests.test_processresponse_example();
begin
  parseResource('processresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example();
begin
  parseResource('processrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_status();
begin
  parseResource('processrequest-example-status.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_reverse();
begin
  parseResource('processrequest-example-reverse.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_reprocess();
begin
  parseResource('processrequest-example-reprocess.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_specific();
begin
  parseResource('processrequest-example-poll-specific.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_payrec();
begin
  parseResource('processrequest-example-poll-payrec.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_inclusive();
begin
  parseResource('processrequest-example-poll-inclusive.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_exclusive();
begin
  parseResource('processrequest-example-poll-exclusive.ttl');
end;

procedure TTurtleResourceTests.test_processrequest_example_poll_eob();
begin
  parseResource('processrequest-example-poll-eob.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example();
begin
  parseResource('procedure-example.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_implant();
begin
  parseResource('procedure-example-implant.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f201_tpf();
begin
  parseResource('procedure-example-f201-tpf.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f004_tracheotomy();
begin
  parseResource('procedure-example-f004-tracheotomy.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f003_abscess();
begin
  parseResource('procedure-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f002_lung();
begin
  parseResource('procedure-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_f001_heart();
begin
  parseResource('procedure-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_procedure_example_biopsy();
begin
  parseResource('procedure-example-biopsy.ttl');
end;

procedure TTurtleResourceTests.test_practitionerrole_example();
begin
  parseResource('practitionerrole-example.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_examples_general();
begin
  parseResource('practitioner-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example();
begin
  parseResource('practitioner-example.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_xcda1();
begin
  parseResource('practitioner-example-xcda1.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_xcda_author();
begin
  parseResource('practitioner-example-xcda-author.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f204_ce();
begin
  parseResource('practitioner-example-f204-ce.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f203_jvg();
begin
  parseResource('practitioner-example-f203-jvg.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f202_lm();
begin
  parseResource('practitioner-example-f202-lm.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f201_ab();
begin
  parseResource('practitioner-example-f201-ab.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f007_sh();
begin
  parseResource('practitioner-example-f007-sh.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f006_rvdb();
begin
  parseResource('practitioner-example-f006-rvdb.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f005_al();
begin
  parseResource('practitioner-example-f005-al.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f004_rb();
begin
  parseResource('practitioner-example-f004-rb.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f003_mv();
begin
  parseResource('practitioner-example-f003-mv.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f002_pv();
begin
  parseResource('practitioner-example-f002-pv.ttl');
end;

procedure TTurtleResourceTests.test_practitioner_example_f001_evdb();
begin
  parseResource('practitioner-example-f001-evdb.ttl');
end;

procedure TTurtleResourceTests.test_person_provider_directory();
begin
  parseResource('person-provider-directory.ttl');
end;

procedure TTurtleResourceTests.test_person_patient_portal();
begin
  parseResource('person-patient-portal.ttl');
end;

procedure TTurtleResourceTests.test_person_grahame();
begin
  parseResource('person-grahame.ttl');
end;

procedure TTurtleResourceTests.test_person_example();
begin
  parseResource('person-example.ttl');
end;

procedure TTurtleResourceTests.test_person_example_f002_ariadne();
begin
  parseResource('person-example-f002-ariadne.ttl');
end;

procedure TTurtleResourceTests.test_paymentreconciliation_example();
begin
  parseResource('paymentreconciliation-example.ttl');
end;

procedure TTurtleResourceTests.test_paymentnotice_example();
begin
  parseResource('paymentnotice-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_glossy_example();
begin
  parseResource('patient-glossy-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_examples_general();
begin
  parseResource('patient-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_patient_examples_cypress_template();
begin
  parseResource('patient-examples-cypress-template.ttl');
end;

procedure TTurtleResourceTests.test_patient_example();
begin
  parseResource('patient-example.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_xds();
begin
  parseResource('patient-example-xds.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_xcda();
begin
  parseResource('patient-example-xcda.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_proband();
begin
  parseResource('patient-example-proband.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_ihe_pcd();
begin
  parseResource('patient-example-ihe-pcd.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_f201_roel();
begin
  parseResource('patient-example-f201-roel.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_f001_pieter();
begin
  parseResource('patient-example-f001-pieter.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_dicom();
begin
  parseResource('patient-example-dicom.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_d();
begin
  parseResource('patient-example-d.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_c();
begin
  parseResource('patient-example-c.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_b();
begin
  parseResource('patient-example-b.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_animal();
begin
  parseResource('patient-example-animal.ttl');
end;

procedure TTurtleResourceTests.test_patient_example_a();
begin
  parseResource('patient-example-a.ttl');
end;

procedure TTurtleResourceTests.test_parameters_example();
begin
  parseResource('parameters-example.ttl');
end;

procedure TTurtleResourceTests.test_organization_example();
begin
  parseResource('organization-example.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_lab();
begin
  parseResource('organization-example-lab.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_insurer();
begin
  parseResource('organization-example-insurer.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_good_health_care();
begin
  parseResource('organization-example-good-health-care.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_gastro();
begin
  parseResource('organization-example-gastro.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f203_bumc();
begin
  parseResource('organization-example-f203-bumc.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f201_aumc();
begin
  parseResource('organization-example-f201-aumc.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f003_burgers_ENT();
begin
  parseResource('organization-example-f003-burgers-ENT.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f002_burgers_card();
begin
  parseResource('organization-example-f002-burgers-card.ttl');
end;

procedure TTurtleResourceTests.test_organization_example_f001_burgers();
begin
  parseResource('organization-example-f001-burgers.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example();
begin
  parseResource('operationoutcome-example.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_validationfail();
begin
  parseResource('operationoutcome-example-validationfail.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_searchfail();
begin
  parseResource('operationoutcome-example-searchfail.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_exception();
begin
  parseResource('operationoutcome-example-exception.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_break_the_glass();
begin
  parseResource('operationoutcome-example-break-the-glass.ttl');
end;

procedure TTurtleResourceTests.test_operationoutcome_example_allok();
begin
  parseResource('operationoutcome-example-allok.ttl');
end;

procedure TTurtleResourceTests.test_operationdefinition_example();
begin
  parseResource('operationdefinition-example.ttl');
end;

procedure TTurtleResourceTests.test_observation_example();
begin
  parseResource('observation-example.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_unsat();
begin
  parseResource('observation-example-unsat.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_satO2();
begin
  parseResource('observation-example-satO2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_sample_data();
begin
  parseResource('observation-example-sample-data.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_glasgow();
begin
  parseResource('observation-example-glasgow.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_glasgow_qa();
begin
  parseResource('observation-example-glasgow-qa.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_5();
begin
  parseResource('observation-example-genetics-5.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_4();
begin
  parseResource('observation-example-genetics-4.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_3();
begin
  parseResource('observation-example-genetics-3.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_2();
begin
  parseResource('observation-example-genetics-2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_genetics_1();
begin
  parseResource('observation-example-genetics-1.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f206_staphylococcus();
begin
  parseResource('observation-example-f206-staphylococcus.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f205_egfr();
begin
  parseResource('observation-example-f205-egfr.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f204_creatinine();
begin
  parseResource('observation-example-f204-creatinine.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f203_bicarbonate();
begin
  parseResource('observation-example-f203-bicarbonate.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f202_temperature();
begin
  parseResource('observation-example-f202-temperature.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f005_hemoglobin();
begin
  parseResource('observation-example-f005-hemoglobin.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f004_erythrocyte();
begin
  parseResource('observation-example-f004-erythrocyte.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f003_co2();
begin
  parseResource('observation-example-f003-co2.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f002_excess();
begin
  parseResource('observation-example-f002-excess.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_f001_glucose();
begin
  parseResource('observation-example-f001-glucose.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_bloodpressure();
begin
  parseResource('observation-example-bloodpressure.ttl');
end;

procedure TTurtleResourceTests.test_observation_example_bloodpressure_cancel();
begin
  parseResource('observation-example-bloodpressure-cancel.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_texture_modified();
begin
  parseResource('nutritionorder-example-texture-modified.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_renaldiet();
begin
  parseResource('nutritionorder-example-renaldiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_pureeddiet();
begin
  parseResource('nutritionorder-example-pureeddiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_pureeddiet_simple();
begin
  parseResource('nutritionorder-example-pureeddiet-simple.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_proteinsupplement();
begin
  parseResource('nutritionorder-example-proteinsupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_infantenteral();
begin
  parseResource('nutritionorder-example-infantenteral.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_fiberrestricteddiet();
begin
  parseResource('nutritionorder-example-fiberrestricteddiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_enteralcontinuous();
begin
  parseResource('nutritionorder-example-enteralcontinuous.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_enteralbolus();
begin
  parseResource('nutritionorder-example-enteralbolus.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_energysupplement();
begin
  parseResource('nutritionorder-example-energysupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_diabeticsupplement();
begin
  parseResource('nutritionorder-example-diabeticsupplement.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_diabeticdiet();
begin
  parseResource('nutritionorder-example-diabeticdiet.ttl');
end;

procedure TTurtleResourceTests.test_nutritionorder_example_cardiacdiet();
begin
  parseResource('nutritionorder-example-cardiacdiet.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_registry();
begin
  parseResource('namingsystem-registry.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_example();
begin
  parseResource('namingsystem-example.ttl');
end;

procedure TTurtleResourceTests.test_namingsystem_example_id();
begin
  parseResource('namingsystem-example-id.ttl');
end;

procedure TTurtleResourceTests.test_messageheader_example();
begin
  parseResource('messageheader-example.ttl');
end;

procedure TTurtleResourceTests.test_message_response_link();
begin
  parseResource('message-response-link.ttl');
end;

procedure TTurtleResourceTests.test_message_request_link();
begin
  parseResource('message-request-link.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample7();
begin
  parseResource('medicationstatementexample7.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample6();
begin
  parseResource('medicationstatementexample6.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample5();
begin
  parseResource('medicationstatementexample5.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample4();
begin
  parseResource('medicationstatementexample4.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample2();
begin
  parseResource('medicationstatementexample2.ttl');
end;

procedure TTurtleResourceTests.test_medicationstatementexample1();
begin
  parseResource('medicationstatementexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationrequestexample2();
begin
  parseResource('medicationrequestexample2.ttl');
end;

procedure TTurtleResourceTests.test_medicationrequestexample1();
begin
  parseResource('medicationrequestexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationexample15();
begin
  parseResource('medicationexample15.ttl');
end;

procedure TTurtleResourceTests.test_medicationexample1();
begin
  parseResource('medicationexample1.ttl');
end;

procedure TTurtleResourceTests.test_medicationdispenseexample8();
begin
  parseResource('medicationdispenseexample8.ttl');
end;

procedure TTurtleResourceTests.test_medicationadministrationexample3();
begin
  parseResource('medicationadministrationexample3.ttl');
end;

procedure TTurtleResourceTests.test_medication_example_f203_paracetamol();
begin
  parseResource('medicationexample0312.ttl');
end;

procedure TTurtleResourceTests.test_media_example();
begin
  parseResource('media-example.ttl');
end;

procedure TTurtleResourceTests.test_media_example_sound();
begin
  parseResource('media-example-sound.ttl');
end;

procedure TTurtleResourceTests.test_media_example_dicom();
begin
  parseResource('media-example-dicom.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat3_example();
begin
  parseResource('measurereport-cms146-cat3-example.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat2_example();
begin
  parseResource('measurereport-cms146-cat2-example.ttl');
end;

procedure TTurtleResourceTests.test_measurereport_cms146_cat1_example();
begin
  parseResource('measurereport-cms146-cat1-example.ttl');
end;

procedure TTurtleResourceTests.test_measure_exclusive_breastfeeding();
begin
  parseResource('measure-exclusive-breastfeeding.ttl');
end;

procedure TTurtleResourceTests.test_location_example();
begin
  parseResource('location-example.ttl');
end;

procedure TTurtleResourceTests.test_location_example_ukpharmacy();
begin
  parseResource('location-example-ukpharmacy.ttl');
end;

procedure TTurtleResourceTests.test_location_example_room();
begin
  parseResource('location-example-room.ttl');
end;

procedure TTurtleResourceTests.test_location_example_patients_home();
begin
  parseResource('location-example-patients-home.ttl');
end;

procedure TTurtleResourceTests.test_location_example_hl7hq();
begin
  parseResource('location-example-hl7hq.ttl');
end;

procedure TTurtleResourceTests.test_location_example_ambulance();
begin
  parseResource('location-example-ambulance.ttl');
end;

procedure TTurtleResourceTests.test_list_example();
begin
  parseResource('list-example.ttl');
end;

procedure TTurtleResourceTests.test_list_example_medlist();
begin
  parseResource('list-example-medlist.ttl');
end;

procedure TTurtleResourceTests.test_list_example_familyhistory_f201_roel();
begin
  parseResource('list-example-familyhistory-f201-roel.ttl');
end;

procedure TTurtleResourceTests.test_list_example_empty();
begin
  parseResource('list-example-empty.ttl');
end;

procedure TTurtleResourceTests.test_list_example_allergies();
begin
  parseResource('list-example-allergies.ttl');
end;

procedure TTurtleResourceTests.test_linkage_example();
begin
  parseResource('linkage-example.ttl');
end;

procedure TTurtleResourceTests.test_library_exclusive_breastfeeding_cqm_logic();
begin
  parseResource('library-exclusive-breastfeeding-cqm-logic.ttl');
end;

procedure TTurtleResourceTests.test_library_exclusive_breastfeeding_cds_logic();
begin
  parseResource('library-exclusive-breastfeeding-cds-logic.ttl');
end;

procedure TTurtleResourceTests.test_library_example();
begin
  parseResource('library-example.ttl');
end;

procedure TTurtleResourceTests.test_library_cms146_example();
begin
  parseResource('library-cms146-example.ttl');
end;

procedure TTurtleResourceTests.test_implementationguide_example();
begin
  parseResource('implementationguide-example.ttl');
end;

procedure TTurtleResourceTests.test_immunizationrecommendation_example();
begin
  parseResource('immunizationrecommendation-example.ttl');
end;

procedure TTurtleResourceTests.test_immunization_example();
begin
  parseResource('immunization-example.ttl');
end;

procedure TTurtleResourceTests.test_immunization_example_refused();
begin
  parseResource('immunization-example-refused.ttl');
end;

procedure TTurtleResourceTests.test_imagingstudy_example();
begin
  parseResource('imagingstudy-example.ttl');
end;

procedure TTurtleResourceTests.test_healthcareservice_example();
begin
  parseResource('healthcareservice-example.ttl');
end;

procedure TTurtleResourceTests.test_guidanceresponse_example();
begin
  parseResource('guidanceresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_group_example();
begin
  parseResource('group-example.ttl');
end;

procedure TTurtleResourceTests.test_group_example_member();
begin
  parseResource('group-example-member.ttl');
end;

procedure TTurtleResourceTests.test_goal_example();
begin
  parseResource('goal-example.ttl');
end;

procedure TTurtleResourceTests.test_flag_example();
begin
  parseResource('flag-example.ttl');
end;

procedure TTurtleResourceTests.test_flag_example_encounter();
begin
  parseResource('flag-example-encounter.ttl');
end;

procedure TTurtleResourceTests.test_familymemberhistory_example();
begin
  parseResource('familymemberhistory-example.ttl');
end;

procedure TTurtleResourceTests.test_familymemberhistory_example_mother();
begin
  parseResource('familymemberhistory-example-mother.ttl');
end;

procedure TTurtleResourceTests.test_explanationofbenefit_example();
begin
  parseResource('explanationofbenefit-example.ttl');
end;

procedure TTurtleResourceTests.test_episodeofcare_example();
begin
  parseResource('episodeofcare-example.ttl');
end;

procedure TTurtleResourceTests.test_enrollmentresponse_example();
begin
  parseResource('enrollmentresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_enrollmentrequest_example();
begin
  parseResource('enrollmentrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_endpoint_example();
begin
  parseResource('endpoint-example.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example();
begin
  parseResource('encounter-example.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_xcda();
begin
  parseResource('encounter-example-xcda.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_home();
begin
  parseResource('encounter-example-home.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f203_20130311();
begin
  parseResource('encounter-example-f203-20130311.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f202_20130128();
begin
  parseResource('encounter-example-f202-20130128.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f201_20130404();
begin
  parseResource('encounter-example-f201-20130404.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f003_abscess();
begin
  parseResource('encounter-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f002_lung();
begin
  parseResource('encounter-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_encounter_example_f001_heart();
begin
  parseResource('encounter-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_eligibilityresponse_example();
begin
  parseResource('eligibilityresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_eligibilityrequest_example();
begin
  parseResource('eligibilityrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_documentreference_example();
begin
  parseResource('documentreference-example.ttl');
end;

procedure TTurtleResourceTests.test_documentmanifest_fm_attachment();
begin
  parseResource('documentmanifest-fm-attachment.ttl');
end;

procedure TTurtleResourceTests.test_document_example_dischargesummary();
begin
  parseResource('document-example-dischargesummary.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_micro1();
begin
  parseResource('diagnosticreport-micro1.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_hla_genetics_results_example();
begin
  parseResource('diagnosticreport-hla-genetics-results-example.ttl');
end;


procedure TTurtleResourceTests.test_diagnosticreport_genetics_comprehensive_bone_marrow_report();
begin
  parseResource('diagnosticreport-genetics-comprehensive-bone-marrow-report.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_examples_general();
begin
  parseResource('diagnosticreport-examples-general.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_ultrasound();
begin
  parseResource('diagnosticreport-example-ultrasound.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_lipids();
begin
  parseResource('diagnosticreport-example-lipids.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_ghp();
begin
  parseResource('diagnosticreport-example-ghp.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f202_bloodculture();
begin
  parseResource('diagnosticreport-example-f202-bloodculture.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f201_brainct();
begin
  parseResource('diagnosticreport-example-f201-brainct.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_f001_bloodexam();
begin
  parseResource('diagnosticreport-example-f001-bloodexam.ttl');
end;

procedure TTurtleResourceTests.test_diagnosticreport_example_dxa();
begin
  parseResource('diagnosticreport-example-dxa.ttl');
end;

procedure TTurtleResourceTests.test_deviceusestatement_example();
begin
  parseResource('deviceusestatement-example.ttl');
end;

procedure TTurtleResourceTests.test_devicemetric_example();
begin
  parseResource('devicemetric-example.ttl');
end;

procedure TTurtleResourceTests.test_devicecomponent_example();
begin
  parseResource('devicecomponent-example.ttl');
end;

procedure TTurtleResourceTests.test_devicecomponent_example_prodspec();
begin
  parseResource('devicecomponent-example-prodspec.ttl');
end;

procedure TTurtleResourceTests.test_device_example();
begin
  parseResource('device-example.ttl');
end;

procedure TTurtleResourceTests.test_device_example_udi1();
begin
  parseResource('device-example-udi1.ttl');
end;

procedure TTurtleResourceTests.test_device_example_software();
begin
  parseResource('device-example-software.ttl');
end;

procedure TTurtleResourceTests.test_device_example_pacemaker();
begin
  parseResource('device-example-pacemaker.ttl');
end;

procedure TTurtleResourceTests.test_device_example_ihe_pcd();
begin
  parseResource('device-example-ihe-pcd.ttl');
end;

procedure TTurtleResourceTests.test_device_example_f001_feedingtube();
begin
  parseResource('device-example-f001-feedingtube.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example();
begin
  parseResource('detectedissue-example.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_lab();
begin
  parseResource('detectedissue-example-lab.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_dup();
begin
  parseResource('detectedissue-example-dup.ttl');
end;

procedure TTurtleResourceTests.test_detectedissue_example_allergy();
begin
  parseResource('detectedissue-example-allergy.ttl');
end;

procedure TTurtleResourceTests.test_coverage_example();
begin
  parseResource('coverage-example.ttl');
end;

procedure TTurtleResourceTests.test_coverage_example_2();
begin
  parseResource('coverage-example-2.ttl');
end;

procedure TTurtleResourceTests.test_contract_example();
begin
  parseResource('contract-example.ttl');
end;

procedure TTurtleResourceTests.test_condition_example2();
begin
  parseResource('condition-example2.ttl');
end;

procedure TTurtleResourceTests.test_condition_example();
begin
  parseResource('condition-example.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_stroke();
begin
  parseResource('condition-example-stroke.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f205_infection();
begin
  parseResource('condition-example-f205-infection.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f204_renal();
begin
  parseResource('condition-example-f204-renal.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f203_sepsis();
begin
  parseResource('condition-example-f203-sepsis.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f202_malignancy();
begin
  parseResource('condition-example-f202-malignancy.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f201_fever();
begin
  parseResource('condition-example-f201-fever.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f003_abscess();
begin
  parseResource('condition-example-f003-abscess.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f002_lung();
begin
  parseResource('condition-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_condition_example_f001_heart();
begin
  parseResource('condition-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_example();
begin
  parseResource('conceptmap-example.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_example_specimen_type();
begin
  parseResource('conceptmap-example-specimen-type.ttl');
end;

procedure TTurtleResourceTests.test_conceptmap_103();
begin
  parseResource('conceptmap-103.ttl');
end;

procedure TTurtleResourceTests.test_composition_example();
begin
  parseResource('composition-example.ttl');
end;

procedure TTurtleResourceTests.test_communicationrequest_example();
begin
  parseResource('communicationrequest-example.ttl');
end;

procedure TTurtleResourceTests.test_communication_example();
begin
  parseResource('communication-example.ttl');
end;

procedure TTurtleResourceTests.test_codesystem_nhin_purposeofuse();
begin
  parseResource('codesystem-nhin-purposeofuse.ttl');
end;

procedure TTurtleResourceTests.test_codesystem_example();
begin
  parseResource('codesystem-example.ttl');
end;

procedure TTurtleResourceTests.test_clinicalimpression_example();
begin
  parseResource('clinicalimpression-example.ttl');
end;

procedure TTurtleResourceTests.test_claimresponse_example();
begin
  parseResource('claimresponse-example.ttl');
end;

procedure TTurtleResourceTests.test_claim_example();
begin
  parseResource('claim-example.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_vision();
begin
  parseResource('claim-example-vision.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_vision_glasses();
begin
  parseResource('claim-example-vision-glasses.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_professional();
begin
  parseResource('claim-example-professional.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_pharmacy();
begin
  parseResource('claim-example-pharmacy.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_orthoplan();
begin
  parseResource('claim-example-oral-orthoplan.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_identifier();
begin
  parseResource('claim-example-oral-identifier.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_contained();
begin
  parseResource('claim-example-oral-contained.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_contained_identifier();
begin
  parseResource('claim-example-oral-contained-identifier.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_oral_average();
begin
  parseResource('claim-example-oral-average.ttl');
end;

procedure TTurtleResourceTests.test_claim_example_institutional();
begin
  parseResource('claim-example-institutional.ttl');
end;

procedure TTurtleResourceTests.test_careteam_example();
begin
  parseResource('careteam-example.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example();
begin
  parseResource('careplan-example.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_pregnancy();
begin
  parseResource('careplan-example-pregnancy.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_integrated();
begin
  parseResource('careplan-example-integrated.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_GPVisit();
begin
  parseResource('careplan-example-GPVisit.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f203_sepsis();
begin
  parseResource('careplan-example-f203-sepsis.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f202_malignancy();
begin
  parseResource('careplan-example-f202-malignancy.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f201_renal();
begin
  parseResource('careplan-example-f201-renal.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f003_pharynx();
begin
  parseResource('careplan-example-f003-pharynx.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f002_lung();
begin
  parseResource('careplan-example-f002-lung.ttl');
end;

procedure TTurtleResourceTests.test_careplan_example_f001_heart();
begin
  parseResource('careplan-example-f001-heart.ttl');
end;

procedure TTurtleResourceTests.test_bundle_transaction();
begin
  parseResource('bundle-transaction.ttl');
end;

procedure TTurtleResourceTests.test_bundle_response();
begin
  parseResource('bundle-response.ttl');
end;

procedure TTurtleResourceTests.test_bundle_example();
begin
  parseResource('bundle-example.ttl');
end;

procedure TTurtleResourceTests.test_binary_f006();
begin
  parseResource('binary-f006.ttl');
end;

procedure TTurtleResourceTests.test_binary_example();
begin
  parseResource('binary-example.ttl');
end;

procedure TTurtleResourceTests.test_basic_example2();
begin
  parseResource('basic-example2.ttl');
end;

procedure TTurtleResourceTests.test_basic_example();
begin
  parseResource('basic-example.ttl');
end;

procedure TTurtleResourceTests.test_basic_example_narrative();
begin
  parseResource('basic-example-narrative.ttl');
end;

procedure TTurtleResourceTests.test_auditevent_example();
begin
  parseResource('auditevent-example.ttl');
end;

procedure TTurtleResourceTests.test_auditevent_example_disclosure();
begin
  parseResource('auditevent-example-disclosure.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_vread();
begin
  parseResource('audit-event-example-vread.ttl');
end;

procedure TTurtleResourceTests.test_audit_event_example_search();
begin
  parseResource('audit-event-example-search.ttl');
end;

function compareObjects(path : String; t1, t2 : TTurtleObject) : String;
var
  c1, c2 : TTurtleComplex;
  l1, l2 : TTurtleList;
  o1, o2 : TTurtleObject;
  key, s : String;
  i : integer;
begin
  result := '';
  if (t1.ClassName <> t2.ClassName) then
    result := 'Objects at '+path+' have different types ("'+t1.ClassName+'"/"'+t2.ClassName+'")'
  else if t1 is TTurtleLiteral then
  begin
    if TTurtleLiteral(t1).value <> TTurtleLiteral(t2).value then
      result := 'Objects at '+path+' have different values ("'+TTurtleLiteral(t1).value+'"/"'+TTurtleLiteral(t2).value+'")'
    else if TTurtleLiteral(t1).type_ <> TTurtleLiteral(t2).type_ then
      result := 'Objects at '+path+' have different types ("'+TTurtleLiteral(t1).type_+'"/"'+TTurtleLiteral(t2).type_+'")'
  end
  else if t1 is TTurtleURL then
  begin
    if TTurtleURL(t1).uri <> TTurtleURL(t2).uri then
      result := 'Objects at '+path+' have different uris ("'+TTurtleURL(t1).uri+'"/"'+TTurtleURL(t2).uri+'")'
  end
  else if t1 is TTurtleComplex then
  begin
    c1 := TTurtleComplex(t1);
    c2 := TTurtleComplex(t2);
    if c1.predicates.Count <> c2.predicates.Count then
      result := 'Objects at '+path+' have different property counts ("'+inttostr(c1.predicates.Count)+'"/"'+inttostr(c2.predicates.Count)+'")'
    else
    begin
      for key in c1.predicates.Keys do
      begin
        o1 := c1.predicates[key];
        if not c2.predicates.TryGetValue(key, o2) then
          exit('Object at '+path+' has no property for "'+key+'" ("'+c1.predicates.SortedKeys.CommaText+'" vs "'+c2.predicates.SortedKeys.CommaText+'")')
        else
        begin
          s := compareObjects(path+' / '+key, o1, o2);
          if s <> '' then
            exit(s);
        end;
      end;
    end;
  end
  else if t1 is TTurtleList then
  begin
    l1 := TTurtleList(t1);
    l2 := TTurtleList(t2);
    if l1.List.Count <> l2.list.Count then
      result := 'Objects at '+path+' have different property counts ("'+inttostr(l1.list.Count)+'"/"'+inttostr(l2.list.Count)+'")'
    else
    begin
      for i := 0 to l1.List.count - 1 do
      begin
        s := compareObjects(path+' # '+inttostr(i), l1.List[i], l2.List[i]);
        if s <> '' then
          exit(s);
      end;
    end;
  end
end;

function compareTurtle(t1, t2 : TTurtleDocument; var msg : string) : boolean;
var
  i : integer;
  p1, p2 : TTurtlePredicate;
begin
  if t1.objects.Count <> t2.objects.Count then
    msg := 'Object Counts differ ('+inttostr(t1.objects.Count)+'/'+inttostr(t2.objects.Count)+')'
  else
  begin
    for i := 0 to t1.objects.Count - 1 do
    begin
      p1 := t1.objects[i];
      p2 := t2.objects[i];
      if (p1.URL.uri <> p2.URL.uri) then
        msg := 'URL mismatch: "'+p1.URL.uri+'"/"'+p2.URL.uri+'"'
      else
        msg := compareObjects(p1.URL.uri, p1.Value, p2.Value);
    end;
  end;
  result := msg = '';
end;

function CheckTurtleIsSame(src1, src2 : String; var msg : string) : boolean;
var
  t1, t2 : TTurtleDocument;
  f1, f2, cmd : String;
begin
  result := false;
  try
    t1 := TTurtleParser.parse(src1);
    try
      t2 := TTurtleParser.parse(src2);
      try
        result := compareTurtle(t1, t2, msg);
        if not result then
        begin
          f1 := MakeTempFilename +'-source.xml';
          f2 := MakeTempFilename +'-dest.xml';
      StringToFile(src1, f1, TEncoding.UTF8);
      StringToFile(src2, f2, TEncoding.UTF8);

//          TRDFGenerator.saveToFile(t1, f1);
//          TRDFGenerator.saveToFile(t2, f2);
          cmd := f1+' '+f2;
          ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
        end;
      finally
        t2.free;
      end;
    finally
      t1.free;
    end;
  except
    on e : Exception do
    begin
      msg := e.Message;
      f1 := MakeTempFilename +'-source.xml';
      f2 := MakeTempFilename +'-dest.xml';
      StringToFile(src1, f1, TEncoding.UTF8);
      StringToFile(src2, f2, TEncoding.UTF8);
      cmd := f1+' '+f2;
      ExecuteLaunch('open', '"C:\Program Files (x86)\WinMerge\WinMergeU.exe"', PChar(cmd), true);
    end;
  end;
end;

initialization
  TDUnitX.RegisterTestFixture(TTurtleTests);
  TDUnitX.RegisterTestFixture(TTurtleResourceTests);
end.
