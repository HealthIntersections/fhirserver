unit fsl_tests;

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

{$I fhir.inc}

interface

Uses
  {$IFDEF WINDOWS} Windows, {$ENDIF} SysUtils, Classes, {$IFNDEF FPC}Soap.EncdDecd, System.NetEncoding, {$ENDIF} SyncObjs, zlib,
  {$IFDEF FPC} FPCUnit, TestRegistry, RegExpr, {$ELSE} TestFramework, {$ENDIF} fsl_testing,
  IdGlobalProtocols,
  fsl_base, fsl_utilities, fsl_stream, fsl_threads, fsl_collections, fsl_fpc,
  fsl_xml,
  {$IFNDEF FPC}
  fsl_msxml,
  {$ENDIF}
  fsl_json, fsl_turtle, fsl_comparisons;

Type
  TFslTestString = class (TFslObject)
  private
    FString : String;
  public
    constructor Create(value : String);
    function Link :  TFslTestString; overload;
  end;

  TFslTestObject = class (TFslObject)
  private
    FValue: String;
  public
    constructor create(value : String); overload;
    property value : String read FValue write FValue;
  end;

  { TFslUtilitiesTestCases }

  TFslUtilitiesTestCases = class (TFslTestCase)
  published
    procedure testSemVer;
    procedure testUnicode;
  end;

  TFslGenericsTests = class (TFslTestCase)
  private
    function doSort(sender : TObject; const left, right : TFslTestObject) : integer;
  published
    procedure testSimple;
    procedure testiterate;
    procedure testRemove;
    procedure testAddAll;
    procedure testReplace;
    procedure testMap;
    procedure testSort;
  end;

  { TFslTestObjectList }

  TFslTestObjectList = class (TFslObjectList)
  private
    function GetEntry(iIndex: Integer): TFslTestObject;
    Function FindByValue(entry : TFslTestObject; Out iIndex: Integer): Boolean; Overload;
  protected
    function ItemClass : TFslObjectClass; override;
    Function CompareByValue(pA, pB: Pointer): Integer; Virtual;
  public
    Property Entries[iIndex : Integer] : TFslTestObject Read GetEntry; Default;
    Procedure SortedByValue;
    Function FindByValue(value: String; Out iIndex: Integer): Boolean; Overload;
    Function getByValue(value: String): TFslTestObject; Overload;
  end;

  { TFslCollectionsTests }

  TFslCollectionsTests = class (TFslTestCase)
  private
    list : TFslTestObjectList;
    procedure executeFail();
  published
    procedure testAdd;
    procedure testAddFail;
    procedure testSort;
    procedure testSort2;
  end;

  {$IFDEF FPC}
  TFslRegexTests = class (TFslTestCase)
  private
  published
    procedure testRegex;
  end;
  {$ENDIF}

  TXPlatformTests = class (TFslTestCase)
  private
    procedure test60sec;
  published
    procedure TesTFslObject;
    procedure TestCriticalSectionSimple;
    procedure TestCriticalSectionThreaded;
    procedure TestKCriticalSectionThreaded;
    procedure TestKCriticalSectionSimple;
    procedure TestSemaphore;
    procedure TestTemp;
    procedure TestFslDateTime;
    procedure TestFslFile;
    procedure TestRemoveAccents;
    procedure TestTimeZoneName;
  end;

Type
  TDecimalTests = Class (TFslTestCase)
  Private
    procedure testString(s, st, std : String);
    procedure TestAdd(s1,s2,s3:String);
    procedure TestMultiply(s1,s2,s3:String);
    procedure TestSubtract(s1,s2,s3:String);
    procedure TestDivide(s1,s2,s3:String);
    procedure TestDivInt(s1,s2,s3:String);
    procedure TestModulo(s1,s2,s3:String);
    procedure TestInteger(i : integer);
    procedure TestCardinal(i : cardinal);
    procedure TestInt64(i : int64);
    procedure TestRoundTrip(n1, n2, n3, t : String);
    procedure TestBoundsCase(v, low, high, ilow, ihigh : String);
    procedure TestTruncation(value : String; digits : integer; outcome : String; round : boolean);
  Published
    Procedure TestIsDecimal;

    Procedure TestAsInteger;
    Procedure TestStringSupport;
    Procedure TestAddition;
    Procedure TestMultiplication;
    Procedure TestBounds;
    Procedure TestNormalisedDecimal;
    Procedure TestInfinity;
    Procedure TestOverloading;
    Procedure TestTrunc;
  End;

Type
  TXmlParserTest = Class (TFslTestSuiteCase)
  Published
    procedure TestCase(Name : String); override;
  End;

  TXmlParserTests = class (TFslTestSuite)
  private
  public
    constructor Create; override;
  end;

  { TXmlParserTest2 }

  TXmlParserTest2 = Class (TFslTestCase)
  private
    procedure runXmlTest(fn : String);
  published
    procedure testUnicode;
    procedure testUtf8;
    procedure testUtf16;
  end;

  TXmlUtilsTest = Class (TFslTestCase)
  Published
    procedure TestUnPretty;
    procedure TestPretty;
    procedure TestNoPretty;
    procedure TestNoDense;
  End;

  TXPathParserTest = Class (TFslTestSuiteCase)
  Private
    tests : TMXmlDocument;
    functionNames : TStringList;
    procedure collectFunctionNames(xp : TMXPathExpressionNode);
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  Published
    procedure TestCase(Name : String); override;
  End;

  TXPathParserTests = class(TFslTestSuite)
  public
    constructor Create; override;
  end;

  TXPathEngineTest = Class (TFslTestSuiteCase)
  Private
    tests : TMXmlDocument;
    {$IFNDEF FPC}
    mstests : IXMLDOMDocument2;
    function findSampleMs(id : String) : IXMLDOMElement;
    procedure runMsTest(test : TMXmlElement; outcomes : TFslList<TMXmlElement>);
    {$ENDIF}
    function findTestCase(name : String) : TMXmlElement;
    function findSample(id : String) : TMXmlElement;
    procedure runXTest(test : TMXmlElement; outcomes : TFslList<TMXmlElement>);
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  Published
    procedure TestCase(Name : String); override;
  End;

  TXPathEngineTests = class (TFslTestSuite)
  public
    constructor Create; override;
  end;

  TXmlPatchTest = Class (TFslTestSuiteCase)
  Private
    tests : TMXmlDocument;
    engine : TXmlPatchEngine;
    // here for FPC to make the exception procedure event.
    test, target, patch, error, patched : TMXmlElement;
    procedure doExecute;
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  Published
    procedure TestCase(Name : String); override;
  End;

  TXmlPatchTests = class (TFslTestSuite)
  public
    constructor Create; override;
  end;

Type

  { TJsonTests }

  TJsonTests = Class (TFslTestCase)
  Private
    procedure jsonUnicodeTest(fn : String);
    procedure jsonTest(src : String; clss : TJsonNodeClass);
  Published
    procedure TestResource;
    procedure TestCustomDoc2;
    procedure TestCustomDoc2Loose;
    procedure TestCustomDecimal;
    procedure testSimple;
    procedure testUtf8n;
    procedure testUtf8;
    procedure testUtf16;
  end;

  TJsonPatchTest = Class (TFslTestSuiteCase)
  Private
    tests : TJsonArray;
    test : TJsonObject;
    engine : TJsonPatchEngine;
    procedure execute;
  public
    Procedure SetUp; override;
    procedure TearDown; override;
  Published
    procedure PatchTest(Name : String);
  End;

  TJsonPatchTests = class (TFslTestSuite)
  private
  public
    constructor Create; override;
  end;

  TTurtleTests = Class (TFslTestCase)
  Private
    procedure parseTTl(filename : String; ok : boolean);
  Published
    procedure test_double_lower_case_e1;
    procedure test_double_lower_case_e2();
    procedure test_empty_collection1();
    procedure test_empty_collection2();
    procedure test_first1();
//    procedure test_first2();
    procedure test_HYPHEN_MINUS_in_localNameNT();
    procedure test_HYPHEN_MINUS_in_localName();
    procedure test_IRI_spoNT();
    procedure test_IRI_subject();
    procedure test_IRI_with_all_punctuationNT();
    procedure test_IRI_with_all_punctuation();
    procedure test_IRI_with_eight_digit_numeric_escape();
    procedure test_IRI_with_four_digit_numeric_escape();
    procedure test_IRIREF_datatypeNT();
    procedure test_IRIREF_datatype();
    procedure test_labeled_blank_node_objectNT();
    procedure test_labeled_blank_node_object();
    procedure test_labeled_blank_node_subjectNT();
    procedure test_labeled_blank_node_subject();
    procedure test_labeled_blank_node_with_leading_digit();
    procedure test_labeled_blank_node_with_leading_underscore();
    procedure test_labeled_blank_node_with_non_leading_extras();
    procedure test_labeled_blank_node_with_PN_CHARS_BASE_character_boundaries();
    procedure test_langtagged_LONG();
    procedure test_langtagged_LONG_with_subtagNT();
    procedure test_langtagged_LONG_with_subtag();
    procedure test_langtagged_non_LONGNT();
    procedure test_langtagged_non_LONG();
    procedure test_lantag_with_subtagNT();
    procedure test_lantag_with_subtag();
    procedure test_lastNT();
    procedure test_last();
    procedure test_literal_falseNT();
    procedure test_literal_false();
    procedure test_LITERAL_LONG1();
    procedure test_LITERAL_LONG1_ascii_boundariesNT();
    procedure test_LITERAL_LONG1_ascii_boundaries();
    procedure test_LITERAL_LONG1_with_1_squoteNT();
    procedure test_LITERAL_LONG1_with_1_squote();
    procedure test_LITERAL_LONG1_with_2_squotesNT();
    procedure test_LITERAL_LONG1_with_2_squotes();
    procedure test_LITERAL_LONG1_with_UTF8_boundaries();
    procedure test_LITERAL_LONG2();
    procedure test_LITERAL_LONG2_ascii_boundariesNT();
    procedure test_LITERAL_LONG2_ascii_boundaries();
    procedure test_LITERAL_LONG2_with_1_squoteNT();
    procedure test_LITERAL_LONG2_with_1_squote();
    procedure test_LITERAL_LONG2_with_2_squotesNT();
    procedure test_LITERAL_LONG2_with_2_squotes();
    procedure test_LITERAL_LONG2_with_REVERSE_SOLIDUSNT();
    procedure test_LITERAL_LONG2_with_REVERSE_SOLIDUS();
    procedure test_LITERAL_LONG2_with_UTF8_boundaries();
    procedure test_literal_trueNT();
    procedure test_literal_true();
    procedure test_literal_with_BACKSPACENT();
    procedure test_literal_with_BACKSPACE();
    procedure test_literal_with_CARRIAGE_RETURNNT();
    procedure test_literal_with_CARRIAGE_RETURN();
    procedure test_literal_with_CHARACTER_TABULATIONNT();
    procedure test_literal_with_CHARACTER_TABULATION();
    procedure test_literal_with_escaped_BACKSPACE();
    procedure test_literal_with_escaped_CARRIAGE_RETURN();
    procedure test_literal_with_escaped_CHARACTER_TABULATION();
    procedure test_literal_with_escaped_FORM_FEED();
    procedure test_literal_with_escaped_LINE_FEED();
//    procedure test_literal_with_FORM_FEEDNT();
    procedure test_literal_with_FORM_FEED();
    procedure test_literal_with_LINE_FEEDNT();
    procedure test_literal_with_LINE_FEED();
    procedure test_literal_with_numeric_escape4NT();
    procedure test_literal_with_numeric_escape4();
    procedure test_literal_with_numeric_escape8();
    procedure test_literal_with_REVERSE_SOLIDUSNT();
    procedure test_literal_with_REVERSE_SOLIDUS();
    procedure test_LITERAL_with_UTF8_boundariesNT();
    procedure test_LITERAL1NT();
    procedure test_LITERAL1();
    procedure test_LITERAL1_all_controlsNT();
    procedure test_LITERAL1_all_controls();
    procedure test_LITERAL1_all_punctuationNT();
    procedure test_LITERAL1_all_punctuation();
//    procedure test_LITERAL1_ascii_boundariesNT();
    procedure test_LITERAL1_ascii_boundaries();
    procedure test_LITERAL1_with_UTF8_boundaries();
    procedure test_LITERAL2();
    procedure test_LITERAL2_ascii_boundariesNT();
    procedure test_LITERAL2_ascii_boundaries();
    procedure test_LITERAL2_with_UTF8_boundaries();
    procedure test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundariesNT();
    procedure test_localName_with_assigned_nfc_bmp_PN_CHARS_BASE_character_boundaries();
    procedure test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundariesNT();
    procedure test_localName_with_assigned_nfc_PN_CHARS_BASE_character_boundaries();
//    procedure test_localname_with_COLONNT();
//    procedure test_localname_with_COLON();
    procedure test_localName_with_leading_digitNT();
    procedure test_localName_with_leading_digit();
    procedure test_localName_with_leading_underscoreNT();
    procedure test_localName_with_leading_underscore();
    procedure test_localName_with_nfc_PN_CHARS_BASE_character_boundariesNT();
    procedure test_localName_with_nfc_PN_CHARS_BASE_character_boundaries();
    procedure test_localName_with_non_leading_extrasNT();
    procedure test_localName_with_non_leading_extras();
    procedure test_negative_numericNT();
    procedure test_negative_numeric();
    procedure test_nested_blankNodePropertyListsNT();
    procedure test_nested_blankNodePropertyLists();
    procedure test_nested_collectionNT();
    procedure test_nested_collection();
    procedure test_number_sign_following_localNameNT();
//    procedure test_number_sign_following_localName();
    procedure test_number_sign_following_PNAME_NSNT();
//    procedure test_number_sign_following_PNAME_NS();
    procedure test_numeric_with_leading_0NT();
    procedure test_numeric_with_leading_0();
    procedure test_objectList_with_two_objectsNT();
    procedure test_objectList_with_two_objects();
    procedure test_old_style_base();
    procedure test_old_style_prefix();
    procedure test_percent_escaped_localNameNT();
//    procedure test_percent_escaped_localName();
    procedure test_positive_numericNT();
    procedure test_positive_numeric();
    procedure test_predicateObjectList_with_two_objectListsNT();
    procedure test_predicateObjectList_with_two_objectLists();
//    procedure test_prefix_only_IRI();
    procedure test_prefix_reassigned_and_usedNT();
    procedure test_prefix_reassigned_and_used();
    procedure test_prefix_with_non_leading_extras();
    procedure test_prefix_with_PN_CHARS_BASE_character_boundaries();
    procedure test_prefixed_IRI_object();
    procedure test_prefixed_IRI_predicate();
    procedure test_prefixed_name_datatype();
    procedure test_repeated_semis_at_end();
    procedure test_repeated_semis_not_at_endNT();
    procedure test_repeated_semis_not_at_end();
    procedure test_reserved_escaped_localNameNT();
//    procedure test_reserved_escaped_localName();
    procedure test_sole_blankNodePropertyList();
    procedure test_SPARQL_style_base();
    procedure test_SPARQL_style_prefix();
    procedure test_turtle_eval_bad_01();
    procedure test_turtle_eval_bad_02();
    procedure test_turtle_eval_bad_03();
//    procedure test_turtle_eval_bad_04();
    procedure test_turtle_eval_struct_01NT();
    procedure test_turtle_eval_struct_01();
    procedure test_turtle_eval_struct_02NT();
    procedure test_turtle_eval_struct_02();
    procedure test_turtle_subm_01NT();
    procedure test_turtle_subm_01();
    procedure test_turtle_subm_02NT();
    procedure test_turtle_subm_02();
    procedure test_turtle_subm_03NT();
    procedure test_turtle_subm_03();
    procedure test_turtle_subm_04NT();
    procedure test_turtle_subm_04();
    procedure test_turtle_subm_05NT();
    procedure test_turtle_subm_05();
    procedure test_turtle_subm_06NT();
    procedure test_turtle_subm_06();
    procedure test_turtle_subm_07NT();
    procedure test_turtle_subm_07();
    procedure test_NT();
    procedure test_turtle_subm_08();
    procedure test_turtle_subm_09NT();
    procedure test_turtle_subm_09();
    procedure test_turtle_subm_10NT();
    procedure test_turtle_subm_10();
    procedure test_turtle_subm_11NT();
    procedure test_turtle_subm_11();
    procedure test_turtle_subm_12NT();
    procedure test_turtle_subm_12();
    procedure test_turtle_subm_13NT();
    procedure test_turtle_subm_13();
    procedure test_turtle_subm_14NT();
    procedure test_turtle_subm_14();
    procedure test_turtle_subm_15NT();
    procedure test_turtle_subm_15();
    procedure test_turtle_subm_16NT();
    procedure test_turtle_subm_16();
    procedure test_turtle_subm_17NT();
    procedure test_turtle_subm_17();
    procedure test_turtle_subm_18NT();
    procedure test_turtle_subm_18();
    procedure test_turtle_subm_19NT();
    procedure test_turtle_subm_19();
    procedure test_turtle_subm_20NT();
    procedure test_turtle_subm_20();
    procedure test_turtle_subm_21NT();
    procedure test_turtle_subm_21();
    procedure test_turtle_subm_22NT();
    procedure test_turtle_subm_22();
    procedure test_turtle_subm_23NT();
    procedure test_turtle_subm_23();
    procedure test_turtle_subm_24NT();
    procedure test_turtle_subm_24();
    procedure test_turtle_subm_25NT();
    procedure test_turtle_subm_25();
    procedure test_turtle_subm_26NT();
    procedure test_turtle_subm_26();
    procedure test_turtle_subm_27NT();
    procedure test_turtle_subm_27();
    procedure test_turtle_syntax_bad_base_01();
    procedure test_turtle_syntax_bad_base_02();
    procedure test_turtle_syntax_bad_base_03();
    procedure test_turtle_syntax_bad_esc_01();
    procedure test_turtle_syntax_bad_esc_02();
    procedure test_turtle_syntax_bad_esc_03();
    procedure test_turtle_syntax_bad_esc_04();
    procedure test_turtle_syntax_bad_kw_01();
    procedure test_turtle_syntax_bad_kw_02();
    procedure test_turtle_syntax_bad_kw_03();
    procedure test_turtle_syntax_bad_kw_04();
    procedure test_turtle_syntax_bad_kw_05();
    procedure test_turtle_syntax_bad_lang_01();
    procedure test_turtle_syntax_bad_LITERAL2_with_langtag_and_datatype();
    procedure test_turtle_syntax_bad_ln_dash_start();
    procedure test_turtle_syntax_bad_ln_escape();
    procedure test_turtle_syntax_bad_ln_escape_start();
    procedure test_turtle_syntax_bad_missing_ns_dot_end();
    procedure test_turtle_syntax_bad_missing_ns_dot_start();
    procedure test_turtle_syntax_bad_n3_extras_01();
    procedure test_turtle_syntax_bad_n3_extras_02();
    procedure test_turtle_syntax_bad_n3_extras_03();
    procedure test_turtle_syntax_bad_n3_extras_04();
    procedure test_turtle_syntax_bad_n3_extras_05();
    procedure test_turtle_syntax_bad_n3_extras_07();
    procedure test_turtle_syntax_bad_n3_extras_08();
    procedure test_turtle_syntax_bad_n3_extras_09();
    procedure test_turtle_syntax_bad_n3_extras_10();
    procedure test_turtle_syntax_bad_n3_extras_11();
    procedure test_turtle_syntax_bad_n3_extras_12();
    procedure test_turtle_syntax_bad_n3_extras_13();
    procedure test_turtle_syntax_bad_ns_dot_end();
    procedure test_turtle_syntax_bad_ns_dot_start();
    procedure test_turtle_syntax_bad_num_01();
    procedure test_turtle_syntax_bad_num_02();
    procedure test_turtle_syntax_bad_num_03();
    procedure test_turtle_syntax_bad_num_04();
    procedure test_turtle_syntax_bad_num_05();
    procedure test_turtle_syntax_bad_number_dot_in_anon();
    procedure test_turtle_syntax_bad_pname_01();
    procedure test_turtle_syntax_bad_pname_02();
    procedure test_turtle_syntax_bad_prefix_01();
    procedure test_turtle_syntax_bad_prefix_02();
    procedure test_turtle_syntax_bad_prefix_03();
    procedure test_turtle_syntax_bad_prefix_04();
    procedure test_turtle_syntax_bad_prefix_05();
    procedure test_turtle_syntax_bad_string_01();
    procedure test_turtle_syntax_bad_string_02();
    procedure test_turtle_syntax_bad_string_03();
    procedure test_turtle_syntax_bad_string_04();
    procedure test_turtle_syntax_bad_string_05();
    procedure test_turtle_syntax_bad_string_06();
    procedure test_turtle_syntax_bad_string_07();
    procedure test_turtle_syntax_bad_struct_01();
    procedure test_turtle_syntax_bad_struct_02();
    procedure test_turtle_syntax_bad_struct_03();
    procedure test_turtle_syntax_bad_struct_04();
    procedure test_turtle_syntax_bad_struct_05();
    procedure test_turtle_syntax_bad_struct_06();
    procedure test_turtle_syntax_bad_struct_07();
    procedure test_turtle_syntax_bad_struct_08();
    procedure test_turtle_syntax_bad_struct_09();
    procedure test_turtle_syntax_bad_struct_10();
    procedure test_turtle_syntax_bad_struct_11();
    procedure test_turtle_syntax_bad_struct_12();
    procedure test_turtle_syntax_bad_struct_13();
    procedure test_turtle_syntax_bad_struct_14();
    procedure test_turtle_syntax_bad_struct_15();
    procedure test_turtle_syntax_bad_struct_16();
    procedure test_turtle_syntax_bad_struct_17();
    procedure test_turtle_syntax_bad_uri_02();
    procedure test_turtle_syntax_bad_uri_03();
    procedure test_turtle_syntax_base_01();
    procedure test_turtle_syntax_base_02();
    procedure test_turtle_syntax_base_03();
    procedure test_turtle_syntax_base_04();
    procedure test_turtle_syntax_blank_label();
    procedure test_turtle_syntax_bnode_01();
    procedure test_turtle_syntax_bnode_02();
    procedure test_turtle_syntax_bnode_03();
    procedure test_turtle_syntax_bnode_04();
    procedure test_turtle_syntax_bnode_05();
    procedure test_turtle_syntax_bnode_06();
    procedure test_turtle_syntax_bnode_07();
    procedure test_turtle_syntax_bnode_08();
    procedure test_turtle_syntax_bnode_09();
    procedure test_turtle_syntax_bnode_10();
    procedure test_turtle_syntax_datatypes_01();
    procedure test_turtle_syntax_datatypes_02();
    procedure test_turtle_syntax_file_01();
    procedure test_turtle_syntax_file_02();
    procedure test_turtle_syntax_file_03();
    procedure test_turtle_syntax_kw_01();
    procedure test_turtle_syntax_kw_02();
    procedure test_turtle_syntax_kw_03();
    procedure test_turtle_syntax_lists_01();
    procedure test_turtle_syntax_lists_02();
    procedure test_turtle_syntax_ln_dots();
    procedure test_turtle_syntax_ns_dots();
    procedure test_turtle_syntax_number_01();
    procedure test_turtle_syntax_number_02();
    procedure test_turtle_syntax_number_03();
    procedure test_turtle_syntax_number_04();
    procedure test_turtle_syntax_number_06();
    procedure test_turtle_syntax_number_07();
    procedure test_turtle_syntax_number_09();
    procedure test_turtle_syntax_number_10();
    procedure test_turtle_syntax_number_11();
    procedure test_turtle_syntax_pname_esc_01();
    procedure test_turtle_syntax_pname_esc_02();
    procedure test_turtle_syntax_pname_esc_03();
    procedure test_turtle_syntax_prefix_01();
    procedure test_turtle_syntax_prefix_03();
    procedure test_turtle_syntax_prefix_04();
    procedure test_turtle_syntax_prefix_07();
    procedure test_turtle_syntax_prefix_08();
    procedure test_turtle_syntax_prefix_09();
    procedure test_turtle_syntax_str_esc_01();
    procedure test_turtle_syntax_str_esc_02();
    procedure test_turtle_syntax_str_esc_03();
    procedure test_turtle_syntax_string_01();
    procedure test_turtle_syntax_string_02();
    procedure test_turtle_syntax_string_03();
    procedure test_turtle_syntax_string_04();
    procedure test_turtle_syntax_string_05();
    procedure test_turtle_syntax_string_06();
    procedure test_turtle_syntax_string_07();
    procedure test_turtle_syntax_string_08();
    procedure test_turtle_syntax_string_09();
    procedure test_turtle_syntax_string_10();
    procedure test_turtle_syntax_string_11();
    procedure test_turtle_syntax_struct_01();
    procedure test_turtle_syntax_struct_02();
    procedure test_turtle_syntax_struct_03();
    procedure test_turtle_syntax_struct_04();
    procedure test_turtle_syntax_struct_05();
    procedure test_turtle_syntax_uri_01();
    procedure test_turtle_syntax_uri_02();
    procedure test_turtle_syntax_uri_03();
    procedure test_turtle_syntax_uri_04();
    procedure test_two_LITERAL_LONG2sNT();
    procedure test_two_LITERAL_LONG2s();
    procedure test_underscore_in_localNameNT();
    procedure test_underscore_in_localName();
    procedure test_anonymous_blank_node_object();
    procedure test_anonymous_blank_node_subject();
    procedure test_bareword_a_predicateNT();
    procedure test_bareword_a_predicate();
    procedure test_bareword_decimalNT();
    procedure test_bareword_decimal();
    procedure test_bareword_doubleNT();
    procedure test_bareword_double();
    procedure test_bareword_integer();
    procedure test_blankNodePropertyList_as_objectNT();
    procedure test_blankNodePropertyList_as_object();
    procedure test_blankNodePropertyList_as_subjectNT();
    procedure test_blankNodePropertyList_containing_collectionNT();
    procedure test_blankNodePropertyList_containing_collection();
    procedure test_blankNodePropertyList_with_multiple_triplesNT();
    procedure test_blankNodePropertyList_with_multiple_triples();
    procedure test_collection_objectNT();
    procedure test_collection_object();
    procedure test_collection_subjectNT();
    procedure test_collection_subject();
    procedure test_comment_following_localName();
    procedure test_comment_following_PNAME_NSNT();
    procedure test_comment_following_PNAME_NS();
    procedure test__default_namespace_IRI();
  End;

   (*
Type
  TDigitalSignatureTests = Class (TFslTestCase)
  private
    procedure testFile(filename : String);
  Published
    Procedure testFileRSA;
    Procedure testFileDSA;
    Procedure testFileJames;
    Procedure testGenRSA_1;
    Procedure testGenRSA_256;
//    Procedure testGenDSA_1;
//    Procedure testGenDSA_256;
  End;
     *)

  TTarGZParserTests = Class (TFslTestCase)
  private
    function load(filename : String) : TFslList<TFslNameBuffer>;
  Published
    Procedure testPackage;
    Procedure testTzData;
  End;

procedure registerTests;

implementation

{ TFslUtilitiesTestCases }

procedure TFslUtilitiesTestCases.testSemVer;
begin
  AssertTrue(TSemVer.isMoreRecent('0.10.0', '0.2.0'));
end;

const
  bu1 : TBytes = [232, 131, 140, 230, 153, 175, 32, 229, 143, 145, 231, 142, 176, 230, 152, 175, 228, 184, 128, 228, 184, 170, 229, 142, 159, 229, 173, 144, 229, 158, 139, 228, 184, 180, 229, 186, 138, 232, 167, 130, 229, 175, 159, 230, 140, 135, 230, 160, 135]; { 239, 188, 140, 229, 185 }
  bu2 : TBytes = [69, 75, 71, 32, 80, 66, 32, 82, 39, 39, 32, 230, 179, 162, 230, 140, 129, 231, 187, 173, 230, 151, 182, 233, 151, 180, 239, 188, 136, 230, 140, 129, 231, 187, 173, 230, 151, 182, 233, 149, 191, 227, 128, 129, 230, 151, 182, 233, 149, 191, 227, 128, 129, 230, 151, 182, 233, 151, 180, 233, 149, 191, 229, 186, 166, 227, 128, 129, 230, 151, 182, 233, 151, 180, 227, 128, 129, 230, 151, 182, 233, 151, 180, 233, 149, 191, 231, 159, 173, 227, 128, 129, 228, 184, 186, 230, 151, 182, 227, 128, 129, 228, 184, 186, 230, 156, 159, 227, 128, 129, 229, 142, 134, 230, 151, 182, 227, 128, 129, 229, 187, 182, 231, 187, 173, 230, 151, 182, 233, 151, 180, 227, 128, 129, 230, 140, 129, 228, 185, 133, 230, 151, 182, 233, 151, 180, 227, 128, 129, 230, 140, 129, 231, 187, 173, 230, 156, 159, 239, 188, 137, 32, 65, 86, 82, 32, 229, 175, 188, 232, 129, 148];

procedure TFslUtilitiesTestCases.testUnicode;
var
  s : String;
  b : TBytes;
begin
  s := TEncoding.UTF8.GetString(bu2);
  AssertTrue(s = 'EKG PB R'''' 波持续时间（持续时长、时长、时间长度、时间、时间长短、为时、为期、历时、延续时间、持久时间、持续期） AVR 导联');

  s := '背景 发现是一个原子型临床观察指标';
  b := TEncoding.UTF8.GetBytes(s);
  s := TEncoding.UTF8.GetString(b);

  s := TEncoding.UTF8.GetString(bu1);
  AssertTrue(s = '背景 发现是一个原子型临床观察指标');
end;

{ TXmlParserTest2 }

procedure TXmlParserTest2.testUnicode;
begin
  runXmlTest('xml-unicode.xml');
end;

procedure TXmlParserTest2.testUtf8;
begin
  runXmlTest('xml-utf8.xml');
end;

procedure TXmlParserTest2.testUtf16;
begin
  runXmlTest('xml-utf16.xml');
end;

procedure TXmlParserTest2.runXmlTest(fn : String);
var
  xml : TMXmlDocument;
  e : TMXmlElement;
  b : TXmlBuilder;
  fnSrc, fnDst : string;
  s : String;
  ok : boolean;
begin
  fnSrc := TestSettings.serverTestFile(['testcases', 'xml', fn]);
  fnDst := MakeTempFilename();
  xml := TMXmlParser.parseFile(fnSrc, []);
  try
    assertTrue(xml <> nil);
    s := xml.ToXml();
    StringToFile(s, fnDst, TEncoding.UTF8);
    ok := CheckXMLIsSame(fnSrc, fnDst, s);
    assertTrue(ok, s);

    b := TMXmlBuilder.Create;
    try
      b.CurrentNamespaces.DefaultNS := '';
      b.IsPretty := true;
      b.Start;
      b.open('base');
      e := xml.docElement.firstElement;
      b.AddAttribute('value', e.attribute['value']);
      b.Tag('attr');
      e := e.nextElement;
      b.TagText('text', e.Text);
      b.close('base');
      b.Finish;
      s := b.Build();
    finally
      b.Free;
    end;
    StringToFile(s, fnDst, TEncoding.UTF8);
    ok := CheckXMLIsSame(fnSrc, fnDst, s);
    assertTrue(ok, s);

    b := TFslXmlBuilder.Create;
    try
      b.CurrentNamespaces.DefaultNS := '';
      b.IsPretty := true;
      b.Start;
      b.open('base');
      e := xml.docElement.firstElement;
      b.AddAttribute('value', e.attribute['value']);
      b.Tag('attr');
      e := e.nextElement;
      b.TagText('text', e.Text);
      b.close('base');
      b.Finish;
      s := b.Build();
    finally
      b.Free;
    end;
    StringToFile(s, fnDst, TEncoding.UTF8);
    ok := CheckXMLIsSame(fnSrc, fnDst, s);
    assertTrue(ok, s);
  finally
    xml.free;
  end;
end;


{$IFDEF FPC}
{ TFslRegexTests }

procedure TFslRegexTests.testRegex;
var
  this : TRegExpr;
begin
  this := TRegExpr.create('(([a-z])+:)*((%[0-9a-fA-F]{2})|[&''\\(\\)*+,;:@_~?!$\\/\\-\\#.\\=a-zA-Z0-9])+');
  try
    assertTrue(this.Exec('http://a.example/p'));
  finally
    this.free;
  end;
end;
{$ENDIF}

{ TFslGenericsTests }

{$HINTS OFF}
procedure TFslGenericsTests.testSimple;
var
  l : TFslList<TFslObject>;
  x : TFslObject;
begin
  // you should get one leak when you execute these tests. this exists to make sure that the leak tracking system is working
  x := TFslObject.Create;
  l := TFslList<TFslObject>.create;
  try
    l.Add(TFslObject.Create);
    assertTrue(l.Count = 1, 'Count should be 1');
  finally
    l.Free;
  end;
end;
{$HINTS ON}

function TFslGenericsTests.doSort(sender : TObject; const left, right : TFslTestObject) : integer;
begin
  result := CompareStr(left.value, right.value);
end;

procedure TFslGenericsTests.testSort;
var
  list : TFslList<TFslTestObject>;
begin
  list := TFslList<TFslTestObject>.Create;
  try
    list.Add(TFslTestObject.Create('a'));
    list.SortE(doSort);
    assertTrue(list.Count = 1);
    assertTrue(list[0].value = 'a');
    list.Insert(0, TFslTestObject.Create('b'));
    assertTrue(list.Count = 2);
    assertTrue(list[0].value = 'b');
    assertTrue(list[1].value = 'a');
    list.SortE(doSort);
    assertTrue(list.Count = 2);
    assertTrue(list[0].value = 'a');
    assertTrue(list[1].value = 'b');
    list.Insert(1, TFslTestObject.Create('c'));
    assertTrue(list.Count = 3);
    assertTrue(list[0].value = 'a');
    assertTrue(list[1].value = 'c');
    assertTrue(list[2].value = 'b');
    list.SortE(doSort);
    assertTrue(list.Count = 3);
    assertTrue(list[0].value = 'a');
    assertTrue(list[1].value = 'b');
    assertTrue(list[2].value = 'c');
  finally
    list.Free;
  end;
end;

procedure TFslGenericsTests.testAddAll;
var
  l : TFslList<TFslObject>;
  l2 : TFslList<TFslTestString>;
  o : TFslTestString;
begin
  l := TFslList<TFslObject>.create;
  l2 := TFslList<TFslTestString>.create;
  try
    l.Add(TFslObject.Create);
    l2.Add(TFslTestString.create('test'));
    for o in l2 do
      l.add(o.Link);
    assertTrue(l.Count = 2);
  finally
    l.Free;
    l2.Free;
  end;
end;

procedure TFslGenericsTests.testRemove;
var
  l : TFslList<TFslObject>;
begin
  l := TFslList<TFslObject>.create;
  try
    l.Add(TFslObject.Create);
    assertTrue(l.Count = 1);
    l.Delete(0);
    assertTrue(l.Count = 0);
    l.Add(TFslObject.Create);
    assertTrue(l.Count = 1);
  finally
    l.Free;
  end;
end;

procedure TFslGenericsTests.testReplace;
var
  l : TFslList<TFslObject>;
begin
  l := TFslList<TFslObject>.create;
  try
    l.Add(TFslObject.Create);
    l[0] := TFslObject.Create;
    assertTrue(l.Count = 1);
  finally
    l.Free;
  end;
end;

procedure TFslGenericsTests.testIterate;
var
  l : TFslList<TFslObject>;
  c : integer;
  o : TFslObject;
begin
  l := TFslList<TFslObject>.create;
  try
    l.Add(TFslObject.Create);
    l.Add(TFslObject.Create);
    l.Add(TFslObject.Create);
    c := 0;
    for o in l do
      if (o = l[c]) then
        inc(c);
    if c <> 3 then
      raise ETestCase.create('Wrong Count');
    assertTrue(l.Count = 3);
  finally
    l.Free;
  end;
end;

procedure TFslGenericsTests.testMap;
var
  map : TFslMap<TFslTestString>;
begin
  map := TFslMap<TFslTestString>.create('tests');
  try
    map.Add('test1', TFslTestString.create('test1'));
    map.Add('test2', TFslTestString.create('test2'));
    map.AddOrSetValue('test2', TFslTestString.create('test3'));
    if map['test1'].FString <> 'test1' then
      raise ETestCase.create('Mismatch');
    if map['test2'].FString <> 'test3' then
      raise ETestCase.create('Mismatch');
    map.Remove('1est1');
    assertTrue(map.Count = 2);
  finally
    map.Free;
  end;
end;

{ TFslTestString }

constructor TFslTestString.create(value: String);
begin
  inherited Create;
  FString := value;
end;

function TFslTestString.Link: TFslTestString;
begin
 result := TFslTestString(inherited link);
end;

{ TXmlTests }

constructor TXmlPatchTests.Create;
var
  tests : TMXmlDocument;
  test : TMXmlElement;
begin
  inherited create;
  tests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r4', 'patch', 'xml-patch-tests.xml']), [xpResolveNamespaces]);
  try
    test := tests.document.first;
    while test <> nil do
    begin
      if test.Name = 'case' then
        AddTest(TXmlPatchTest.create(test.attribute['name']));
      test := test.Next;
    end;
  finally
    tests.Free;
  end;
end;

{ TXmlPatchTest }

procedure TXmlPatchTest.doExecute();
begin
  engine.execute(tests, target, patch);
end;

procedure TXmlPatchTest.TestCase(Name: String);
var
  s : String;
  ok : boolean;
begin
  test := tests.document.first;
  while test <> nil do
  begin
    if (test.Name = 'case') and (name = test.attribute['name']) then
    begin
      target := test.element('target');
      patch := test.element('patch');
      error := test.element('error');
      patched := test.element('patched');

      if (error <> nil) then
        assertWillRaise(doExecute, EXmlException, error.text)
      else
      begin
        engine.execute(tests, target, patch);
        StringToFile(target.first.ToXml(true), 'c:\temp\outcome.xml', TEncoding.UTF8);
        StringToFile(patched.first.ToXml(true), 'c:\temp\patched.xml', TEncoding.UTF8);
        ok := CheckXMLIsSame('c:\temp\patched.xml', 'c:\temp\outcome.xml', s);
        assertTrue(ok, s);
      end;
    end;
    test := test.Next;
  end;
end;

procedure TXmlPatchTest.setup;
begin
  tests := TMXmlParser.ParseFile(TestSettings.fhirTestFile(['r4', 'patch', 'xml-patch-tests.xml']), [xpResolveNamespaces, xpDropWhitespace]);
  engine := TXmlPatchEngine.Create;
end;

procedure TXmlPatchTest.TearDown;
begin
  engine.Free;
  tests.Free;
end;

{ TXmlParserTest }

procedure TXmlParserTest.TestCase(Name: String);
var
  xml : TMXmlElement;
begin
  xml := TMXmlParser.parseFile(TestSettings.serverTestFile(['testcases', 'xml', name]), []);
  try
    assertPass();
  finally
    xml.Free;
  end;
end;

{ TXmlParserTests }

constructor TXmlParserTests.Create;
var
  sr : TSearchRec;
begin
  inherited Create;
  if FindFirst(TestSettings.serverTestFile(['testcases', 'xml', '*.xml']), faAnyFile, SR) = 0 then
  repeat
    AddTest(TXmlParserTest.Create(sr.Name));
  until FindNext(SR) <> 0;
end;

{ TXPathParserTests }

constructor TXPathParserTests.Create;
var
  tests : TMXmlDocument;
  path : TMXmlElement;
  i : integer;
begin
  inherited Create;
  tests := TMXmlParser.ParseFile(TestSettings.serverTestFile(['testcases', 'xml', 'xpath-parser-tests.xml']), [xpDropWhitespace, xpDropComments]);
  try
    i := 0;
    path := tests.document.first;
    while path <> nil do
    begin
      AddTest(TXPathParserTest.create(inttostr(i)));
      inc(i);
      path := path.next;
    end;
  finally
    tests.Free;
  end;
end;

{ TXPathTests }

{
function TXPathTests.findTest(name: String): TMXmlElement;
var
  res, path : TMXmlElement;
begin
  result := nil;
  res := tests.document.first;
  while res <> nil do
  begin
    path := res.first;
    while path <> nil do
    begin
      if (path.attribute['path'] = name) then
        exit(path);
      path := path.Next;
    end;
    res := res.Next;
  end;
end;

function XpathForPath(path : string):string;
var
  p : TArray<String>;
  b : TStringBuilder;
  s : String;
begin
  p := path.Split(['.']);
  b := TStringBuilder.Create;
  try
    for s in p do
    begin
      if b.Length > 0 then
        b.Append('/');
      b.Append('f:');
      b.Append(s);
    end;
    result := b.ToString;
  finally
    b.Free;
  end;
end;
}

procedure TXPathParserTest.collectFunctionNames(xp: TMXPathExpressionNode);
var
  node : TMXPathExpressionNode;
begin
  if xp = nil then
    exit;
  if xp.NodeType = xentFunction then
  begin
    if functionNames.IndexOf(xp.value) = -1 then
      functionNames.Add(xp.value);
  end;
  for node in xp.filters do
    collectFunctionNames(node);
  for node in xp.Params do
    collectFunctionNames(node);
  collectFunctionNames(xp.next);
  collectFunctionNames(xp.Group);
  collectFunctionNames(xp.NextOp);
end;

procedure TXPathParserTest.TestCase(Name: String);
var
  test : TMXmlElement;
  xp : TMXPathExpressionNode;
begin
  test := tests.document.children[StrToInt(name)];
  xp := TMXmlParser.parseXPath(test.attribute['value']);
  try
    collectFunctionNames(xp);
    assertPass();
  finally
    xp.Free
  end;
end;

procedure TXPathParserTest.setup;
begin
  tests := TMXmlParser.ParseFile(TestSettings.serverTestFile(['testcases', 'xml', 'xpath-parser-tests.xml']), [xpDropWhitespace, xpDropComments]);
  functionNames := TStringList.Create;
end;

procedure TXPathParserTest.TearDown;
begin
  functionNames.Free;
  tests.Free;
end;

constructor TXPathEngineTests.Create;
var
  tests : TMXmlDocument;
  tcase : TMXmlElement;
begin
  inherited create;
  tests := TMXmlParser.ParseFile(TestSettings.serverTestFile(['testcases', 'xml', 'xpath-tests.xml']), [xpResolveNamespaces]);
  try
    tcase := tests.document.firstElement;
    while tcase <> nil do
    begin
      if tcase.Name = 'case' then
        addTest(TXPathEngineTest.create(tcase.attribute['name']));
      tcase := tcase.nextElement;
    end;
  finally
    tests.Free;
  end;
end;

{ TXPathEngineTest }

function TXPathEngineTest.findSample(id: String): TMXmlElement;
var
  sample : TMXmlElement;
begin
  sample := tests.document.firstElement;
  while sample <> nil do
  begin
    if sample.Name = 'sample' then
    begin
      if (sample.attribute['id'] = id) then
        exit(sample);
    end;
    sample := sample.next;
  end;
  result := nil;
end;

{$IFNDEF FPC}
function TXPathEngineTest.findSampleMs(id: String): IXMLDOMElement;
var
  sample : IXMLDOMElement;
begin
  sample := TMsXmlParser.FirstChild(mstests.documentElement);
  while sample <> nil do
  begin
    if sample.nodeName = 'sample' then
    begin
      if (sample.getAttribute('id') = id) then
        exit(sample);
    end;
    sample := TMsXmlParser.NextSibling(sample);
  end;
  result := nil;
end;

procedure TXPathEngineTest.runMsTest(test : TMXmlElement; outcomes : TFslList<TMXmlElement>);
var
  focus : IXMLDOMElement;
  outcome: TMXmlElement;
  nodes : IXMLDOMNodeList;
  node : IXMLDOMNode;
  i : integer;
begin
  if (test.attribute['ms'] = 'no') then
    exit;
  for outcome in outcomes do
    if not StringArrayExistsSensitive(['text', 'attribute', 'element', 'comment'], outcome.attribute['type']) then
      exit;

  focus := TMsXmlParser.FirstChild(findSampleMs(test.attribute['id']));
  nodes := focus.selectNodes(test.element('xpath').attribute['value']);
  if test.element('outcomes').HasAttribute['count'] then
    assertTrue(StrToInt(test.element('outcomes').attribute['count']) = nodes.length, 'MS: Wrong number of nodes returned - expected '+test.element('outcomes').attribute['count']+', found '+inttostr(nodes.length))
  else
  begin
    assertTrue(outcomes.Count = nodes.length, 'MS: Wrong number of nodes returned - expected '+inttostr(outcomes.Count)+', found '+inttostr(nodes.length));
    for i := 0 to outcomes.Count - 1 do
    begin
      node := nodes.item[i];
      outcome := outcomes[i];
      if outcome.attribute['type'] = 'string' then
      begin
        raise ETestCase.create('not done yet');
  //      assertTrue(node is TMXmlString, 'MS: Node '+inttostr(i)+' has the wrong type (expected string, found '+node.ClassName.substring(5));
  //      assertTrue(TMXmlString(node).value = outcome.attribute['value'], 'MS: Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+TMXmlString(node).value);
      end
      else if outcome.attribute['type'] = 'number' then
      begin
        raise ETestCase.create('not done yet');
  //      assertTrue(node is TMXmlNumber, 'MS: Node '+inttostr(i)+' has the wrong type (expected number, found '+node.ClassName.substring(5));
  //      assertTrue(TMXmlNumber(node).value = StrToInt(outcome.attribute['value']), 'MS: Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+inttostr(TMXmlNumber(node).value));
      end
      else if outcome.attribute['type'] = 'boolean' then
      begin
        raise ETestCase.create('not done yet');
  //      assertTrue(node is TMXmlBoolean, 'MS: Node '+inttostr(i)+' has the wrong type (expected boolean, found '+node.ClassName.substring(5));
  //      assertTrue(TMXmlBoolean(node).value = StringToBoolean(outcome.attribute['value']), 'MS: Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+BooleanToString(TMXmlBoolean(node).value));
      end
      else if outcome.attribute['type'] = 'attribute' then
      begin
        assertTrue(node.nodeType = NODE_ATTRIBUTE, 'MS: Node '+inttostr(i)+' has the wrong type (expected Attribute, found '+inttostr(node.nodeType));
        assertTrue(node.text = outcome.attribute['value'], 'MS: Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+node.text);
      end
      else if outcome.attribute['type'] = 'element' then
      begin
        assertTrue(node.nodeType = NODE_ELEMENT, 'MS: Node '+inttostr(i)+' has the wrong type (expected element, found '+inttostr(node.nodeType));
        assertTrue(node.baseName = outcome.attribute['name'], 'MS: Node '+inttostr(i)+' has the wrong name (expected '+outcome.attribute['name']+', found '+node.baseName);
        assertTrue(node.namespaceURI = outcome.attribute['namespace'], 'MS: Node '+inttostr(i)+' has the wrong namespace (expected '+outcome.attribute['namespace']+', found '+node.NamespaceURI);
      end
      else if outcome.attribute['type'] = 'text' then
      begin
        assertTrue(node.nodeType = NODE_TEXT, 'MS: Node '+inttostr(i)+' has the wrong type (expected text, found '+inttostr(node.nodeType));
        if outcome.HasAttribute['value'] then
          assertTrue(node.text = outcome.Attribute['value'], 'MS: Node '+inttostr(i)+' has the wrong type (expected text "'+outcome.Attribute['value']+'", found '+node.text);
      end
      else if outcome.attribute['type'] = 'comment' then
      begin
        raise ETestCase.create('not done yet');
  //      assertTrue((node is TMXmlElement) and (TMXmlElement(node).nodeType = ntComment), 'Node '+inttostr(i)+' has the wrong type (expected comment, found '+node.ClassName.substring(5));
  //
      end
      else
        raise ETestCase.create('Error Message');
    end;
  end;
end;

{$ENDIF}

function TXPathEngineTest.findTestCase(name: String): TMXmlElement;
var
  tcase : TMXmlElement;
begin
  tcase := tests.document.firstElement;
  while tcase <> nil do
  begin
    if tcase.Name = 'case' then
    begin
      if (tcase.attribute['name'] = Name) then
        exit(tcase);
    end;
    tcase := tcase.next;
  end;
  result := nil;
end;

procedure TXPathEngineTest.runXTest(test : TMXmlElement; outcomes : TFslList<TMXmlElement>);
var
  focus, outcome : TMXmlElement;
  nodes : TFslList<TMXmlNode>;
  node : TMXmlNode;
  i : integer;
begin
  focus := findSample(test.attribute['id']).firstElement;
  nodes := tests.select(test.element('xpath').attribute['value'], focus);
  try
  if test.element('outcomes').hasAttribute['count'] then
    assertTrue(StrToInt(test.element('outcomes').attribute['count']) = nodes.Count, 'Wrong number of nodes returned - expected '+test.element('outcomes').attribute['count']+', found '+inttostr(nodes.Count))
  else
  begin
    assertTrue(outcomes.Count = nodes.Count, 'Wrong number of nodes returned - expected '+inttostr(outcomes.Count)+', found '+inttostr(nodes.Count));
    for i := 0 to outcomes.Count - 1 do
    begin
      node := nodes[i];
      outcome := outcomes[i];
      if outcome.attribute['type'] = 'string' then
      begin
        assertTrue(node is TMXmlString, 'Node '+inttostr(i)+' has the wrong type (expected string, found '+node.ClassName.substring(5));
        assertTrue(TMXmlString(node).value = outcome.attribute['value'], 'Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+TMXmlString(node).value);
      end
      else if outcome.attribute['type'] = 'number' then
      begin
        assertTrue(node is TMXmlNumber, 'Node '+inttostr(i)+' has the wrong type (expected number, found '+node.ClassName.substring(5));
        assertTrue(TMXmlNumber(node).value = StrToInt(outcome.attribute['value']), 'Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+inttostr(TMXmlNumber(node).value));
      end
      else if outcome.attribute['type'] = 'boolean' then
      begin
        assertTrue(node is TMXmlBoolean, 'Node '+inttostr(i)+' has the wrong type (expected boolean, found '+node.ClassName.substring(5));
        assertTrue(TMXmlBoolean(node).value = StringToBoolean(outcome.attribute['value']), 'Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+BooleanToString(TMXmlBoolean(node).value));
      end
      else if outcome.attribute['type'] = 'attribute' then
      begin
        assertTrue(node is TMXmlAttribute, 'Node '+inttostr(i)+' has the wrong type (expected Attribute, found '+node.ClassName.substring(5));
        assertTrue(TMXmlAttribute(node).LocalName = outcome.attribute['name'], 'Node '+inttostr(i)+' has the wrong name (expected '+outcome.attribute['name']+', found '+TMXmlAttribute(node).LocalName);
        assertTrue(TMXmlAttribute(node).value = outcome.attribute['value'], 'Node '+inttostr(i)+' has the wrong value (expected '+outcome.attribute['value']+', found '+TMXmlAttribute(node).value);
      end
      else if outcome.attribute['type'] = 'element' then
      begin
        assertTrue((node is TMXmlElement) and (TMXmlElement(node).nodeType = ntElement), 'Node '+inttostr(i)+' has the wrong type (expected element, found '+node.ClassName.substring(5));
        assertTrue(TMXmlElement(node).LocalName = outcome.attribute['name'], 'Node '+inttostr(i)+' has the wrong name (expected '+outcome.attribute['name']+', found '+TMXmlElement(node).LocalName);
        assertTrue(TMXmlElement(node).NamespaceURI = outcome.attribute['namespace'], 'Node '+inttostr(i)+' has the wrong namespace (expected '+outcome.attribute['namespace']+', found '+TMXmlElement(node).NamespaceURI);
      end
      else if outcome.attribute['type'] = 'text' then
      begin
        assertTrue((node is TMXmlElement) and (TMXmlElement(node).nodeType = ntText), 'Node '+inttostr(i)+' has the wrong type (expected text, found '+node.ClassName.substring(5));

      end
      else if outcome.attribute['type'] = 'comment' then
      begin
        assertTrue((node is TMXmlElement) and (TMXmlElement(node).nodeType = ntComment), 'Node '+inttostr(i)+' has the wrong type (expected comment, found '+node.ClassName.substring(5));

      end
      else
        raise ETestCase.create('Error Message');
    end;
  end;
  finally
    nodes.Free;
  end;
end;

procedure TXPathEngineTest.TestCase(Name: String);
var
  test : TMXmlElement;
  outcomes : TFslList<TMXmlElement>;
begin
  test := findTestCase(name);
  if test = nil then
    assertFail('can''t find test')
  else
  begin
    outcomes := tests.selectElements('node', test.element('outcomes'));
    try
      {$IFNDEF FPC}
      runMsTest(test, outcomes);
      {$ENDIF}
      runXTest(test, outcomes);
    finally
      outcomes.Free;
    end;
  end;
end;

procedure TXPathEngineTest.setup;
begin
  tests := TMXmlParser.ParseFile(TestSettings.serverTestFile(['testcases', 'xml', 'xpath-tests.xml']), [xpResolveNamespaces]);
  tests.NamespaceAbbreviations.AddOrSetValue('f', 'http://hl7.org/fhir');
  tests.NamespaceAbbreviations.AddOrSetValue('h', 'http://www.w3.org/1999/xhtml');
  {$IFNDEF FPC}
  mstests := TMsXmlParser.Parse(TestSettings.serverTestFile(['testcases', 'xml', 'xpath-tests.xml']));
  mstests.setProperty('SelectionNamespaces','xmlns:f=''http://hl7.org/fhir'' xmlns:h=''http://www.w3.org/1999/xhtml''');
  {$ENDIF}
end;

procedure TXPathEngineTest.TearDown;
begin
  tests.Free;
end;

{ TDecimalTests }

procedure TDecimalTests.testString(s, st, std: String);
var
  dec : TFslDecimal;
  s1, s2 : String;
begin
  dec := TFslDecimal.valueOf(s);
  s1 := dec.AsString;
  s2 := dec.AsScientific;
  assertTrue(s1 = st);
  assertTrue(s2 = std);
  dec := TFslDecimal.valueOf(std);
  s1 := dec.AsDecimal;
  assertTrue(s1 = st);
end;

procedure TDecimalTests.TestStringSupport;
begin
  testString('1', '1', '1e0');
  testString('0', '0', '0e0');
  testString('10', '10', '1.0e1');
  testString('99', '99', '9.9e1');
  testString('-1', '-1', '-1e0');
  testString('-0', '0', '0e0');
  testString('-10', '-10', '-1.0e1');
  testString('-99', '-99', '-9.9e1');

  testString('1.1', '1.1', '1.1e0');
  testString('-1.1', '-1.1', '-1.1e0');
  testString('11.1', '11.1', '1.11e1');
  testString('1.11', '1.11', '1.11e0');
  testString('1.111', '1.111', '1.111e0');
  testString('0.1', '0.1', '1e-1');
  testString('00.1', '0.1', '1e-1');
  testString('.1', '0.1', '1e-1');
  testString('1.0', '1.0', '1.0e0');
  testString('1.00', '1.00', '1.00e0');
  testString('1.000000000000000000000000000000000000000', '1.000000000000000000000000000000000000000', '1.000000000000000000000000000000000000000e0');

  testString('-11.1', '-11.1', '-1.11e1');
  testString('-1.11', '-1.11', '-1.11e0');
  testString('-1.111', '-1.111', '-1.111e0');
  testString('-0.1', '-0.1', '-1e-1');
  testString('-00.1', '-0.1', '-1e-1');
  testString('-.1', '-0.1', '-1e-1');
  testString('-1.0', '-1.0', '-1.0e0');
  testString('-1.00', '-1.00', '-1.00e0');
  testString('-1.000000000000000000000000000000000000000', '-1.000000000000000000000000000000000000000', '-1.000000000000000000000000000000000000000e0');

  testString('0.0', '0.0', '0.0e0');
  testString('0.0000', '0.0000', '0.0000e0');
  testString('0.1', '0.1', '1e-1');
  testString('00.1', '0.1', '1e-1');
  testString('0.100', '0.100', '1.00e-1');
  testString('100', '100', '1.00e2');
  testString('1.0', '1.0', '1.0e0');
  testString('1.1', '1.1', '1.1e0');
  testString('-0.1', '-0.1', '-1e-1');
  testString('0.01', '0.01', '1e-2');
  testString('0.001', '0.001', '1e-3');
  testString('0.0001', '0.0001', '1e-4');
  testString('00.0001', '0.0001', '1e-4');
  testString('000.0001', '0.0001', '1e-4');
  testString('-0.01', '-0.01', '-1e-2');
  testString('10.01', '10.01', '1.001e1');
  testString('0.0001', '0.0001', '1e-4');
  testString('0.00001', '0.00001', '1e-5');
  testString('0.000001', '0.000001', '1e-6');
  testString('0.0000001', '0.0000001', '1e-7');
  testString('0.000000001', '0.000000001', '1e-9');
  testString('0.00000000001', '0.00000000001', '1e-11');
  testString('0.0000000000001', '0.0000000000001', '1e-13');
  testString('0.000000000000001', '0.000000000000001', '1e-15');
  testString('0.00000000000000001', '0.00000000000000001', '1e-17');
  testString('10.1', '10.1', '1.01e1');
  testString('100.1', '100.1', '1.001e2');
  testString('1000.1', '1000.1', '1.0001e3');
  testString('10000.1', '10000.1', '1.00001e4');
  testString('100000.1', '100000.1', '1.000001e5');
  testString('1000000.1', '1000000.1', '1.0000001e6');
  testString('10000000.1', '10000000.1', '1.00000001e7');
  testString('100000000.1', '100000000.1', '1.000000001e8');
  testString('1000000000.1', '1000000000.1', '1.0000000001e9');
  testString('10000000000.1', '10000000000.1', '1.00000000001e10');
  testString('100000000000.1', '100000000000.1', '1.000000000001e11');
  testString('1000000000000.1', '1000000000000.1', '1.0000000000001e12');
  testString('10000000000000.1', '10000000000000.1', '1.00000000000001e13');
  testString('100000000000000.1', '100000000000000.1', '1.000000000000001e14');
//  testString('1e-3', '1e-3');   , '1e-3');  e0  }
end;

procedure TDecimalTests.TestAddition;
begin
  TestAdd('1', '1', '2');
  TestAdd('0', '1', '1');
  TestAdd('0', '0', '0');
  TestAdd('5', '5', '10');
  TestAdd('10', '1', '11');
  TestAdd('11', '12', '23');
  TestAdd('15', '16', '31');
  TestAdd('150', '160', '310');
  TestAdd('153', '168', '321');
  TestAdd('15300000000000000000000000000000000001', '1680', '15300000000000000000000000000000001681');
  TestAdd('1', '.1', '1.1');
  TestAdd('1', '.001', '1.001');
  TestAdd('.1', '.1', '0.2');
  TestAdd('.1', '.01', '0.11');

  TestSubtract('2', '1', '1');
  TestSubtract('2', '0', '2');
  TestSubtract('0', '0', '0');
  TestSubtract('0', '2', '-2');
  TestSubtract('2', '2', '0');
  TestSubtract('1', '2', '-1');
  TestSubtract('20', '1', '19');
  TestSubtract('2', '.1', '1.9');
  TestSubtract('2', '.000001', '1.999999');
  TestSubtract('2', '2.000001', '-0.000001');
  TestSubtract('3.5', '35.5', '-32.0');

  TestAdd('5', '6', '11');
  TestAdd('5', '-6', '-1');
  TestAdd('-5', '6', '1');
  TestAdd('-5', '-6', '-11');

  TestSubtract('5', '6', '-1');
  TestSubtract('6', '5', '1');
  TestSubtract('5', '-6', '11');
  TestSubtract('6', '-5', '11');
  TestSubtract('-5', '6', '-11');
  TestSubtract('-6', '5', '-11');
  TestSubtract('-5', '-6', '1');
  TestSubtract('-6', '-5', '-1');

  TestAdd('2', '0.001', '2.001');
  TestAdd('2.0', '0.001', '2.001');
end;

procedure TDecimalTests.TestAdd(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.add(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestSubtract(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.Subtract(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.testTrunc;
begin
  testTruncation('1', 0, '1', false);
  testTruncation('1.01', 0, '1', false);
  testTruncation('-1.01', 0, '-1', false);
  testTruncation('0.01', 0, '0', false);
  testTruncation('-0.01', 0, '0', false);
  testTruncation('0.1', 0, '0', false);
  testTruncation('0.0001', 0, '0', false);
  testTruncation('100.000000000000000000000000000000000000000001', 0, '100', false);

  TestTruncation('1.2345678', 0, '1', true);
  TestTruncation('1.2345678', 1, '1.2', true);
  TestTruncation('1.2345678', 2, '1.23', true);
  TestTruncation('1.2345678', 3, '1.234', true);
  TestTruncation('1.2345678', 6, '1.234567', true);
  TestTruncation('1.2345678', 10, '1.2345678', true);
//  TestTruncation('1.2345678', 0, '1', false);
//  TestTruncation('1.2345678', 1, '1.2', false);
//  TestTruncation('1.2345678', 2, '1.23', false);
//  TestTruncation('1.2345678', 3, '1.234', false);
//  TestTruncation('1.2345678', 6, '1.234568', false);
//  TestTruncation('1.2345678', 10, '1.2345678', false);
end;

procedure TDecimalTests.TestTruncation(value: String; digits: integer; outcome: String; round: boolean);
var
  o1, o2 : TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(value);
  o2 := o1.Trunc(digits);
  assertTrue(o2.AsDecimal = outcome);
end;

procedure TDecimalTests.TestMultiplication;
begin
  TestMultiply('2', '2', '4');
  TestMultiply('2', '0.5', '1');
  TestMultiply('0', '0', '0');
  TestMultiply('0', '1', '0');
  TestMultiply('4', '4', '16');
  TestMultiply('20', '20', '400');
  TestMultiply('200', '20', '4000');
  TestMultiply('400', '400', '160000');
  TestMultiply('2.0', '2.0', '4.0');
  TestMultiply('2.00', '2.0', '4.0');
  TestMultiply('2.0', '0.2', '0.4');
  TestMultiply('2.0', '0.20', '0.40');
  TestMultiply('13', '13', '169');
  TestMultiply('12', '89', '1068');
  TestMultiply('1234', '6789', '8377626');

  TestMultiply('10000', '0.0001', '1');
  TestMultiply('10000', '0.00010', '1.0');
  TestMultiply('10000', '0.000100', '1.00');
  TestMultiply('10000', '0.0001000', '1.000');
  TestMultiply('10000', '0.00010000', '1.0000');
  TestMultiply('10000', '0.000100000', '1.00000');
  TestMultiply('10000.0', '0.000100000', '1.00000');
  TestMultiply('10000.0', '0.0001000000', '1.00000');
  TestMultiply('10000.0', '0.00010000000', '1.00000');

  TestMultiply('2', '-2', '-4');
  TestMultiply('-2', '2', '-4');
  TestMultiply('-2', '-2', '4');

  TestMultiply('35328734682734', '2349834295876423', '83016672387407213199375780482');
  TestMultiply('35328734682734000000000', '2349834295876423000000000', '83016672387407213199375780482000000000000000000');
  TestMultiply('3532873468.2734', '23498342958.76423', '83016672387407213199.375780482');

  TestDivide('500', '4', '125');
  TestDivide('1260257', '37', '34061');

  TestDivide('127', '4', '31.75');
  TestDivide('10', '10', '1');
  TestDivide('1', '1', '1');
  TestDivide('1', '3', '0.333333333333333333333333');
  TestDivide('1.0', '3', '0.33');
  TestDivide('10', '3', '3.33333333333333333333333');
  TestDivide('10.0', '3', '3.33');
  TestDivide('10.00', '3', '3.333');
  TestDivide('10.00', '3.0', '3.3');
  TestDivide('100', '1', '100');
  TestDivide('1000', '10', '100');
  TestDivide('100001', '10', '10000.1');
  TestDivide('100', '10', '10');
  TestDivide('1', '10', '0.1');
  TestDivide('1', '15', '0.0666666666666666666666667');
  TestDivide('1.0', '15', '0.067');
  TestDivide('1.00', '15.0', '0.0667');
  TestDivide('1', '0.1', '10');
  TestDivide('1', '0.10', '10');
  TestDivide('1', '0.010', '100');
  TestDivide('1', '1.5', '0.67');
  TestDivide('1.0', '1.5', '0.67');
  TestDivide('10', '1.5', '6.7');

  TestDivide('-1', '1', '-1');
  TestDivide('1', '-1', '-1');
  TestDivide('-1', '-1', '1');

  TestDivide('2', '2', '1');
  TestDivide('20', '2', '10');
  TestDivide('22', '2', '11');

  TestDivide('83016672387407213199375780482', '2349834295876423', '35328734682734');
  TestDivide('83016672387407213199375780482000000000000000000', '2349834295876423000000000', '35328734682734000000000');
  TestDivide('83016672387407213199.375780482', '23498342958.76423', '3532873468.2734');

  TestDivInt('500', '4', '125');
  TestDivInt('1260257', '37', '34061');
  TestDivInt('127', '4', '31');
  TestDivInt('10', '10', '1');
  TestDivInt('1', '1', '1');
  TestDivInt('100', '1', '100');
  TestDivInt('1000', '10', '100');
  TestDivInt('100001', '10', '10000');
  TestDivInt('1', '1.5', '0');
  TestDivInt('10', '1.5', '6');

  TestModulo('10', '1', '0');
  TestModulo('7', '4', '3');

  TestMultiply('2', '2', '4');
  TestMultiply('2.0', '2.0', '4.0');
  TestMultiply('2.00', '2.0', '4.0');

  TestDivide('10.0',  '3', '3.33');
  TestDivide('10.00',  '3', '3.333');
  TestDivide('10.00',  '3.0', '3.3');
  TestDivide('10',  '3.0', '3.3');

  TestRoundTrip('1','60', '60', '1');
end;

procedure TDecimalTests.TestMultiply(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.Multiply(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestRoundTrip(n1, n2, n3, t: String);
var
  o1, o2, o3, o4: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(n1);
  o2 := TFslDecimal.valueOf(n2);
  o3 := o1.Divide(o2);
  o4 := o3.Multiply(TFslDecimal.valueOf(n3));
  assertTrue(o4.AsDecimal = t);
end;

procedure TDecimalTests.TestDivide(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.Divide(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestDivInt(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.DivInt(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestModulo(s1, s2, s3: String);
var
  o1, o2, o3: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(s1);
  o2 := TFslDecimal.valueOf(s2);
  o3 := o1.Modulo(o2);
  assertTrue(o3.AsDecimal = s3);
end;

procedure TDecimalTests.TestAsInteger;
begin
  TestInteger(0);
  TestInteger(1);
  TestInteger(2);
  TestInteger(64);
  TestInteger(High(Integer));
  TestInteger(-1);
  TestInteger(-2);
  TestInteger(-64);
  TestInteger(Low(Integer));

  TestCardinal(0);
  TestCardinal(2);
  TestCardinal(High(Cardinal));

  TestInt64(0);
  TestInt64(1);
  testInt64(-1);
  TestInt64(High(integer));
  TestInt64(Low(integer));
  TestInt64(High(Cardinal));
  TestInt64(High(int64));
  TestInt64(Low(int64));
end;

procedure TDecimalTests.TestBounds;
begin
  TestBoundsCase('1',      '0.5',   '1.5',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('1.0',   '0.95',  '1.05',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('1.00', '0.995', '1.005',  '0.999999999999999999999999',   '1.00000000000000000000001');
  TestBoundsCase('0',     '-0.5',   '0.5', '-0.000000000000000000000001',   '0.000000000000000000000001');
  TestBoundsCase('0.0',  '-0.05',  '0.05', '-0.000000000000000000000001',   '0.000000000000000000000001');
  TestBoundsCase('-1',    '-1.5',  '-0.5', '-1.000000000000000000000001',  '-0.99999999999999999999999');
end;

procedure TDecimalTests.TestBoundsCase(v, low, high, ilow, ihigh : String);
var
  o1: TFslDecimal;
begin
  o1 := TFslDecimal.valueOf(v);
  assertTrue(o1.upperBound.AsDecimal = high);
  assertTrue(o1.lowerBound.AsDecimal = low);
//    check(o1.immediateUpperBound.AsDecimal = ihigh);
//    check(o1.immediateLowerBound.AsDecimal = ilow);
end;

function n(s : String; defUp : boolean) : String;
begin
  result := TFslDecimal.valueOf(s).normaliseDecimal(5, 5, defUp);
end;
const up = true; dn = false;

procedure TDecimalTests.TestNormalisedDecimal;
begin
  // simple numbers
  assertTrue(n('0',         up) = '000000.00000');
  assertTrue(n('0',         dn) = '000000.00000');
  assertTrue(n('-0',        up) = '000000.00000');
  assertTrue(n('-0',        dn) = '000000.00000');
  assertTrue(n('1',         up) = '000001.00000');
  assertTrue(n('1',         dn) = '000001.00000');
  assertTrue(n('0.1',       up) = '000000.10000');
  assertTrue(n('0.1',       dn) = '000000.10000');
  assertTrue(n('-1',        up) = '!99999.00000');
  assertTrue(n('-1',        dn) = '!99999.00000');
  assertTrue(n('-0.1',      up) = '!99999.90000');
  assertTrue(n('-0.1',      dn) = '!99999.90000');

  // limits
  assertTrue(n('99999',     up) = '099999.00000');
  assertTrue(n('99999',     dn) = '099999.00000');
  assertTrue(n('-99999',    up) = '!00001.00000');
  assertTrue(n('-99999',    dn) = '!00001.00000');
  assertTrue(n('0.00001',   up) = '000000.00001');
  assertTrue(n('0.00001',   dn) = '000000.00001');
  assertTrue(n('-0.00001',  up) = '!99999.99999');
  assertTrue(n('-0.00001',  dn) = '!99999.99999');

  // past the limit +large
  assertTrue(n('100000',    up) = '0XXXXX.XXXXX');
  assertTrue(n('100000',    dn) = '099999.99999');

  // past the limit -large
  assertTrue(n('-100001',   up) = '!00000.00000');
  assertTrue(n('-100001',   dn) = '!#####.#####');

  // past the limit +small
  assertTrue(n('0.000001',  up) = '000000.00001');
  assertTrue(n('0.000001',  dn) = '000000.00000');

  // past the limit -small
  assertTrue(n('-0.000001', up) = '000000.00000');
  assertTrue(n('-0.000001', dn) = '!99999.99999');

  // now, check order:
  assertTrue(n('1000000', true) > n('1000', true));
  assertTrue(n('10000', true) > n('1', true));
  assertTrue(n('1', true) > n('0.1', true));
  assertTrue(n('1', true) > n('-1', true));
  assertTrue(n('-1', true) > n('-10000', true));
  assertTrue(n('-10000', true) > n('-1000000', true));
end;

procedure TDecimalTests.TestOverloading;
begin
  assertTrue(TFslDecimal('1') + TFslDecimal(2) = TFslDecimal('3'));
end;

procedure TDecimalTests.TestInteger(i: integer);
var
  d : TFslDecimal;
begin
  d := TFslDecimal.valueOf(i);
  assertTrue(d.AsInteger = i);
end;

procedure TDecimalTests.TestIsDecimal;
begin
  assertTrue(StringIsDecimal('0'), '"0" is a decimal');
  assertTrue(StringIsDecimal('+0'), '"+0" is a decimal');
  assertFalse(StringIsDecimal('0+'), '"0+" is not a decimal');
  assertFalse(StringIsDecimal('+'), '"+" is not a decimal');
  assertTrue(StringIsDecimal('-0'), '"-0" is a decimal');
  assertFalse(StringIsDecimal('0-'), '"0-" is not a decimal');
  assertFalse(StringIsDecimal('-'), '"-" is not a decimal');
  assertTrue(StringIsDecimal('0e0'), '"0e0" is a decimal');
  assertTrue(StringIsDecimal('+0e+0'), '"+0e+0" is a decimal');
  assertTrue(StringIsDecimal('-0e-0'), '"-0e-0" is a decimal');
  assertFalse(StringIsDecimal('0e'), '"0e" is not a decimal');
  assertFalse(StringIsDecimal('e0'), '"e0" is not a decimal');
  assertTrue(StringIsDecimal('1.2'), '"1.2" is a decimal');
  assertTrue(StringIsDecimal('-1.2'), '"-1.2" is a decimal');
  assertTrue(StringIsDecimal('+1.2'), '"+1.2" is a decimal');
  assertFalse(StringIsDecimal('1. 2'), '"1. 2" is not a decimal');
  assertFalse(StringIsDecimal('1 .2'), '"1 .2" is not a decimal');
  assertFalse(StringIsDecimal(' 1.2'), '" 1.2" is not a decimal');
  assertFalse(StringIsDecimal('1.2 '), '"1.2 " is not a decimal');
  assertTrue(StringIsDecimal('1.2e2'), '"1.2e2" is a decimal');
  assertTrue(StringIsDecimal('1.2e-2'), '"1.2e2" is a decimal');
  assertTrue(StringIsDecimal('1.2e+2'), '"1.2e2" is a decimal');
  assertFalse(StringIsDecimal('1.2e2e3'), '"1.2e2e3" is not a decimal');
end;

procedure TDecimalTests.TestCardinal(i: cardinal);
var
  i64 : int64;
  d : TFslDecimal;
begin
  i64 := i;
  d := TFslDecimal.valueOf(i64);
  assertTrue(d.AsCardinal = i);
  //check(d.AsInteger = i);
end;

procedure TDecimalTests.TestInfinity;
begin
  assertTrue(TFslDecimal.makeInfinity.IsInfinite);
  assertTrue(TFslDecimal.makeInfinity.Negated.IsNegative);
  assertFalse(TFslDecimal.makeUndefined.IsInfinite);
  assertFalse(TFslDecimal.makeInfinity.IsUndefined);
  assertFalse(TFslDecimal.makeNull.IsUndefined);
  assertFalse(TFslDecimal.makeNull.IsInfinite);
  assertFalse(TFslDecimal.makeNull.isANumber);
  assertTrue(TFslDecimal.makeInfinity.Equals(TFslDecimal.makeInfinity));

  assertTrue(TFslDecimal.ValueOf('Inf').IsInfinite);
  assertTrue(TFslDecimal.ValueOf('-Inf').IsInfinite);
  assertTrue(not TFslDecimal.ValueOf('Inf').IsNegative);
  assertTrue(TFslDecimal.ValueOf('-Inf').IsNegative);

  assertTrue(n('Inf',    up) = '0XXXXX.XXXXX');
  assertTrue(n('Inf',    dn) = '099999.99999');
  assertTrue(n('+Inf',    up) = '0XXXXX.XXXXX');

  assertTrue(n('-Inf',   up) = '!00000.00000');
  assertTrue(n('-Inf',   dn) = '!#####.#####');

end;

procedure TDecimalTests.TestInt64(i: int64);
var
  d : TFslDecimal;
begin
  d := TFslDecimal.valueOf(i);
  assertTrue(d.AsInt64 = i);
end;

{ TTurtleTests }

procedure TTurtleTests.parseTtl(filename: String; ok: boolean);
var
  s : String;
  ttl : TTurtleDocument;
begin
  s := fileToString(TestSettings.fhirTestFile(['turtle', filename]), TEncoding.UTF8);
  try
    ttl := TTurtleParser.parse(s);
    try
      assertTrue(ttl <> nil);
      assertTrue(ok);
    finally
      ttl.Free;
    end;
  except
    on e : Exception do
    begin
      assertTrue(not ok, 'Unexpected Exception: '+e.message);
    end;
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

var
  globalNum : cardinal;
  globalThread : TThreadID;
  cs : TRTLCriticalSection;
  kcs : TFslLock;
  sem : TSemaphore;

Const
  TEST_FILE_CONTENT : AnsiString = 'this is some test content'+#13#10;

procedure TXPlatformTests.test60sec;
begin
  TFslDateTime.make(EncodeDate(2013, 4, 5) + EncodeTime(12, 34, 60, 0), dttzUnknown).toHL7
end;

procedure TXPlatformTests.TestFslFile;
var
  filename : String;
  f : TFslFile;
  s : AnsiString;
begin
  filename := Path([SystemTemp, 'delphi.file.test.txt']);
  if FileExists(filename) then
  begin
    FileSetReadOnly(filename, false);
    FileDelete(filename);
  end;
  assertFalse(FileExists(filename));
  f := TFslFile.Create(filename, fmCreate);
  try
    f.Write(TEST_FILE_CONTENT[1], length(TEST_FILE_CONTENT));
  finally
    f.Free;
  end;
  assertTrue(FileExists(filename));
  assertTrue(FileSize(filename) = 27);
  f := TFslFile.Create(filename, fmOpenRead);
  try
    SetLength(s, f.Size);
    f.Read(s[1], f.Size);
    assertTrue(s = TEST_FILE_CONTENT);
  finally
    f.Free;
  end;
  FileSetReadOnly(filename, true);
  FileDelete(filename);
  assertTrue(FileExists(filename));
  FileSetReadOnly(filename, false);
  FileDelete(filename);
  assertFalse(FileExists(filename));
end;

procedure TXPlatformTests.TesTFslObject;
var
  obj : TFslObject;
begin
  obj := TFslObject.Create;
  try
    assertTrue(obj.FslObjectReferenceCount = 0);
    obj.Link;
    assertTrue(obj.FslObjectReferenceCount = 1);
    obj.Free;
    assertTrue(obj.FslObjectReferenceCount = 0);
  finally
    obj.Free;
  end;
end;

procedure TXPlatformTests.TestCriticalSectionSimple;
begin
  InitializeCriticalSection(cs);
  try
    EnterCriticalSection(cs);
    try
      assertTrue(true);
    finally
      LeaveCriticalSection(cs);
    end;
  finally
    DeleteCriticalSection(cs);
  end;
end;

procedure TXPlatformTests.TestKCriticalSectionSimple;
begin
  kcs := TFslLock.Create('test');
  try
    kcs.Enter;
    try
      assertTrue(true);
    finally
      kcs.Leave;
    end;
  finally
    kcs.Free;
  end;
end;

type
  TTestCriticalSectionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TTestKCriticalSectionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TTestSemaphoreThread = class(TThread)
  protected
    procedure Execute; override;
  end;

procedure TXPlatformTests.TestCriticalSectionThreaded;
begin
  globalThread := GetCurrentThreadId;
  InitializeCriticalSection(cs);
  try
    EnterCriticalSection(cs);
    try
      TTestCriticalSectionThread.create(false);
      Sleep(10);
      assertTrue(globalThread = GetCurrentThreadId);
    finally
      LeaveCriticalSection(cs);
    end;
    sleep(10);
    EnterCriticalSection(cs);
    try
      assertTrue(globalThread <> GetCurrentThreadId);
    finally
      LeaveCriticalSection(cs);
    end;
  finally
    DeleteCriticalSection(cs);
  end;
end;

procedure TXPlatformTests.TestKCriticalSectionThreaded;
begin
  globalThread := GetCurrentThreadId;
  kcs := TFslLock.Create('none');
  try
    kcs.Enter;
    try
      TTestKCriticalSectionThread.create(false);
      Sleep(10);
      assertTrue(globalThread = GetCurrentThreadId);
    finally
      kcs.Leave;
    end;
    sleep(10);
    kcs.Enter;
    try
      assertTrue(globalThread <> GetCurrentThreadId);
    finally
      kcs.Leave;
    end;
  finally
    kcs.free;
  end;
end;

procedure TXPlatformTests.TestRemoveAccents;
begin
  assertEqual('Grahame Grieve', RemoveAccents('Grahame Grieve'));
  assertEqual('aaeeiiooouuu AAEEIIOOOUUU', RemoveAccents('aáeéiíoóöuúü AÁEÉIÍOÓÖUÚÜ'));
  assertEqual('Ваnерии Никоnаевич СЕРГЕЕВ', RemoveAccents('Валерий Николаевич СЕРГЕЕВ'));
end;

procedure TXPlatformTests.TestTemp;
begin
  assertTrue(SystemTemp <> '');
end;

procedure TXPlatformTests.TestTimeZoneName;
begin
  assertTrue(TimeZoneIANAName <> '');
end;

procedure TXPlatformTests.TestFslDateTime;
var
  d1, d2 : TFslDateTime;
  dt1, dt2 : Double;
begin
  // null
  assertTrue(d1.null);
  assertFalse(d1.notNull);
  d1 := TFslDateTime.makeToday;
  assertTrue(d1.notNull);
  assertFalse(d1.null);
  d1 := TFslDateTime.makeNull;
  assertTrue(d1.null);
  assertFalse(d1.notNull);

  // format support
  assertTrue(TFslDateTime.fromXML('2013-04-05T12:34:56').toHL7 = '20130405123456');
  assertTrue(TFslDateTime.fromXML('2013-04-05T12:34:56Z').toHL7 = '20130405123456Z');
  assertTrue(TFslDateTime.fromXML('2013-04-05T12:34:56+10:00').toHL7 = '20130405123456+1000');
  assertTrue(TFslDateTime.fromXML('2013-04-05T12:34:56-10:00').toHL7 = '20130405123456-1000');
  assertTrue(TFslDateTime.fromXML('2013-04-05').toHL7 = '20130405');
  assertTrue(TFslDateTime.fromHL7('20130405123456-1000').toXML = '2013-04-05T12:34:56-10:00');

  // Date Time conversion
  assertTrue(TFslDateTime.make(EncodeDate(2013, 4, 5) + EncodeTime(12, 34,56, 0), dttzUnknown).toHL7 = '20130405123456.000');
  assertWillRaise(test60Sec, EConvertError, '');
  dt1 := EncodeDate(2013, 4, 5) + EncodeTime(12, 34,56, 0);
  dt2 := TFslDateTime.fromHL7('20130405123456').DateTime;
  assertTrue(dt1 = dt2);

  // comparison
  d1 := TFslDateTime.make(EncodeDate(2011, 2, 2)+ EncodeTime(14, 0, 0, 0), dttzLocal);
  d2 := TFslDateTime.make(EncodeDate(2011, 2, 2)+ EncodeTime(15, 0, 0, 0), dttzLocal);
  assertTrue(d2.after(d1, false));
  assertFalse(d1.after(d1, false));
  assertTrue(d1.after(d1, true));
  assertFalse(d2.before(d1, false));
  assertFalse(d1.before(d1, false));
  assertTrue(d1.before(d1, true));
  assertFalse(d1.after(d2, false));
  assertTrue(d1.before(d2, false));
  assertTrue(d1.compare(d2) = -1);
  assertTrue(d2.compare(d1) = 1);
  assertTrue(d1.compare(d1) = 0);

  // Timezone Wrangling
  d1 := TFslDateTime.make(EncodeDate(2011, 2, 2)+ EncodeTime(14, 0, 0, 0), dttzLocal); // during daylight savings (+11)
  d2 := TFslDateTime.make(EncodeDate(2011, 2, 2)+ EncodeTime(3, 0, 0, 0), dttzUTC); // UTC Time
  assertTrue(sameInstant(d1.DateTime - TimezoneBias(EncodeDate(2011, 2, 2)), d2.DateTime));
  assertTrue(sameInstant(d1.UTC.DateTime, d2.DateTime));
  assertTrue(not d1.equal(d2));
  assertTrue(d1.sameTime(d2));
  d1 := TFslDateTime.make(EncodeDate(2011, 7, 2)+ EncodeTime(14, 0, 0, 0), dttzLocal); // not during daylight savings (+10)
  d2 := TFslDateTime.make(EncodeDate(2011, 7, 2)+ EncodeTime(4, 0, 0, 0), dttzUTC); // UTC Time
  dt1 := d1.DateTime - TimezoneBias(EncodeDate(2011, 7, 2));
  dt2 := d2.DateTime;
  assertTrue(sameInstant(dt1, dt2));
  assertTrue(sameInstant(d1.UTC.DateTime, d2.DateTime));
  assertTrue(not d1.equal(d2));
  assertTrue(d1.sameTime(d2));
  assertTrue(TFslDateTime.fromHL7('20130405120000+1000').sameTime(TFslDateTime.fromHL7('20130405100000+0800')));
  assertTrue(TFslDateTime.fromXML('2017-11-05T05:30:00.0Z').sameTime(TFslDateTime.fromXML('2017-11-05T05:30:00.0Z')));
  assertTrue(TFslDateTime.fromXML('2017-11-05T09:30:00.0+04:00').sameTime(TFslDateTime.fromXML('2017-11-05T05:30:00.0Z')));
  assertTrue(TFslDateTime.fromXML('2017-11-05T01:30:00.0-04:00').sameTime(TFslDateTime.fromXML('2017-11-05T05:30:00.0Z')));
  assertTrue(TFslDateTime.fromXML('2017-11-05T09:30:00.0+04:00').sameTime(TFslDateTime.fromXML('2017-11-05T01:30:00.0-04:00')));

  // Min/Max
  assertTrue(TFslDateTime.fromHL7('20130405123456').Min.toHL7 = '20130405123456.000');
  assertTrue(TFslDateTime.fromHL7('20130405123456').Max.toHL7 = '20130405123457.000');
  assertTrue(TFslDateTime.fromHL7('201304051234').Min.toHL7 = '20130405123400.000');
  assertTrue(TFslDateTime.fromHL7('201304051234').Max.toHL7 = '20130405123500.000');

  assertTrue(TFslDateTime.fromHL7('201301010000').before(TFslDateTime.fromHL7('201301010000'), true));
  assertTrue(not TFslDateTime.fromHL7('201301010000').before(TFslDateTime.fromHL7('201301010000'), false));
  assertTrue(TFslDateTime.fromHL7('201301010000').before(TFslDateTime.fromHL7('201301010001'), true));
  assertTrue(not TFslDateTime.fromHL7('201301010001').before(TFslDateTime.fromHL7('201301010000'), true));
  //
//  d1 := UniversalDateTime;
//  d2 := LocalDateTime;
//  d3 := TimeZoneBias;
//  assertTrue(d1 <> d2);
//  assertTrue(d1 = d2 - d3);
end;

{ TTestCriticalSectionThread }

procedure TTestCriticalSectionThread.execute;
begin
  EnterCriticalSection(cs);
  try
    globalThread := GetCurrentThreadId;
  finally
    LeaveCriticalSection(cs);
  end;
end;

procedure TXPlatformTests.TestSemaphore;
var
  thread : TTestSemaphoreThread;
begin
  globalNum := 0;
  sem := TSemaphore.Create(nil, 0, 1, '');
  try
    thread := TTestSemaphoreThread.Create(false);
    try
      thread.FreeOnTerminate := true;
      while (globalNum = 0) do
        sleep(100);
      assertTrue(globalNum = 1, '1');
      sem.Release;
      sleep(500);
      assertTrue(globalNum = 2, '2');
      sem.Release;
      sleep(500);
      assertTrue(globalNum = 3, '3');
      sleep(500);
      assertTrue(globalNum = 3, '4');
    finally
      thread.Terminate;
      sem.release;
    end;
    sleep(500);
    assertTrue(globalNum = 100, '100');
  finally
    sem.Free;
  end;
end;

{ TTestSemaphoreThread }

procedure TTestSemaphoreThread.execute;
begin
  inc(globalNum);
  while not Terminated do
  begin
    case sem.WaitFor(10000) of
      wrSignaled:
        begin
          inc(globalNum);
        end;
      wrTimeout:
        begin
          raise exception.create('timeout');
        end;
      wrAbandoned :
        begin
          raise exception.create('abandoned');
        end;
      wrError :
        begin
          raise exception.create('error');
        end;
    end;
  end;
  globalNum := 100;
end;

{ TTestKCriticalSectionThread }

procedure TTestKCriticalSectionThread.Execute;
begin
  kcs.Enter;
  try
    globalThread := GetCurrentThreadId;
  finally
    kcs.Leave;
  end;
end;

{ TJsonTests }

procedure TJsonTests.jsonUnicodeTest(fn: String);
var
  json : TJsonObject;
  fnSrc, fnDst, s : string;
  cnt : TBytes;
  ok : boolean;
begin
  fnSrc := TestSettings.serverTestFile(['testcases', 'json', fn]);
  fnDst := MakeTempFilename();
  json := TJSONParser.ParseFile(fnSrc);
  try
    assertTrue(json <> nil);
    cnt := TJSONWriter.writeObject(json, true);
    bytesToFile(cnt, fnDst);
    ok := CheckJsonIsSame(fnSrc, fnDst, s);
    assertTrue(ok, s);
  finally
    json.Free;
  end;
end;

procedure TJsonTests.TestCustomDecimal;
var
  json : TJsonObject;
  f : TFileStream;
begin
  f := TFileStream.Create(TestSettings.fhirTestFile(['r4', 'examples', 'observation-decimal.json']), fmopenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f);
    try
      assertTrue(json <> nil);
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TJsonTests.jsonTest(src : String; clss : TJsonNodeClass);
var
  n : TJsonNode;
begin
  n := TJsonParser.ParseNode(src);
  try
    assertTrue(n is clss);
  finally
    n.free;
  end;
end;

procedure TJsonTests.testSimple;
begin
  jsonTest('{"t" : "t"}', TJsonObject);
  jsonTest('{"t" : 1}', TJsonObject);
  jsonTest('{"t" : true}', TJsonObject);
  jsonTest('{"t" : null}', TJsonObject);
  jsonTest('{"t" : ["t"]}', TJsonObject);
  jsonTest('{}', TJsonObject);
  jsonTest('[]', TJsonArray);
  jsonTest('[{}]', TJsonArray);
  jsonTest('[{"t" : []}]', TJsonArray);
  jsonTest('["", 1, null, true, {"t" : []}]', TJsonArray);
end;

procedure TJsonTests.TestCustomDoc2;
var
  json : TJsonObject;
  f : TFileStream;
begin
  f := TFileStream.Create(TestSettings.serverTestFile(['testcases', 'json', 'test.json']), fmopenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f);
    try
      assertTrue(json <> nil);
      assertTrue(json.properties.Count = 3);
      assertTrue(json.str['type'] = 'FHIR Custom Resource Directory');
      assertTrue(json.arr['prefixes'].Count = 1);
      assertTrue(json.arr['names'].Count = 1);
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TJsonTests.TestCustomDoc2Loose;
var
  json : TJsonObject;
  f : TFileStream;
begin
  f := TFileStream.Create(TestSettings.serverTestFile(['testcases', 'json', 'test-loose.json']), fmopenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f, 0, true);
    try
      assertTrue(json <> nil);
      assertTrue(json.properties.Count = 3);
      assertTrue(json.str['type'] = 'FHIR Custom Resource Directory');
      assertTrue(json.arr['prefixes'].Count = 1);
      assertTrue(json.arr['names'].Count = 1);
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TJsonTests.TestResource;
var
  json : TJsonObject;
  f : TFileStream;
begin
  f := TFileStream.Create(TestSettings.fhirTestFile(['r4', 'examples', 'account-example.json']), fmopenRead + fmShareDenyWrite);
  try
    json := TJSONParser.Parse(f);
    try
      assertTrue(json <> nil);
    finally
      json.Free;
    end;
  finally
    f.Free;
  end;
end;

procedure TJsonTests.testUtf16;
begin
  jsonUnicodeTest('json-utf16.json');
end;

procedure TJsonTests.testUtf8;
begin
  jsonUnicodeTest('json-utf8.json');
end;

procedure TJsonTests.testUtf8n;
begin
  jsonUnicodeTest('json-utf8n.json');
end;

constructor TJsonPatchTests.Create;
var
  tests : TJsonArray;
  test : TJsonNode;
  s : string;
begin
  inherited create;
  tests := TJSONParser.ParseNode(FileToBytes(TestSettings.serverTestFile(['testcases', 'json', 'json-patch-tests.json']))) as TJsonArray;
  try
    for test in tests do
    begin
      s := (test as TJsonObject)['comment'];
      s := s.Substring(0, s.IndexOf(' '));
      AddTest(TJsonPatchTest.create(s));
    end;
  finally
    tests.free;
  end;
end;

{ TJsonPatchTest }

procedure TJsonPatchTest.execute;
begin
  engine.applyPatch(test.obj['doc'], test.arr['patch']).Free;
end;

procedure TJsonPatchTest.PatchTest(Name: String);
var
  t : TJsonNode;
  outcome : TJsonObject;
  s : String;
begin
  for t in tests do
  begin
    test := t as TJsonObject;
    s := test['comment'];
    if s.StartsWith(Name) then
    begin
      if test.has('error') then
      begin
        assertWillRaise(execute, EJsonException, '');
      end
      else
      begin
        outcome := engine.applyPatch(test.obj['doc'], test.arr['patch']);
        try
          assertTrue(TJsonNode.compare(outcome, test.obj['expected']))
        finally
          outcome.Free;
        end;
      end;
    end;
  end;
end;

procedure TJsonPatchTest.setup;
begin
  tests := TJSONParser.ParseNode(FileToBytes(TestSettings.serverTestFile(['testcases', 'json', 'json-patch-tests.json']))) as TJsonArray;
  engine := TJsonPatchEngine.Create;
end;

procedure TJsonPatchTest.TearDown;
begin
  engine.Free;
  tests.Free;
end;

(*
{ TDigitalSignatureTests }

procedure TDigitalSignatureTests.testFile(filename : String);
var
  bytes : TBytes;
  f : TFileStream;
  sig : TDigitalSigner;
begin
  f := TFileStream.Create(filename, fmOpenRead);
  try
    setLength(bytes, f.Size);
    f.Read(bytes[0], length(bytes));
  finally
    f.free;
  end;
  sig := TDigitalSigner.Create;
  try
    assertTrue(sig.verifySignature(bytes));
  finally
    sig.Free;
  end;
end;

procedure TDigitalSignatureTests.testFileDSA;
begin
  testFile('C:\work\fhirserver\utilities\tests\signatures\java_example_dsa.xml');
end;

procedure TDigitalSignatureTests.testFileJames;
begin
  testFile('C:\work\fhirserver\utilities\tests\signatures\james.xml');
end;

procedure TDigitalSignatureTests.testFileRSA;
begin
  testFile('C:\work\fhirserver\utilities\tests\signatures\java_example_rsa.xml');
end;

procedure TDigitalSignatureTests.testGenRsa_1;
var
  bytes : TBytes;
  sig : TDigitalSigner;
begin
  sig := TDigitalSigner.Create;
  try
    sig.PrivateKey := 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem';

    bytes := sig.signEnveloped(TEncoding.UTF8.GetBytes('<Envelope xmlns="urn:envelope">'+#13#10+'</Envelope>'+#13#10), sdXmlRSASha1, true);
  finally
    sig.Free;
  end;

  sig := TDigitalSigner.Create;
  try
    assertTrue(sig.verifySignature(bytes));
  finally
    sig.Free;
  end;
end;

procedure TDigitalSignatureTests.testGenRsa_256;
var
  bytes : TBytes;
  sig : TDigitalSigner;
begin
  sig := TDigitalSigner.Create;
  try
    sig.PrivateKey := 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem';

    bytes := sig.signEnveloped(TEncoding.UTF8.GetBytes('<Envelope xmlns="urn:envelope">'+#13#10+'</Envelope>'+#13#10), sdXmlRSASha256, true);
  finally
    sig.Free;
  end;

  sig := TDigitalSigner.Create;
  try
    assertTrue(sig.verifySignature(bytes));
  finally
    sig.Free;
  end;
end;

//procedure TDigitalSignatureTests.testGenDsa_1;
//var
//  bytes : TBytes;
//  sig : TDigitalSigner;
//  output : string;
//begin
//  sig := TDigitalSigner.Create;
//  try
//    sig.PrivateKey := 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem';
//
//    bytes := sig.signEnveloped(TEncoding.UTF8.GetBytes('<Envelope xmlns="urn:envelope">'+#13#10+'</Envelope>'+#13#10), sdXmlDSASha1, true);
//  finally
//    sig.Free;
//  end;
//
//  sig := TDigitalSigner.Create;
//  try
//    assertTrue(sig.verifySignature(bytes));
//  finally
//    sig.Free;
//  end;
//end;
//
//procedure TDigitalSignatureTests.testGenDsa_256;
//var
//  bytes : TBytes;
//  sig : TDigitalSigner;
//  output : string;
//begin
//  sig := TDigitalSigner.Create;
//  try
//    sig.PrivateKey := 'C:\work\fhirserver\utilities\tests\signatures\private_key.pem';
//
//    bytes := sig.signEnveloped(TEncoding.UTF8.GetBytes('<Envelope xmlns="urn:envelope">'+#13#10+'</Envelope>'+#13#10), sdXmlDSASha256, true);
//  finally
//    sig.Free;
//  end;
//
//  sig := TDigitalSigner.Create;
//  try
//    assertTrue(sig.verifySignature(bytes));
//  finally
//    sig.Free;
//  end;
//end;
  *)

{ TFslTestObjectList }

function TFslTestObjectList.GetEntry(iIndex: Integer): TFslTestObject;
begin
  result := TFslTestObject(ObjectByIndex[iIndex]);
end;

function TFslTestObjectList.FindByValue(entry: TFslTestObject; out iIndex: Integer): Boolean;
begin
  Result := Find(entry, iIndex, CompareByValue);
end;

function TFslTestObjectList.ItemClass: TFslObjectClass;
begin
  result := TFslTestObject;
end;

function TFslTestObjectList.CompareByValue(pA, pB: Pointer): Integer;
begin
  Result := StringCompare(TFslTestObject(pA).Value, TFslTestObject(pB).Value);
end;

procedure TFslTestObjectList.SortedByValue;
begin
  SortedBy(CompareByValue);
end;

function TFslTestObjectList.FindByValue(value: String; out iIndex: Integer): Boolean;
Var
  entry : TFslTestObject;
Begin
  entry := TFslTestObject(ItemNew);
  Try
    entry.value := value;

    Result := FindByValue(entry, iIndex);
  Finally
    entry.Free;
  End;
end;

function TFslTestObjectList.getByValue(value: String): TFslTestObject;
Var
  iIndex : Integer;
Begin
  If FindByValue(value, iIndex) Then
    Result := Entries[iIndex]
  Else
    Result := Nil;
end;

{ TFslCollectionsTests }

procedure TFslCollectionsTests.testAdd;
begin
  list := TFslTestObjectList.create;
  try
    list.Add(TFslTestObject.create);
    assertTrue(list.Count = 1);
  finally
    list.Free;
  end;
end;

procedure TFslCollectionsTests.executeFail();
begin
  list.Add(TFslTestObjectList.create);
end;

procedure TFslCollectionsTests.testAddFail;
begin
  list := TFslTestObjectList.create;
  try
    assertWillRaise(executeFail, EFslInvariant, '');
    assertTrue(list.Count = 0);
  finally
    list.Free;
  end;
end;

procedure TFslCollectionsTests.testSort;
begin
  list := TFslTestObjectList.create;
  try
    list.Add(TFslTestObject.create('B'));
    list.Add(TFslTestObject.create('D'));
    list.Add(TFslTestObject.create('C'));
    list.Add(TFslTestObject.create('A'));
    list.sortedByValue;
    assertTrue(list[0].Value = 'A');
    assertTrue(list[1].Value = 'B');
    assertTrue(list[2].Value = 'C');
    assertTrue(list[3].Value = 'D');
  finally
    list.Free;
  end;
end;

procedure TFslCollectionsTests.testSort2;
begin
  list := TFslTestObjectList.create;
  try
    list.sortedByValue;
    list.Add(TFslTestObject.create('A'));
    list.Add(TFslTestObject.create('B'));
    assertTrue(list[0].Value = 'A');
    assertTrue(list[1].Value = 'B');
    assertTrue(list.getByValue('A') <> nil);
    assertTrue(list.getByValue('B') <> nil);
    assertTrue(list.getByValue('C') = nil);
  finally
    list.Free;
  end;
end;

{ TFslTestObject }

constructor TFslTestObject.create(value: String);
begin
  Create;
  self.value := value;
end;

{ TXmlUtilsTest }

procedure TXmlUtilsTest.TestNoDense;
var
  x : TMXmlDocument;
  src, output, tgt : String;
begin
  src := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-pretty.xml']), TEncoding.UTF8);
  tgt := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-pretty.xml']), TEncoding.UTF8);
  x := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    output := x.ToXml(true, false);
  finally
    x.Free;
  end;
  StringToFile(output, TestSettings.serverTestFile(['testcases', 'xml', 'xml-output.xml']), TEncoding.UTF8);
  assertEqual(output, tgt);
end;

procedure TXmlUtilsTest.TestNoPretty;
var
  x : TMXmlDocument;
  src, output, tgt : String;
begin
  src := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-condensed.xml']), TEncoding.UTF8);
  tgt := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-condensed.xml']), TEncoding.UTF8);
  x := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    output := x.ToXml(false, false);
  finally
    x.Free;
  end;
  StringToFile(output, TestSettings.serverTestFile(['testcases', 'xml', 'xml-output.xml']), TEncoding.UTF8);
  assertEqual(output, tgt);
end;

procedure TXmlUtilsTest.TestPretty;
var
  x : TMXmlDocument;
  src, output, tgt : String;
begin
  src := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-condensed.xml']), TEncoding.UTF8);
  tgt := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-pretty.xml']), TEncoding.UTF8);
  x := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    output := x.ToXml(true, false);
  finally
    x.Free;
  end;
  StringToFile(output, TestSettings.serverTestFile(['testcases', 'xml', 'xml-output.xml']), TEncoding.UTF8);
  assertEqual(output, tgt);
end;

procedure TXmlUtilsTest.TestUnPretty;
var
  x : TMXmlDocument;
  src, output, tgt : String;
begin
  src := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-pretty.xml']), TEncoding.UTF8);
  tgt := FileToString(TestSettings.serverTestFile(['testcases', 'xml', 'xml-condensed.xml']), TEncoding.UTF8);
  x := TMXmlParser.parse(src, [xpDropWhitespace]);
  try
    output := x.ToXml(false, false);
  finally
    x.Free;
  end;
  assertEqual(output, tgt);
end;

{ TTarGZParserTests }

function TTarGZParserTests.load(filename : String) : TFslList<TFslNameBuffer>;
var
  z : TZDecompressionStream;
  tar : TTarArchive;
  entry : TTarDirRec;
  bi : TBytesStream;
  item : TFslNameBuffer;
  stream : TFileStream;
begin
  result := TFslList<TFslNameBuffer>.create;
  try
    stream := TFileStream.Create(filename, fmOpenRead);
    try
      z := TZDecompressionStream.Create(stream, 15+16);
      try
        tar := TTarArchive.Create(z);
        try
          while tar.FindNext(entry) do
          begin
            item := TFslNameBuffer.Create;
            try
              item.Name := String(entry.Name);
              bi := TBytesStream.Create;
              try
                tar.ReadFile(bi);
                item.AsBytes := copy(bi.Bytes, 0, bi.size);
              finally
                bi.free;
              end;
              result.Add(item.link)
            finally
              item.Free;
            end;
          end;
        finally
          tar.free;
        end;
      finally
        z.free;
      end;
    finally
      stream.Free;
    end;
    result.link;
  finally
    result.Free;
  end;
end;

procedure TTarGZParserTests.testPackage;
var
  tgz : TFslList<TFslNameBuffer>;
begin
  tgz := load(TestSettings.serverTestFile(['testcases', 'tgz', 'package.tgz']));
  try
    assertTrue(tgz.Count = 11);
  finally
    tgz.Free;
  end;
end;

procedure TTarGZParserTests.testTzData;
var
  tgz : TFslList<TFslNameBuffer>;
begin
  tgz := load('tzdata.tar.gz');
  try
    assertTrue(tgz.Count = 12);
  finally
    tgz.Free;
  end;
end;

procedure RegisterTests;
// don't use unit initialization - give other code time to set up directories etc
begin
  // These tests exist only to test the test infrastructure itself, if that's needed

  RegisterTest('Library.Generics Tests', TFslGenericsTests.Suite);
  RegisterTest('Library.Collection Tests', TFslCollectionsTests.Suite);
  RegisterTest('Library.XPlatform Tests', TXPlatformTests.Suite);
  RegisterTest('Library.Decimal Tests', TDecimalTests.Suite);
  RegisterTest('Library.TarGz Tests', TTarGZParserTests.Suite);
  RegisterTest('Library.Utilities tests', TFslUtilitiesTestCases.Suite);
  {$IFDEF FPC}
  RegisterTest('Library.Regex Test', TFslRegexTests.Suite);
  {$ENDIF}

  RegisterTest('Formats.XML Tests', TXmlParserTests.create);
  RegisterTest('Formats.XML Tests', TXmlParserTest2.Suite);
  RegisterTest('Formats.XML Utility Tests', TXmlUtilsTest.Suite);
  RegisterTest('Formats.XPath Tests', TXPathParserTests.create);
  RegisterTest('Formats.XPath Engine Tests', TXPathEngineTests.create);
  RegisterTest('Formats.XML Patch Tests', TXmlPatchTests.create);
  RegisterTest('Formats.Json Tests', TJsonTests.Suite);
  RegisterTest('Formats.Turtle Tests', TTurtleTests.Suite);
end;

end.



