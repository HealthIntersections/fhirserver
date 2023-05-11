unit FHIR.WP.Unicode;

{
Copyright (c) 2001+, Kestral Computing Pty Ltd (http://www.kestral.com.au)
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

interface

Uses
  Windows, SysUtils, Classes, Graphics, Generics.Collections,
  fsl_utilities;

Const
  Unicode_Zero_Width_Joiner = #$200D;
  Unicode_Zero_Width_NonJoiner = #$200C;
  Unicode_Space = #$0020;
  Unicode_No_Break_Space = #$00A0; // produces a baseline advance without a glyph but inhibits rather than enabling a line-break.
  Unicode_No_Width_Space = #$200B; // allows a line-break but provides no space: in a sense joining, rather than separating, two words.
  Unicode_Word_Joiner = #$2060; // inhibits line breaks and also involves none of the white space produced by a baseline advance.
  Unicode_Line_Separator = #$2028;
  Unicode_Paragraph_Separator = #$2029;

  // For legacy reasons, the UCS also includes spaces of varying sizes that are compatibility equivalents for the space character.
  // While these spaces of varying width are important in typography, the Unicode processing model calls for such visual effects
  // to be handled by rich text, markup and other such protocols. They are included in the Unicode repertoire primarily to handle
  // lossless roundtrip transcoding from other character set encodings. These spaces include:
  Unicode_Space_En_Quad = #$2000;
  Unicode_Space_Em_Quad = #$2001;
  Unicode_Space_En_Space = #$2002;
  Unicode_Space_Em_Space = #$2003;
  Unicode_Space_Three_Per_Em_Space = #$2004;
  Unicode_Space_Four_Per_Em_Space = #$2005;
  Unicode_Space_Six_Per_Em_Space = #$2006;
  Unicode_Space_Figure_Space = #$2007;
  Unicode_Space_Punctuation_Space = #$2008;
  Unicode_Space_Thin_Space = #$2009;
  Unicode_Space_Hair_Space = #$200A;
  Unicode_Space_Mathematical_Space = #$205F;

  Unicode_Mongolian_Vowel_Separator = #$180E;
  Unicode_Ideographic_Space = #$3000; // behaves as an ideographic separator and generally rendered as white space of the same width as an ideograph.
  Unicode_Ogham_Space_Mark = #$1680; // this character is sometimes displayed with a glyph and other times as only white space.

  Unicode_Non_breaking_hyphen = #$2011;
  Unicode_Tibetan_Mark_Delimiter_Tsheg_Bstar = #$0F0C;
  Unicode_Narrow_no_break_space = #$202F;
  Unicode_Soft_hyphen = #$00AD;
  Unicode_Tibetan_Mark_Intersyllabic_Tsheg = #$0F0B;
  Unicode_Zero_width_space = #$200B;

  Unicode_Left_to_right_mark = #$200E;
  Unicode_Right_to_left_mark = #$200F;
  Unicode_Left_to_right_embedding = #$202A;
  Unicode_Right_to_left_embedding = #$202B;
  Unicode_Pop_directional_formatting = #$202C;
  Unicode_Left_to_right_override = #$202D;
  Unicode_Right_to_left_override = #$202E;

Function IsUnicodeWhiteSpace(ch : Char) : Boolean;

Type
  TUnicodeBlock = (
     Unicode_Block_All,
     Unicode_Block_Basic_Latin, Unicode_Block_Latin_1_Supplement, Unicode_Block_Latin_Extended_A, Unicode_Block_Latin_Extended_B, Unicode_Block_IPA_Extensions,
     Unicode_Block_Spacing_Modifier_Letters, Unicode_Block_Combining_Diacritical_Marks, Unicode_Block_Greek_and_Coptic, Unicode_Block_Cyrillic, Unicode_Block_Cyrillic_Supplement,
     Unicode_Block_Armenian, Unicode_Block_Hebrew, Unicode_Block_Arabic, Unicode_Block_Syriac, Unicode_Block_Arabic_Supplement,
     Unicode_Block_Thaana, Unicode_Block_NKo, Unicode_Block_Samaritan, Unicode_Block_Mandaic, Unicode_Block_Arabic_Extended_A,
     Unicode_Block_Devanagari, Unicode_Block_Bengali, Unicode_Block_Gurmukhi, Unicode_Block_Gujarati, Unicode_Block_Oriya,
     Unicode_Block_Tamil, Unicode_Block_Telugu, Unicode_Block_Kannada, Unicode_Block_Malayalam, Unicode_Block_Sinhala,
     Unicode_Block_Thai, Unicode_Block_Lao, Unicode_Block_Tibetan, Unicode_Block_Myanmar, Unicode_Block_Georgian,
     Unicode_Block_Hangul_Jamo, Unicode_Block_Ethiopic, Unicode_Block_Ethiopic_Supplement, Unicode_Block_Cherokee, Unicode_Block_Unified_Canadian_Aboriginal_Syllabics,
     Unicode_Block_Ogham, Unicode_Block_Runic, Unicode_Block_Tagalog, Unicode_Block_Hanunoo, Unicode_Block_Buhid,
     Unicode_Block_Tagbanwa, Unicode_Block_Khmer, Unicode_Block_Mongolian, Unicode_Block_Unified_Canadian_Aboriginal_Syllabics_Extended, Unicode_Block_Limbu,
     Unicode_Block_Tai_Le, Unicode_Block_New_Tai_Lue, Unicode_Block_Khmer_Symbols, Unicode_Block_Buginese, Unicode_Block_Tai_Tham,
     Unicode_Block_Balinese, Unicode_Block_Sundanese, Unicode_Block_Batak, Unicode_Block_Lepcha, Unicode_Block_Ol_Chiki,
     Unicode_Block_Sundanese_Supplement, Unicode_Block_Vedic_Extensions, Unicode_Block_Phonetic_Extensions, Unicode_Block_Phonetic_Extensions_Supplement, Unicode_Block_Combining_Diacritical_Marks_Supplement,
     Unicode_Block_Latin_Extended_Additional, Unicode_Block_Greek_Extended, Unicode_Block_General_Punctuation, Unicode_Block_Superscripts_and_Subscripts, Unicode_Block_Currency_Symbols,
     Unicode_Block_Combining_Diacritical_Marks_for_Symbols, Unicode_Block_Letterlike_Symbols, Unicode_Block_Number_Forms, Unicode_Block_Arrows, Unicode_Block_Mathematical_Operators,
     Unicode_Block_Miscellaneous_Technical, Unicode_Block_Control_Pictures, Unicode_Block_Optical_Character_Recognition, Unicode_Block_Enclosed_Alphanumerics, Unicode_Block_Box_Drawing,
     Unicode_Block_Block_Elements, Unicode_Block_Geometric_Shapes, Unicode_Block_Miscellaneous_Symbols, Unicode_Block_Dingbats, Unicode_Block_Miscellaneous_Mathematical_Symbols_A,
     Unicode_Block_Supplemental_Arrows_A, Unicode_Block_Braille_Patterns, Unicode_Block_Supplemental_Arrows_B, Unicode_Block_Miscellaneous_Mathematical_Symbols_B, Unicode_Block_Supplemental_Mathematical_Operators,
     Unicode_Block_Miscellaneous_Symbols_and_Arrows, Unicode_Block_Glagolitic, Unicode_Block_Latin_Extended_C, Unicode_Block_Coptic, Unicode_Block_Georgian_Supplement,
     Unicode_Block_Tifinagh, Unicode_Block_Ethiopic_Extended, Unicode_Block_Cyrillic_Extended_A, Unicode_Block_Supplemental_Punctuation, Unicode_Block_CJK_Radicals_Supplement,
     Unicode_Block_Kangxi_Radicals, Unicode_Block_Ideographic_Description_Characters, Unicode_Block_CJK_Symbols_and_Punctuation, Unicode_Block_Hiragana, Unicode_Block_Katakana,
     Unicode_Block_Bopomofo, Unicode_Block_Hangul_Compatibility_Jamo, Unicode_Block_Kanbun, Unicode_Block_Bopomofo_Extended, Unicode_Block_CJK_Strokes,
     Unicode_Block_Katakana_Phonetic_Extensions, Unicode_Block_Enclosed_CJK_Letters_and_Months, Unicode_Block_CJK_Compatibility, Unicode_Block_CJK_Unified_Ideographs_Extension_A, Unicode_Block_Yijing_Hexagram_Symbols,
     Unicode_Block_CJK_Unified_Ideographs, Unicode_Block_Yi_Syllables, Unicode_Block_Yi_Radicals, Unicode_Block_Lisu, Unicode_Block_Vai,
     Unicode_Block_Cyrillic_Extended_B, Unicode_Block_Bamum, Unicode_Block_Modifier_Tone_Letters, Unicode_Block_Latin_Extended_D, Unicode_Block_Syloti_Nagri,
     Unicode_Block_Common_Indic_Number_Forms, Unicode_Block_Phags_pa, Unicode_Block_Saurashtra, Unicode_Block_Devanagari_Extended, Unicode_Block_Kayah_Li,
     Unicode_Block_Rejang, Unicode_Block_Hangul_Jamo_Extended_A, Unicode_Block_Javanese, Unicode_Block_Cham, Unicode_Block_Myanmar_Extended_A,
     Unicode_Block_Tai_Viet, Unicode_Block_Meetei_Mayek_Extensions, Unicode_Block_Ethiopic_Extended_A, Unicode_Block_Meetei_Mayek, Unicode_Block_Hangul_Syllables,
     Unicode_Block_Hangul_Jamo_Extended_B, Unicode_Block_High_Surrogates, Unicode_Block_High_Private_Use_Surrogates, Unicode_Block_Low_Surrogates, Unicode_Block_Private_Use_Area,
     Unicode_Block_CJK_Compatibility_Ideographs, Unicode_Block_Alphabetic_Presentation_Forms, Unicode_Block_Arabic_Presentation_Forms_A, Unicode_Block_Variation_Selectors, Unicode_Block_Vertical_Forms,
     Unicode_Block_Combining_Half_Marks, Unicode_Block_CJK_Compatibility_Forms, Unicode_Block_Small_Form_Variants, Unicode_Block_Arabic_Presentation_Forms_B, Unicode_Block_Halfwidth_and_Fullwidth_Forms,
     Unicode_Block_Specials{, Unicode_Block_Linear_B_Syllabary, Unicode_Block_Linear_B_Ideograms, Unicode_Block_Aegean_Numbers, Unicode_Block_Ancient_Greek_Numbers,
     Unicode_Block_Ancient_Symbols, Unicode_Block_Phaistos_Disc, Unicode_Block_Lycian, Unicode_Block_Carian, Unicode_Block_Old_Italic,
     Unicode_Block_Gothic, Unicode_Block_Ugaritic, Unicode_Block_Old_Persian, Unicode_Block_Deseret, Unicode_Block_Shavian,
     Unicode_Block_Osmanya, Unicode_Block_Cypriot_Syllabary, Unicode_Block_Imperial_Aramaic, Unicode_Block_Phoenician, Unicode_Block_Lydian,
     Unicode_Block_Meroitic_Hieroglyphs, Unicode_Block_Meroitic_Cursive, Unicode_Block_Kharoshthi, Unicode_Block_Old_South_Arabian, Unicode_Block_Avestan,
     Unicode_Block_Inscriptional_Parthian, Unicode_Block_Inscriptional_Pahlavi, Unicode_Block_Old_Turkic, Unicode_Block_Rumi_Numeral_Symbols, Unicode_Block_Brahmi,
     Unicode_Block_Kaithi, Unicode_Block_Sora_Sompeng, Unicode_Block_Chakma, Unicode_Block_Sharada, Unicode_Block_Takri,
     Unicode_Block_Cuneiform, Unicode_Block_Cuneiform_Numbers_and_Punctuation, Unicode_Block_Egyptian_Hieroglyphs, Unicode_Block_Bamum_Supplement, Unicode_Block_Miao,
     Unicode_Block_Kana_Supplement, Unicode_Block_Byzantine_Musical_Symbols, Unicode_Block_Musical_Symbols, Unicode_Block_Ancient_Greek_Musical_Notation, Unicode_Block_Tai_Xuan_Jing_Symbols,
     Unicode_Block_Counting_Rod_Numerals, Unicode_Block_Mathematical_Alphanumeric_Symbols, Unicode_Block_Arabic_Mathematical_Alphabetic_Symbols, Unicode_Block_Mahjong_Tiles, Unicode_Block_Domino_Tiles,
     Unicode_Block_Playing_Cards, Unicode_Block_Enclosed_Alphanumeric_Supplement, Unicode_Block_Enclosed_Ideographic_Supplement, Unicode_Block_Miscellaneous_Symbols_And_Pictographs, Unicode_Block_Emoticons,
     Unicode_Block_Transport_And_Map_Symbols, Unicode_Block_Alchemical_Symbols, Unicode_Block_CJK_Unified_Ideographs_Extension_B, Unicode_Block_CJK_Unified_Ideographs_Extension_C, Unicode_Block_CJK_Unified_Ideographs_Extension_D,
     Unicode_Block_CJK_Compatibility_Ideographs_Supplement, Unicode_Block_Tags, Unicode_Block_Variation_Selectors_Supplement, Unicode_Block_Supplementary_Private_Use_Area_A, Unicode_Block_Supplementary_Private_Use_Area_B});

  TUnicodeBlockSet = set of TUnicodeBlock;

Const
  UNICODE_BLOCK_NAMES : Array [TUnicodeBlock] of String = (
    'All',
    'Basic Latin', 'Latin-1 Supplement', 'Latin Extended-A', 'Latin Extended-B', 'IPA Extensions',
    'Spacing Modifier Letters', 'Combining Diacritical Marks', 'Greek and Coptic', 'Cyrillic', 'Cyrillic Supplement',
    'Armenian', 'Hebrew', 'Arabic', 'Syriac', 'Arabic Supplement',
    'Thaana', 'NKo', 'Samaritan', 'Mandaic', 'Arabic Extended-A',
    'Devanagari', 'Bengali', 'Gurmukhi', 'Gujarati', 'Oriya',
    'Tamil', 'Telugu', 'Kannada', 'Malayalam', 'Sinhala',
    'Thai', 'Lao', 'Tibetan', 'Myanmar', 'Georgian',
    'Hangul Jamo', 'Ethiopic', 'Ethiopic Supplement', 'Cherokee', 'Unified Canadian Aboriginal Syllabics',
    'Ogham', 'Runic', 'Tagalog', 'Hanunoo', 'Buhid',
    'Tagbanwa', 'Khmer', 'Mongolian', 'Unified Canadian Aboriginal Syllabics Extended', 'Limbu',
    'Tai Le', 'New Tai Lue', 'Khmer Symbols', 'Buginese', 'Tai Tham',
    'Balinese', 'Sundanese', 'Batak', 'Lepcha', 'Ol Chiki',
    'Sundanese Supplement', 'Vedic Extensions', 'Phonetic Extensions', 'Phonetic Extensions Supplement', 'Combining Diacritical Marks Supplement',
    'Latin Extended Additional', 'Greek Extended', 'General Punctuation', 'Superscripts and Subscripts', 'Currency Symbols',
    'Combining Diacritical Marks for Symbols', 'Letterlike Symbols', 'Number Forms', 'Arrows', 'Mathematical Operators',
    'Miscellaneous Technical', 'Control Pictures', 'Optical Character Recognition', 'Enclosed Alphanumerics', 'Box Drawing',
    'Block Elements', 'Geometric Shapes', 'Miscellaneous Symbols', 'Dingbats', 'Miscellaneous Mathematical Symbols-A',
    'Supplemental Arrows-A', 'Braille Patterns', 'Supplemental Arrows-B', 'Miscellaneous Mathematical Symbols-B', 'Supplemental Mathematical Operators',
    'Miscellaneous Symbols and Arrows', 'Glagolitic', 'Latin Extended-C', 'Coptic', 'Georgian Supplement',
    'Tifinagh', 'Ethiopic Extended', 'Cyrillic Extended-A', 'Supplemental Punctuation', 'CJK Radicals Supplement',
    'Kangxi Radicals', 'Ideographic Description Characters', 'CJK Symbols and Punctuation', 'Hiragana', 'Katakana',
    'Bopomofo', 'Hangul Compatibility Jamo', 'Kanbun', 'Bopomofo Extended', 'CJK Strokes',
    'Katakana Phonetic Extensions', 'Enclosed CJK Letters and Months', 'CJK Compatibility', 'CJK Unified Ideographs Extension A', 'Yijing Hexagram Symbols',
    'CJK Unified Ideographs', 'Yi Syllables', 'Yi Radicals', 'Lisu', 'Vai',
    'Cyrillic Extended-B', 'Bamum', 'Modifier Tone Letters', 'Latin Extended-D', 'Syloti Nagri',
    'Common Indic Number Forms', 'Phags-pa', 'Saurashtra', 'Devanagari Extended', 'Kayah Li',
    'Rejang', 'Hangul Jamo Extended-A', 'Javanese', 'Cham', 'Myanmar Extended-A',
    'Tai Viet', 'Meetei Mayek Extensions', 'Ethiopic Extended-A', 'Meetei Mayek', 'Hangul Syllables',
    'Hangul Jamo Extended-B', 'High Surrogates', 'High Private Use Surrogates', 'Low Surrogates', 'Private Use Area',
    'CJK Compatibility Ideographs', 'Alphabetic Presentation Forms', 'Arabic Presentation Forms-A', 'Variation Selectors', 'Vertical Forms',
    'Combining Half Marks', 'CJK Compatibility Forms', 'Small Form Variants', 'Arabic Presentation Forms-B', 'Halfwidth and Fullwidth Forms',
    'Specials'{, 'Linear B Syllabary', 'Linear B Ideograms', 'Aegean Numbers', 'Ancient Greek Numbers',
    'Ancient Symbols', 'Phaistos Disc', 'Lycian', 'Carian', 'Old Italic',
    'Gothic', 'Ugaritic', 'Old Persian', 'Deseret', 'Shavian',
    'Osmanya', 'Cypriot Syllabary', 'Imperial Aramaic', 'Phoenician', 'Lydian',
    'Meroitic Hieroglyphs', 'Meroitic Cursive', 'Kharoshthi', 'Old South Arabian', 'Avestan',
    'Inscriptional Parthian', 'Inscriptional Pahlavi', 'Old Turkic', 'Rumi Numeral Symbols', 'Brahmi',
    'Kaithi', 'Sora Sompeng', 'Chakma', 'Sharada', 'Takri',
    'Cuneiform', 'Cuneiform Numbers and Punctuation', 'Egyptian Hieroglyphs', 'Bamum Supplement', 'Miao',
    'Kana Supplement', 'Byzantine Musical Symbols', 'Musical Symbols', 'Ancient Greek Musical Notation', 'Tai Xuan Jing Symbols',
    'Counting Rod Numerals', 'Mathematical Alphanumeric Symbols', 'Arabic Mathematical Alphabetic Symbols', 'Mahjong Tiles', 'Domino Tiles',
    'Playing Cards', 'Enclosed Alphanumeric Supplement', 'Enclosed Ideographic Supplement', 'Miscellaneous Symbols And Pictographs', 'Emoticons',
    'Transport And Map Symbols', 'Alchemical Symbols', 'CJK Unified Ideographs Extension B', 'CJK Unified Ideographs Extension C', 'CJK Unified Ideographs Extension D',
    'CJK Compatibility Ideographs Supplement', 'Tags', 'Variation Selectors Supplement', 'Supplementary Private Use Area-A', 'Supplementary Private Use Area-B'});

  UNICODE_BLOCK_RANGES : Array [TUnicodeBlock] of String = (
    '0020..FFFF',
    '0020..007F', '0080..00FF', '0100..017F', '0180..024F', '0250..02AF',
    '02B0..02FF', '0300..036F', '0370..03FF', '0400..04FF', '0500..052F',
    '0530..058F', '0590..05FF', '0600..06FF', '0700..074F', '0750..077F',
    '0780..07BF', '07C0..07FF', '0800..083F', '0840..085F', '08A0..08FF',
    '0900..097F', '0980..09FF', '0A00..0A7F', '0A80..0AFF', '0B00..0B7F',
    '0B80..0BFF', '0C00..0C7F', '0C80..0CFF', '0D00..0D7F', '0D80..0DFF',
    '0E00..0E7F', '0E80..0EFF', '0F00..0FFF', '1000..109F', '10A0..10FF',
    '1100..11FF', '1200..137F', '1380..139F', '13A0..13FF', '1400..167F',
    '1680..169F', '16A0..16FF', '1700..171F', '1720..173F', '1740..175F',
    '1760..177F', '1780..17FF', '1800..18AF', '18B0..18FF', '1900..194F',
    '1950..197F', '1980..19DF', '19E0..19FF', '1A00..1A1F', '1A20..1AAF',
    '1B00..1B7F', '1B80..1BBF', '1BC0..1BFF', '1C00..1C4F', '1C50..1C7F',
    '1CC0..1CCF', '1CD0..1CFF', '1D00..1D7F', '1D80..1DBF', '1DC0..1DFF',
    '1E00..1EFF', '1F00..1FFF', '2000..206F', '2070..209F', '20A0..20CF',
    '20D0..20FF', '2100..214F', '2150..218F', '2190..21FF', '2200..22FF',
    '2300..23FF', '2400..243F', '2440..245F', '2460..24FF', '2500..257F',
    '2580..259F', '25A0..25FF', '2600..26FF', '2700..27BF', '27C0..27EF',
    '27F0..27FF', '2800..28FF', '2900..297F', '2980..29FF', '2A00..2AFF',
    '2B00..2BFF', '2C00..2C5F', '2C60..2C7F', '2C80..2CFF', '2D00..2D2F',
    '2D30..2D7F', '2D80..2DDF', '2DE0..2DFF', '2E00..2E7F', '2E80..2EFF',
    '2F00..2FDF', '2FF0..2FFF', '3000..303F', '3040..309F', '30A0..30FF',
    '3100..312F', '3130..318F', '3190..319F', '31A0..31BF', '31C0..31EF',
    '31F0..31FF', '3200..32FF', '3300..33FF', '3400..4DBF', '4DC0..4DFF',
    '4E00..9FFF', 'A000..A48F', 'A490..A4CF', 'A4D0..A4FF', 'A500..A63F',
    'A640..A69F', 'A6A0..A6FF', 'A700..A71F', 'A720..A7FF', 'A800..A82F',
    'A830..A83F', 'A840..A87F', 'A880..A8DF', 'A8E0..A8FF', 'A900..A92F',
    'A930..A95F', 'A960..A97F', 'A980..A9DF', 'AA00..AA5F', 'AA60..AA7F',
    'AA80..AADF', 'AAE0..AAFF', 'AB00..AB2F', 'ABC0..ABFF', 'AC00..D7AF',
    'D7B0..D7FF', 'D800..DB7F', 'DB80..DBFF', 'DC00..DFFF', 'E000..F8FF',
    'F900..FAFF', 'FB00..FB4F', 'FB50..FDFF', 'FE00..FE0F', 'FE10..FE1F',
    'FE20..FE2F', 'FE30..FE4F', 'FE50..FE6F', 'FE70..FEFF', 'FF00..FFEF',
    'FFF0..FFFF'{, '10000..1007F', '10080..100FF', '10100..1013F', '10140..1018F',
    '10190..101CF', '101D0..101FF', '10280..1029F', '102A0..102DF', '10300..1032F',
    '10330..1034F', '10380..1039F', '103A0..103DF', '10400..1044F', '10450..1047F',
    '10480..104AF', '10800..1083F', '10840..1085F', '10900..1091F', '10920..1093F',
    '10980..1099F', '109A0..109FF', '10A00..10A5F', '10A60..10A7F', '10B00..10B3F',
    '10B40..10B5F', '10B60..10B7F', '10C00..10C4F', '10E60..10E7F', '11000..1107F',
    '11080..110CF', '110D0..110FF', '11100..1114F', '11180..111DF', '11680..116CF',
    '12000..123FF', '12400..1247F', '13000..1342F', '16800..16A3F', '16F00..16F9F',
    '1B000..1B0FF', '1D000..1D0FF', '1D100..1D1FF', '1D200..1D24F', '1D300..1D35F',
    '1D360..1D37F', '1D400..1D7FF', '1EE00..1EEFF', '1F000..1F02F', '1F030..1F09F',
    '1F0A0..1F0FF', '1F100..1F1FF', '1F200..1F2FF', '1F300..1F5FF', '1F600..1F64F',
    '1F680..1F6FF', '1F700..1F77F', '20000..2A6DF', '2A700..2B73F', '2B740..2B81F',
    '2F800..2FA1F', 'E0000..E007F', 'E0100..E01EF', 'F0000..FFFFF', '100000..10FFFF'});


Function GetBlocksForFont(fontName : String) : TUnicodeBlockSet;

Function GetCharsForBlock(block : TUnicodeBlock) : TCharArray; overload;
Function GetCharsForBlock(block : TUnicodeBlock; fontName : String; filter : String) : TCharArray; overload;

Function GetBlockForChar(ch : Char) : TUnicodeBlock;
Function GetNameForChar(ch : Char) : String;

implementation

{$R resources\FHIR.WP.Unicode.res}

var
  GCharNames : TDictionary<Char, String>;

Function IsUnicodeWhiteSpace(ch : Char) : Boolean;
begin
  result := CharInSet(ch, [#$0009..#$000D, #$0085, Unicode_Space, Unicode_No_Break_Space,
      Unicode_No_Width_Space, Unicode_Line_Separator, Unicode_Paragraph_Separator,
      Unicode_Zero_Width_NonJoiner, Unicode_Space_En_Quad, Unicode_Space_Em_Quad,
      Unicode_Space_En_Space, Unicode_Space_Em_Space, Unicode_Space_Three_Per_Em_Space,
      Unicode_Space_Four_Per_Em_Space, Unicode_Space_Six_Per_Em_Space, Unicode_Space_Figure_Space,
      Unicode_Space_Punctuation_Space, Unicode_Space_Thin_Space, Unicode_Space_Hair_Space,
      Unicode_Space_Mathematical_Space, Unicode_Mongolian_Vowel_Separator, Unicode_Ideographic_Space,
      Unicode_Ogham_Space_Mark, Unicode_Non_breaking_hyphen, Unicode_Tibetan_Mark_Delimiter_Tsheg_Bstar,
      Unicode_Zero_width_space]);
end;

Function GetCharsForBlock(block : TUnicodeBlock) : TCharArray;
var
  i, low, high : Word;
  l,h : string;
begin
  StringSplit(UNICODE_BLOCK_RANGES[block], '..', l, h);
  low := StrToInt('$'+l);
  high := StrToInt('$'+h);
  setlength(result, high - low + 1);
  for I := Low to High do
    result[i - low] := char(i);
end;

{$R-}
Function inGlyphRange(info : PGlyphSet; ch : word) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to info^.cRanges - 1 do
  begin
    if (ch >= ord(info^.Ranges[i].wcLow)) and (ch < ord(info^.Ranges[i].wcLow) + info^.Ranges[i].cGlyphs) then
    begin
      result := true;
      break;
    end;
  end;
end;

Function RangesOverlap(info : PGlyphSet; low, high : word) : boolean;
var
  i : integer;
begin
  result := false;
  for i := 0 to info^.cRanges - 1 do
  begin
    if (high >= ord(info^.Ranges[i].wcLow)) and (low <= ord(info^.Ranges[i].wcLow) + info^.Ranges[i].cGlyphs) then
    begin
      result := true;
      break;
    end;
  end;
end;

Function GetBlocksForFont(fontName : String) : TUnicodeBlockSet;
var
  info : PGlyphSet;
  size : integer;
  Bitmap : TBitmap;
  a : TUnicodeBlock;
  i, c, low, high : Word;
  l, h : string;
begin
  result := [Unicode_Block_All];
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Name := fontName;
    size := GetFontUnicodeRanges(Bitmap.Canvas.Handle, nil);
    GetMem(info, size);
    try
      GetFontUnicodeRanges(Bitmap.Canvas.Handle, info);

      for a := Unicode_Block_Basic_Latin to Unicode_Block_Specials do
      begin
        StringSplit(UNICODE_BLOCK_RANGES[a], '..', l, h);
        low := StrToInt('$'+l);
        high := StrToInt('$'+h);
        if RangesOverlap(info, low, high) then
           result := result + [a];
      end;
    finally
      freemem(info);
    end;
  finally
    Bitmap.Free;
  end;
end;

Function GetCharsForBlock(block : TUnicodeBlock; fontName : String; filter : String) : TCharArray;
var
  info : PGlyphSet;
  size : integer;
  Bitmap : TBitmap;
  i, c, low, high : Word;
  l, h : string;
begin
  filter := lowercase(filter);
  Bitmap := TBitmap.Create;
  try
    Bitmap.Canvas.Font.Name := fontName;
    size := GetFontUnicodeRanges(Bitmap.Canvas.Handle, nil);
    GetMem(info, size);
    try
      GetFontUnicodeRanges(Bitmap.Canvas.Handle, info);

      StringSplit(UNICODE_BLOCK_RANGES[block], '..', l, h);
      low := StrToInt('$'+l);
      high := StrToInt('$'+h);
      setlength(result, high - low + 1);
      c := 0;
      for I := Low to High do
      begin
        if inGlyphRange(info, i) and ((filter = '') or (pos(filter, lowercase(GetNameForChar(char(i)))) > 0) ) then
        begin
          result[c] := char(i);
          inc(c);
        end;
      end;
      SetLength(result, c);
    finally
      freemem(info);
    end;
  finally
    Bitmap.Free;
  end;
end;

Function GetBlockForChar(ch : Char) : TUnicodeBlock;
var
  a : TUnicodeBlock;
  low, high : Word;
  l, h : string;
begin
  result := Unicode_Block_All;
  for a := Unicode_Block_Basic_Latin to Unicode_Block_Specials do
  begin
    StringSplit(UNICODE_BLOCK_RANGES[a], '..', l, h);
    low := StrToInt('$'+l);
    high := StrToInt('$'+h);
    if (ord(ch) >= low) and (ord(ch) <= high) then
    begin
      result := a;
      break;
    end;
  end;
end;

procedure LoadCharNames;
var
  ResStream: TResourceStream;
  ansi : AnsiString;
  i, j : integer;
  ch : char;
begin
  ResStream := TResourceStream.Create(hInstance, 'Unicode_Data', RT_RCDATA);
  try
    SetLength(ansi, ResStream.Size);
    ResStream.Read(ansi[1], length(ansi));
  finally
    ResStream.free;
  end;
  GCharNames := TDictionary<Char,String>.create;
  i := 1;
  while (i< length(ansi)) do
  begin
    ch := Char(StrToInt('$'+copy(ansi, i, 4)));
    inc(i, 5);
    if ansi[i-1] <> ';' then
      break; // off scale of windows supported chars

    j := i;
    while (ansi[i] <> ';') do
      inc(i);
    GCharNames.Add(ch, StringTitleCase(copy(ansi, j, i-j)));
    while (ansi[i] <> #10) do
      inc(i);
    inc(i);
  end;
end;

Function GetNameForChar(ch : Char) : String;
begin
  if GCharNames = nil then
    LoadCharNames;
  if GCharNames.ContainsKey(ch) then
    result := GCharNames.Items[ch]
  else
    result := '??';
end;

Initialization
finalization
  GCharNames.Free;
end.
