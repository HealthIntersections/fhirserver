Unit FHIR.WP.V2Ft;


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

(*

So the idea is that there is one or more OBX Segments in a sequence
that must be parsed into a word processor document. In general, there
is two main different configurations: 1 OBX with FT and a sequence of
repeats, or a sequence of OBX's with a FT in each.

The interface of this just takes a stram which is a sequence
of OBX-5 fields / repeats, with the FT as RawContent (use
the RawContent property of the relevant field or repeat to get the
content) and separated by crlf's

This class can read TX types as well - they are really just a constrained
case of FT.

Here is a set of sample messages
** paste sample messages here along with the appropriate setings for the messages

Format 1 (Dictaphone) - each OBX is a paragraph

MSH|^~\&|POWERSCRIBE|RNSH|WESTMEAD|RNSH|20070317220949787||ORU^R01|20070317220900-1|P|2.2
PID|||1313464|331662|AIKARI^HADASSAH||19830902000000|F|||^C/- 18 CANDOWIE CRESCENT^BAULKHAM HILLS^^2153||9639 9509||||||||||||||||||
PV1||I|6N^^^RNSH||||||||||||||Gates^Gates^Robert^^^Dr||06R076388
ORC|RE|06R076388||06R076388GENREP
OBR||06R076388||CXRM^Chest(M)||20070130234200|20070130234200|||||||||Gates^Gates^Robert^^^Dr||Main|Gen||M|||Gen
OBX|1|FT|06R076388GENREP|1|CT BRAIN AND CT FACIAL BONES CLINICAL HISTORY:||||||F|||||DICT1^User Dictaphone
OBX|2|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|3|FT|06R076388GENREP|1|Fall, ? LOC.  On Aspirin.||||||F|||||DICT1^User Dictaphone
OBX|4|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|5|FT|06R076388GENREP|1|TECHNIQUE:   Non contrast axial scans have been performed from the skull base
    to the vertex.  Fine cuts have been performed through the facial bones with coronal and sagittal
    reformations. REPORT:  No acute intracerebral haemorrhage or cerebral contusion seen. There are no
    extra-axial collections.  The ventricles and subarachnoid spaces are prominent in keeping with the
    patient's age.  No subarachnoid haemorrhage seen.  There is moderate periventricular low density
    change consistent with small vessel ischaemia.   \Zi+\No acute infarct seen.\Zi-\   No skull base or vault
    fracture detected.  The mastoid air cells and paranasal sinuses are clear.  No facial bone
    fractures seen.  Deviation of the nasal septum to the left side is .  Allowing for metallic
    artefact from dental fillings, no mandibular fracture is seen.  The TMJs appear within
    normal limits.||||||F|||||DICT1^User Dictaphone
OBX|6|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|7|FT|06R076388GENREP|1|COMMENT:  No acute intracranial pathology identified.  No fracture seen.||||||F|||||DICT1^User Dictaphone
OBX|8|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|9|FT|06R076388GENREP|1|CT ABDOMEN AND PELVIS WITH CONTRAST CLINICAL HISTORY:||||||F|||||DICT1^User Dictaphone
OBX|10|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|11|FT|06R076388GENREP|1|MRI shows sepsis.  Abdominal pain and distension.  ? cause.||||||F|||||DICT1^User Dictaphone
OBX|12|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|13|FT|06R076388GENREP|1|TECHNIQUE:    Axial scans were performed through the abdomen and pelvis following
    oral and intravenous contrast.  Delayed scans were performed following insertion of
    a urinary catheter. FINDINGS:   There is a large fluid filled mass in the central
    lower abdomen measuring 24 cm AP x 20 cm trans x 25 cm cranio caudal.  This contains some internal
    septations and there is a small amount of soft tissue adjacent to the left posterolateral aspect
    of this mass.  There is a rounded calcific area closely related to this mass on the left side
    measuring 3 cm.  This mass is closely related to the urinary bladder, causing compression of the
    bladder.  There is no direct contact with the bladder, as confirmed by the scan following urinary
    catheterisation.  This mass most likely arises from an ovary, ? cyst adenoma, ? cyst adenocarcinoma.
    Further assessment by ultrasound is recommended.  The uterus appears separate from this mass
    and is anteverted and is of normal size.   There is a trace of free fluid within the abdomen, located
    to the right side of this cystic mass.  No free fluid is seen in the pouch of Douglas. There is
    effacement of the bowel but no evidence of obstruction.  No  collections are seen.  The liver,
    gall bladder, spleen, adrenals and pancreas are within normal limits. Both kidney are atrophied,
    with some perinephric stranding, consistent with a history of end stage renal failure.  There
    is no  lymphadenopathy. There is bibasal collapse/consolidation at the lung bases with small
    bilateral pleural effusions.  There is degenerative change within the lumbosacral spine.  There
    is irregularity of the inferior end plate of the L4 vertebral body with associated
    anterolisthesis of L4 on L5.  While this may relate to degenerative change, given the history
    of sepsis discitis/osteomyelitis should also be considered.  If there is concern then a
    bone scan is recommended.||||||F|||||DICT1^User Dictaphone
OBX|14|FT|06R076388GENREP|1|||||||F|||||DICT1^User Dictaphone
OBX|15|FT|06R076388GENREP|1|COMMENT:   Large cystic septated mass arising from the pelvis, likely ovarian in nature, ? cyst adenoma ? cyst adenocarcinoma.  Further assessment by ultrasound is recommended.  End plate irregularity of the L4 vertebral body, ? discitis/osteomyelitis. A bone scan may be helpful to further assess.  Bibasal collapse/consolidation.||||||F|||||DICT1^User Dictaphone



________________________________



FT codes:

\H\  start highlighting
\N\  normal text (end highlighting)

.sp <number> End current output line and skip <number> vertical spaces. <number> is a positive
integer or absent. If <number> is absent, skip one space. The horizontal character
position remains unchanged. Note that only for purposes of compatibility with
previous versions of HL7, ^\.sp\ is equivalent to \.br\.
.br Begin new output line. Set the horizontal position to the current left margin and
increment the vertical position by 1.
.in <number> Indent <number> of spaces, where <number> is a positive or negative integer. This
command cannot appear after the first printable character of a line.
.ti <number> Temporarily indent <number> of spaces where number is a positive or negative
integer. This command cannot appear after the first printable character of a line.
.sk < number> Skip <number> spaces to the right.
.ce End current output line and center the next line.

not supported

.fi Begin word wrap or fill mode. This is the default state. It can be changed to a no-wrap
mode using the .nf command.
.nf Begin no-wrap mode.


local extensions for formatting (all start with \Z)
i+ or i-      italics on or off
b+ or b-      bold on or off
u+ or u-      underline on or off
fc[C]         font color. plain fc to reset to default. C is a known HTML colour name, or a RGB value
fb[C]         background color. plain fc to reset to default. C is a known HTML colour name, or a RGB value with a # at the start
fs[N]         font size. plan fs to reset to default. + or - to make relative changes. ie fs+2 means 2 points bigger
fn[X]         font name. plain fn to reset to default
fo[super|sub] superscript or subscript. plain fo to reset to normal
pi            overrule normal analysis and insert a paragraph.

pl[N]         set left indent. plan l to reset to default
pr[N]         set right indent. plan l to reset to default
pa[l|r|c|j]   alignment - left, right, centre or justified. plain pa to reset to default
pt[v]         set paragraph type. possible values: disc, circle, square, arabic, alphaLow, alphaUp, romanLow, romainUp. plain "pt" means a normal paragraph
pn[v]         set number formatting for numbered lists. possible values: Dot, Slash, Parenthesis, Colon, SemiColon. plain "pn" means just a plan number

for all the paragraph controls below, the last one within a paragraph is the one used.
all other formatting states are reset when a new paragraph is started.

*)

Interface


Uses
  SysUtils,
  fsl_utilities, fsl_stream,
  wp_format, wp_text,
  wp_types, wp_document, wp_working;

Type
  TWPHL7FTReader = Class (TWPReader)
    Private
      FLastWasPara : Boolean;
      FHL7Font : TWPSFontDetails;
      FHL7Para : TWPSParagraphDetails;

      FLoop : Integer;
      FText : String;
      FWord : String;

      Procedure Commit(oDocument : TWPWorkingDocument; Const sWord : String);
      Procedure CommitWord(oDocument : TWPWorkingDocument);
      Procedure AddPara(oDocument : TWPWorkingDocument);

      Function GetChar(Var ch : Char) : Boolean;
      Function GetNumber : Integer;
      Function GetRemainder : String;

      Procedure ProcessEscape(oDocument : TWPWorkingDocument);
      Procedure ProcessDot(oDocument : TWPWorkingDocument);
      Procedure ProcessBr(oDocument : TWPWorkingDocument);
      Procedure ProcessCe(oDocument : TWPWorkingDocument);
      Procedure ProcessIn(oDocument : TWPWorkingDocument);
      Procedure ProcessSk(oDocument : TWPWorkingDocument);
      Procedure ProcessSp(oDocument : TWPWorkingDocument);
      Procedure ProcessTi;
      Procedure ProcessLocalExtension(oDocument : TWPWorkingDocument);
      Procedure ProcessFontCommand(oDocument : TWPWorkingDocument);
      Procedure ProcessParagraphCommand(oDocument : TWPWorkingDocument);

      Procedure SetBold(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetItalic(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetUnderline(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetFontColor(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetFontBackground(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetFontSize(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetFontName(oDocument : TWPWorkingDocument; Const sValue : String);
      Procedure SetFontOffset(oDocument : TWPWorkingDocument; Const sValue : String);

      Procedure SetParagraphLeft(Const sValue : String);
      Procedure SetParagraphRight(Const sValue : String);
      Procedure SetParagraphAlignment(Const sValue : String);
      Procedure SetParagraphType(Const sValue : String);
      Procedure SetParagraphNumberFormat(Const sValue : String);

  protected
    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;
      destructor Destroy; Override;

      Procedure Read(oDocument : TWPWorkingDocument); Override;
  End;


{
This unit generates a text stream which is marked up
using HL7 escape sequences for FT where appropriate
(if permitted)

The escape sequences are prepared ready for encoding using
the HL7 V2 library (so the char #1 (HL7V2Cells.INTERNAL_ESCAPE_CHAR))


Todo: does the addition of highlighting screw the table metrics up enough to care about?
}



Type
  TWPHL7FTWriter = Class (TWPTextWriter)
    Private
      FUseHighlighting : Boolean;
      FAllOneField : Boolean;
      FUseFormattedTextEncoding : Boolean;

      FHighlight : Boolean;

      Procedure CheckHighlighting(oPiece : TWPWorkingDocumentPiece);

    Protected
      Procedure WriteText(oText : TWPWorkingDocumentTextPiece); Overload; Override;
      Procedure WriteImage(oImage : TWPWorkingDocumentImagePiece); Overload; Override;

      Procedure WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece); Overload; Override;
      Procedure WriteBreak(oBreak : TWPWorkingDocumentBreakPiece); Overload; Override;
      Procedure WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece); Override;
      Procedure WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;
      Procedure WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean); Overload; Override;
      Procedure WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece); Overload; Override;

      Procedure ConfigureTextWriter(oWriter : TWPTextWriterModelWriter); Overload; Override;

    function sizeInBytesV : cardinal; override;
    Public
      constructor Create; Override;

      Property UseHighlighting : Boolean Read FUseHighlighting Write FUseHighlighting;
      Property AllOneField : Boolean Read FAllOneField Write FAllOneField;
      Property UseFormattedTextEncoding : Boolean Read FUseFormattedTextEncoding Write FUseFormattedTextEncoding;
  End;


Implementation


Const
  ESCAPE_CHARACTER = '\';
  //ESCAPE_CHARACTER = #1;
  INDENT_FACTOR = 1;


Constructor TWPHL7FTReader.Create;
Begin
  Inherited;

  FHL7Font := TWPSFontDetails.Create;
  FHL7Para := TWPSParagraphDetails.Create;
End;


Destructor TWPHL7FTReader.Destroy;
Begin
  FHL7Font.Free;
  FHL7Para.Free;

  Inherited;
End;


Procedure TWPHL7FTReader.AddPara(oDocument : TWPWorkingDocument);
Var
  oPara : TWPWorkingDocumentParaPiece;
Begin
  oPara := TWPWorkingDocumentParaPiece.Create;
  Try
    oPara.SpeechMagicDouble := SpeechMagicDouble;
    If HasStyle Then
      oPara.Style := Style
    Else
      oPara.Style := DEFAULT_STYLE_NAME;
    If HasFont Then
      oPara.Font.Assign(Font)
    Else If Styles.HasDefaultStyle Then
      oPara.Font.Assign(Styles.DefaultStyle.Font)
    Else
      oPara.Font.Defaults;
    oPara.Format.Update(FHL7Para);
    oPara.Font.Update(FHL7Font);
    FHL7Para.Clear;
    FHL7Font.Clear;
    oDocument.Pieces.Add(oPara.Link);
    FLastWasPara := True;
  Finally
    oPara.Free;
  End;

End;

Procedure TWPHL7FTReader.Commit(oDocument : TWPWorkingDocument; Const sWord : String);
Var
  oText : TWPWorkingDocumentTextPiece;
Begin
  oText := TWPWorkingDocumentTextPiece.Create;
  Try
    If HasStyle Then
      oText.Style := Style
    Else
      oText.Style := DEFAULT_STYLE_NAME;
    If HasFont Then
      oText.Font.Assign(Font)
    Else If Styles.HasDefaultStyle Then
      oText.Font.Assign(Styles.DefaultStyle.Font)
    Else
      oText.Font.Defaults;
    oText.Font.Update(FHL7Font);
    oText.Content := sWord;
    oDocument.Pieces.Add(oText.Link);
    FLastWasPara := False;
  Finally
    oText.Free;
  End;
End;


Procedure TWPHL7FTReader.CommitWord(oDocument : TWPWorkingDocument);
Begin
  If FWord <> '' Then
  Begin
    While Length(FWord) > MAX_WORD_LENGTH Do
    Begin
      Commit(oDocument, Copy(FWord, 1, MAX_WORD_LENGTH));
      FWord := Copy(FWord, MAX_WORD_LENGTH + 1, MAXINT);
    End;

    Commit(oDocument, FWord);
    FWord := '';
  End;
End;

Function  TWPHL7FTReader.GetChar(Var ch : Char) : Boolean;
Begin
  Result := FLoop < Length(FText);
  If Result Then
  Begin
    Inc(FLoop);
    ch := FText[FLoop];
  End;
End;


Function TWPHL7FTReader.GetRemainder : String;
Var
  s : String;
Begin
  While (FLoop < Length(FText)) And (FText[FLoop+1] <> ESCAPE_CHARACTER) Do
  Begin
    Inc(FLoop);
    s := s + FText[FLoop];
  End;
   Result := s;
End;


Function TWPHL7FTReader.GetNumber : Integer;
Var
  s : String;
Begin
  s := GetRemainder;
  If (s = '') Or Not StringIsInteger32(s) Then
    Result := 0
  Else
    Result := StringToInteger32(s);
End;


Procedure TWPHL7FTReader.Read(oDocument : TWPWorkingDocument);
Var
  ch : Char;
  oReader : TFslTextExtractor;
Begin
  Inherited;

  If HasFont Then
    FHL7Font.Assign(Font)
  Else
    FHL7Font.Clear;
  FHL7Para.Clear;

  oReader := TFslTextExtractor.Create(Stream.Link);
  Try
    FText := oReader.ConsumeRestStream;
  Finally
    oReader.Free;
  End;

  FLoop := 0;
  FWord := '';
  While GetChar(ch) Do
  Begin
    if ch = ESCAPE_CHARACTER then
       ProcessEscape(oDocument)
    Else if CharInSet(ch, [#13, #10]) Then
    Begin
       CommitWord(oDocument);
       AddPara(oDocument);
       If (FLoop < Length(FText)) And CharInSet(FText[FLoop+1], [#13, #10]) Then
         Inc(FLoop);
    End
    Else if ch = #9 Then
    Begin
      CommitWord(oDocument);
      Commit(oDocument, StringMultiply(' ', TAB_CHAR_COUNT));
    End
    Else If CharInSet(ch, CHAR_WORD_BREAK) Then
    Begin
      CommitWord(oDocument);
      Commit(oDocument, ch);
    End
    Else If Not CharInSet(ch, [#0, #26]) Then
      FWord := FWord + ch;
  End;
  CommitWord(oDocument);
  If Not FLastWasPara Then
    AddPara(oDocument);
  DoneReading(oDocument);
End;


Procedure TWPHL7FTReader.ProcessEscape(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  If GetChar(ch) Then
  Begin
    Case ch Of
      'H' : SetBold(oDocument, '+');
      'N' : SetBold(oDocument, '-');
      '.' : ProcessDot(oDocument);
      'Z' : ProcessLocalExtension(oDocument);
    End;

    While getChar(ch) Do
    Begin
      If (ch = ESCAPE_CHARACTER) Then
        Break;
    End;
  End;
End;

Procedure TWPHL7FTReader.ProcessFontCommand(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  If GetChar(ch) Then
  Begin
    Case ch Of
      'c' : SetFontColor(oDocument, GetRemainder);
      'b' : SetFontBackground(oDocument, GetRemainder);
      's' : SetFontSize(oDocument, GetRemainder);
      'n' : SetFontName(oDocument, GetRemainder);
      'o' : SetFontOffset(oDocument, GetRemainder);
    End;
  End;
End;


Procedure TWPHL7FTReader.ProcessParagraphCommand(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  If GetChar(ch) Then
    Begin
    Case ch Of
      'l' : SetParagraphLeft(getRemainder);
      'r' : SetParagraphRight(getRemainder);
      'a' : SetParagraphAlignment(getRemainder);
      't' : SetParagraphType(getRemainder);
      'n' : SetParagraphNumberFormat(getRemainder);
      'i' :
        Begin
        CommitWord(oDocument);
        AddPara(oDocument);
        End;
    End;
    End;
End;

Procedure TWPHL7FTReader.ProcessLocalExtension(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  If GetChar(ch) Then
  Begin
    Case ch Of
      'i': SetItalic(oDocument, GetRemainder);
      'b': SetBold(oDocument, GetRemainder);
      'u': SetUnderline(oDocument, GetRemainder);
      'f': ProcessFontCommand(oDocument);
      'p': ProcessParagraphCommand(oDocument);
    End;
  End;
End;

Procedure TWPHL7FTReader.ProcessBr(oDocument : TWPWorkingDocument);
Begin
  // .br Begin new output line. Set the horizontal position to the current left margin and increment the vertical position by 1.
  CommitWord(oDocument);
  AddPara(oDocument);
End;

Procedure TWPHL7FTReader.ProcessCe(oDocument : TWPWorkingDocument);
Begin
  // .ce End current output line and center the next line.
  CommitWord(oDocument);
  AddPara(oDocument);
  SetParagraphAlignment('center');
End;

Procedure TWPHL7FTReader.ProcessIn(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  // .in <number> Indent <number> of spaces, where <number> is a positive or negative integer. This command cannot appear after the first printable character of a line.
  If GetChar(ch) Then
  Begin
    SetParagraphLeft(IntegerToString(GetNumber));
  End;
End;

Procedure TWPHL7FTReader.ProcessSk(oDocument : TWPWorkingDocument);
Var
  iLoop : Integer;
Begin
  // .sk < number> Skip <number> spaces to the right.
  CommitWord(oDocument);
  For iLoop := 1 To GetNumber Do
    Commit(oDocument, ' ');
End;

Procedure TWPHL7FTReader.ProcessSp(oDocument : TWPWorkingDocument);
Var
  iLoop : Integer;
Begin
  // .sp <number> End current output line and skip <number> vertical spaces. <number> is a positive
  // integer or absent. If <number> is absent, skip one space. The horizontal character
  // position remains unchanged. Note that only for purposes of compatibility with
  // previous versions of HL7, ^\.sp\ is equivalent to \.br\.

  // this is not really right, but we have no way to not reset the horizontal position
  CommitWord(oDocument);
  AddPara(oDocument);
  For iLoop := 1 To GetNumber Do
    AddPara(oDocument);
End;

Procedure TWPHL7FTReader.ProcessTi;
Var
  ch : Char;
Begin
  // .ti <number> Temporarily indent <number> of spaces where number is a positive or negative integer. This command cannot appear after the first printable character of a line.
  If GetChar(ch) Then
  Begin
    SetParagraphLeft(IntegerToString(GetNumber));
  End;
End;

Procedure TWPHL7FTReader.ProcessDot(oDocument : TWPWorkingDocument);
Var
  ch : Char;
Begin
  If GetChar(ch) Then
  Begin
    Case ch Of
      'b'{r} :  ProcessBr(oDocument);
      'c'{e} : ProcessCe(oDocument);
      'i'{n} : ProcessIn(oDocument);
      's'{k/p} :
        If GetChar(ch) Then
          If (ch = 'k') Then
            ProcessSk(oDocument)
          Else If (ch = 'p') Then
            ProcessSp(oDocument);
      't'{i} : ProcessTi;
    End;
  End;
End;


Procedure TWPHL7FTReader.SetBold(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  aVal : TWPSTriState;
Begin
  If sValue = '+' Then
    aVal := tsTrue
  Else If sValue = '-' Then
    aVal := tsFalse
  Else
    aVal := tsUnknown;
  If aVal <> FHL7Font.Bold Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Bold := aVal;
    End;
End;

Procedure TWPHL7FTReader.SetItalic(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  aVal : TWPSTriState;
Begin
  If sValue = '+' Then
    aVal := tsTrue
  Else If sValue = '-' Then
    aVal := tsFalse
  Else
    aVal := tsUnknown;
  If aVal <> FHL7Font.Italic Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Italic := aVal;
    End;
End;

Procedure TWPHL7FTReader.SetUnderline(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  aVal : TWPSTriState;
Begin
  If sValue = '+' Then
    aVal := tsTrue
  Else If sValue = '-' Then
    aVal := tsFalse
  Else
    aVal := tsUnknown;
  If aVal <> FHL7Font.Underline Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Underline := aVal;
    End;
End;

Procedure TWPHL7FTReader.SetFontColor(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  iVal : TColour;
Begin
  If Not StringIsHTMLColour(sValue) Then
    iVal := DEF_COLOUR
  Else
    iVal := HTMLColourStringToColour(sValue);
  If iVal <> FHL7Font.Foreground Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Foreground := iVal;
    End;
End;

Procedure TWPHL7FTReader.SetFontBackground(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  iVal : TColour;
Begin
  If Not StringIsHTMLColour(sValue) Then
    iVal := DEF_COLOUR
  Else
    iVal := HTMLColourStringToColour(sValue);
  If iVal <> FHL7Font.Background Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Background := iVal;
    End;
End;

Procedure TWPHL7FTReader.SetFontSize(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  iVal : Integer;
Begin
  If (sValue = '') Or Not (StringIsInteger32(sValue) Or  ((sValue[1] = '+') Or (sValue[1] = '-')) And StringIsInteger32(Copy(sValue, 2, $FF))) Then
    iVal := DEF_WORD
  Else If (sValue[1] = '+') Then
    If FHL7Font.Size <> DEF_WORD Then
      iVal := FHL7Font.Size + StringToInteger32(Copy(sValue, 2, $FF))
    Else If HasFont And (Font.Size <> DEF_WORD) Then
      iVal := Font.Size + StringToInteger32(Copy(sValue, 2, $FF))
    Else
      iVal := DEFAULT_FONT_SIZE + StringToInteger32(Copy(sValue, 2, $FF))
  Else If (sValue[1] = '-') Then
    If FHL7Font.Size <> DEF_WORD Then
      iVal := FHL7Font.Size - StringToInteger32(Copy(sValue, 2, $FF))
    Else If HasFont And (Font.Size <> DEF_WORD) Then
      iVal := Font.Size - StringToInteger32(Copy(sValue, 2, $FF))
    Else
      iVal := DEFAULT_FONT_SIZE - StringToInteger32(Copy(sValue, 2, $FF))
  Else
    iVal := StringToInteger32(sValue);
  If (iVal <> FHL7Font.Size) Then
    Begin
    CommitWord(oDocument);
    FHL7Font.Size := iVal;
    End;
End;

Procedure TWPHL7FTReader.SetFontName(oDocument : TWPWorkingDocument; Const sValue : String);
Begin
  If sValue <> FHL7Font.Name Then
  Begin
    CommitWord(oDocument);
    FHL7Font.Name := sValue;
  End;
End;

Procedure TWPHL7FTReader.SetFontOffset(oDocument : TWPWorkingDocument; Const sValue : String);
Var
  aValue : TWPSFontState;
  sWorkingValue : String;
Begin
  sWorkingValue := Lowercase(sValue);

  If (sWorkingValue = 'super') Then
    aValue := fsSuperscript
  Else If (sWorkingValue = 'sub') Then
    aValue := fsSubscript
  Else
    aValue := fsNormal;
  If (FHL7Font.State <> aValue) Then
  Begin
    CommitWord(oDocument);
    FHL7Font.State := aValue;
  End;
End;

Procedure TWPHL7FTReader.SetParagraphLeft(Const sValue : String);
Begin
  If StringIsInteger32(sValue) Then
    FHL7Para.LeftIndent := StringToInteger32(sValue)
  Else
    FHL7Para.LeftIndent := DEF_INT;
End;

Procedure TWPHL7FTReader.SetParagraphRight(Const sValue : String);
Begin
  If StringIsInteger32(sValue) Then
    FHL7Para.RightIndent := StringToInteger32(sValue)
  Else
    FHL7Para.RightIndent := DEF_INT;
End;

Procedure TWPHL7FTReader.SetParagraphAlignment(Const sValue : String);
Var
  sWorkingValue : String;
Begin
  sWorkingValue := Lowercase(sValue);
  If (sWorkingValue = 'left') Or (sWorkingValue = 'l') Then
    FHL7Para.AlignLeft
  Else If (sWorkingValue = 'right') Or (sWorkingValue = 'r') Then
    FHL7Para.AlignRight
  Else If (sWorkingValue = 'centre') Or (sWorkingValue = 'center') Or (sWorkingValue = 'c') Then
    FHL7Para.AlignCentre
  Else If (sWorkingValue = 'justify') Or (sWorkingValue = 'justified') Or (sWorkingValue = 'j') Then
    FHL7Para.AlignJustify
  Else
    FHL7Para.AlignNone;
End;

Procedure TWPHL7FTReader.SetParagraphType(Const sValue : String);
Var
  sWorkingValue : String;
Begin
  sWorkingValue := Lowercase(sValue);
  If (sWorkingValue = 'disc') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeBullets;
    FHL7Para.BulletType := tbDisc;
    End
  Else If (sWorkingValue = 'circle') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeBullets;
    FHL7Para.BulletType := tbCircle;
    End
  Else If (sWorkingValue = 'square') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeBullets;
    FHL7Para.BulletType := tbSquare;
    End
  Else If (sWorkingValue = 'arabic') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeNumbers;
    FHL7Para.NumberType := tnArabic;
    End
  Else If (sWorkingValue = 'alphalow') Or (sWorkingValue = 'alpha') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeNumbers;
    FHL7Para.NumberType := tnLowerAlpha;
    End
  Else If (sWorkingValue = 'alphaup') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeNumbers;
    FHL7Para.NumberType := tnUpperAlpha;
    End
  Else If (sWorkingValue = 'romanlow') Or (sWorkingValue = 'roman') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeNumbers;
    FHL7Para.NumberType := tnLowerRoman;
    End
  Else If (sWorkingValue = 'romanup') Then
    Begin
    FHL7Para.ListType := WPSParagraphListTypeNumbers;
    FHL7Para.NumberType := tnUpperRoman;
    End
  Else
    FHL7Para.ListType := WPSParagraphListTypeNone;
End;


Procedure TWPHL7FTReader.SetParagraphNumberFormat(Const sValue : String);
Var
  sWorkingValue : String;
Begin
  sWorkingValue := Lowercase(sValue);
  If sWorkingValue = 'dot' Then
    FHL7Para.NumberFormat := nwDot
  Else If sWorkingValue = 'slash' Then
    FHL7Para.NumberFormat := nwSlash
  Else If sWorkingValue = 'parenthesis' Then
    FHL7Para.NumberFormat := nwParenthesis
  Else If sWorkingValue = 'colon' Then
    FHL7Para.NumberFormat := nwColon
  Else If sWorkingValue = 'semicolon' Then
    FHL7Para.NumberFormat := nwSemiColon
  Else
    FHL7Para.NumberFormat := nwNone;
End;



Const
  // #1 for \ because of how the HL7 library works
  HIGHLIGHT_CODE_ON = #1+'H'+#1;
  HIGHLIGHT_CODE_OFF = #1+'N'+#1;

function TWPHL7FTReader.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FHL7Font.sizeInBytes);
  inc(result, FHL7Para.sizeInBytes);
  inc(result, (FText.length * sizeof(char)) + 12);
  inc(result, (FWord.length * sizeof(char)) + 12);
end;

Constructor TWPHL7FTWriter.Create;
Begin
  Inherited;
  UseHighlighting := True;
End;


Procedure TWPHL7FTWriter.ConfigureTextWriter(oWriter : TWPTextWriterModelWriter);
Begin
  If FAllOneField Or FUseFormattedTextEncoding Then
    oWriter.Eoln := #1 + '.br' + #1;       // tricky - should be escape or internal escape? - discuss with Grahame if changing
End;


Function FontBigger(iSize, iCriteria : Integer): Boolean;
Begin
  Result := (iSize = DEF_WORD) Or (iSize >= iCriteria);
End;


Procedure TWPHL7FTWriter.CheckHighlighting(oPiece : TWPWorkingDocumentPiece);
Var
  bHighlighted : Boolean;
Begin
  bHighlighted := Assigned(oPiece) And ((oPiece.Font.Bold = tsTrue) Or ((oPiece.Font.Italic = tsTrue) And (FontBigger(oPiece.Font.Size, 10))) Or (oPiece.Font.Underline = tsTrue));
  If FUseHighlighting And (bHighlighted <> FHighlight) Then
  Begin
    FHighlight := Not FHighlight;
    If FHighLight Then
    Begin
      FFormatBefore := HIGHLIGHT_CODE_ON;
      FFormatAfter := HIGHLIGHT_CODE_OFF;
    End
    Else
    Begin
      FFormatBefore := '';
      FFormatAfter := '';
    End;
  End;
End;


Procedure TWPHL7FTWriter.WriteText(oText : TWPWorkingDocumentTextPiece);
Begin
  CheckHighlighting(oText);
  Inherited WriteText(oText);
End;


Procedure TWPHL7FTWriter.WriteImage(oImage : TWPWorkingDocumentImagePiece);
Begin
  CheckHighlighting(oImage);
  Inherited WriteImage(oImage);
End;


Procedure TWPHL7FTWriter.WriteParagraphStop(oParagraph : TWPWorkingDocumentParaPiece; bNextIsSection : Boolean; oSection : TWPWorkingDocumentSectionStartPiece);
Begin
  CheckHighlighting(Nil);
  Inherited WriteParagraphStop(oParagraph, bNextIsSection, oSection);
End;


Procedure TWPHL7FTWriter.WriteBreak(oBreak : TWPWorkingDocumentBreakPiece);
Begin
  CheckHighlighting(Nil);
  Inherited WriteBreak(oBreak);
End;


Procedure TWPHL7FTWriter.WriteLineBreak(oBreak : TWPWorkingDocumentLineBreakPiece);
Begin
  If FAllOneField Or FUseFormattedTextEncoding Then
    AddText('', true)
  Else
    Inherited WriteLineBreak(oBreak);
End;


Procedure TWPHL7FTWriter.WriteTableStop(oTable : TWPWorkingDocumentTableStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckHighlighting(Nil);
  Inherited WriteTableStop(oTable, oStop);
End;


Procedure TWPHL7FTWriter.WriteTableRowStop(oTableRow : TWPWorkingDocumentTableRowStartPiece; oStop : TWPWorkingDocumentStopPiece; bIsLast : Boolean);
Begin
  CheckHighlighting(Nil);
  Inherited WriteTableRowStop(oTableRow, oStop, bIsLast);
End;


Procedure TWPHL7FTWriter.WriteTableCellStop(oTableCell : TWPWorkingDocumentTableCellStartPiece; oStop : TWPWorkingDocumentStopPiece);
Begin
  CheckHighlighting(Nil);
  Inherited WriteTableCellStop(oTableCell, oStop);
End;



function TWPHL7FTWriter.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
end;

End.


