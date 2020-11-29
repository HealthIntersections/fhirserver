Unit FHIR.WP.Spelling;

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

Interface

Uses
   fsl_collections,
  FHIR.WP.Settings, wp_types, wp_working;

Type
  EWordIsBanned = Class (EWPException);
  TWordCheckType = (wcAccepted, wcExcluded, wcCorrected, wcDenied, wcRepeated);
  TWordCheckEvent = Procedure (Sender:TObject; Const Word:String; Var CheckType : TWordCheckType; Var Replacement:String) Of Object;
  TAllowWordEvent = Procedure (Sender:TObject; Const Word:String) Of Object;

  TWPSpeller = Class (TWPSettable)
  Private
    FAllowedWords : TFslStringList;
    procedure SetAllowedWords(const Value: TFslStringList);
  protected
    function sizeInBytesV : cardinal; override;
  Public
    constructor Create; Override;
    destructor Destroy; Override;
    Function Link : TWPSpeller; Overload;

    // for a word, check it's state (should be quick, called constantly
    Function CheckSpelling(Const sWord : String) : TWPWorkingDocumentSpellCheckingState; Overload; Virtual;

    // for a word, make a list of alternate suggestions
    Procedure ListSuggestions(Const sWord : String; oList : TFslStringList); Overload; Virtual;

    // add the word to the custom dictionary
    Function AddWord(Const sWord : String) : Boolean; Overload; Virtual;

    // ignore the word - persists in document
    Function IgnoreWord(Const sWord : String) : Boolean; Overload; Virtual;

    // work through the contents of the WP doing a spell check interactively
    Procedure Check(oWP : TObject; aIgnoreCheck : TWordCheckEvent; aAllowWord : TAllowWordEvent); Overload; Virtual;

    Property AllowedWords : TFslStringList read FAllowedWords write SetAllowedWords;
  End;

Implementation

{ TWPSpeller }

Function TWPSpeller.AddWord(Const sWord: String): Boolean;
Begin
  Result := False;
End;

Function TWPSpeller.CheckSpelling(Const sWord: String): TWPWorkingDocumentSpellCheckingState;
Begin
  Result := scsExempt;
End;

Function TWPSpeller.IgnoreWord(Const sWord: String): Boolean;
Begin
  Result := False;
End;

Function TWPSpeller.Link: TWPSpeller;
Begin
  Result := TWPSpeller(Inherited Link);
End;

Procedure TWPSpeller.ListSuggestions(Const sWord: String; oList: TFslStringList);
Begin

End;

Procedure TWPSpeller.Check(oWP : TObject; aIgnoreCheck : TWordCheckEvent; aAllowWord : TAllowWordEvent);
Begin
  raiseError('Check', 'Interactive Spell Checking is not provided by this Spelling provider');
End;



constructor TWPSpeller.Create;
begin
  inherited;
  FAllowedWords := TFslStringList.Create;
end;

destructor TWPSpeller.Destroy;
begin
  FAllowedWords.Free;
  inherited;
end;

procedure TWPSpeller.SetAllowedWords(const Value: TFslStringList);
begin
  FAllowedWords.Free;
  FAllowedWords := Value;
end;

function TWPSpeller.sizeInBytesV : cardinal;
begin
  result := inherited sizeInBytesV;
  inc(result, FAllowedWords.sizeInBytes);
end;

End.
