Unit FHIR.WP.AnnotationDefiners;

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
  Windows, Graphics,
  fsl_utilities,
  FHIR.WP.Control, wp_types, wp_document, wp_definers, FHIR.WP.Snomed, FHIR.WP.Icons;

Type
  TWPAnnotationLinkedDefinitionProvider = Class (TWPAnnotationDefinitionProvider)
    Private
      FWordProcessor : TWordProcessor;
    Public
      constructor Create(oWordProcessor : TWordProcessor); Overload; Virtual;
      Property WordProcessor : TWordProcessor Read FWordProcessor Write FWordProcessor;
  End;

Const
  NS_ANNOTATION_SNOMED = 'libraries\publishing\wordprocessor\snomed';

Type
  TWPSnomedDefinitionProvider = Class (TWPAnnotationLinkedDefinitionProvider)
  private
    FClient : TSnomedClient;
    procedure SetClient(const Value: TSnomedClient);

  Public
    constructor Create; Overload; Override;
    destructor Destroy; Override;


    Function GetNamespace : String; Overload; Override;
    Function GetTitle : String; Overload; Override;
    Function GetIconIndex : Integer; Overload; Override;

    Function IsInsertKey(iKey: Word; Const aShift: TWPShiftStates; sText : String) : Boolean; Overload; Override;

    Function EditAnnotation(sText : String; var sAnnotation : String) : TEditAnnotationResult; Overload; Override;
    Function GetColour(sAnnotation : String) : TColour; Overload; Override;
    Function GetDisplay(sAnnotation : String) : String; Overload; Override;
    Function FontSize : Integer; Overload; Override;

    Property Client : TSnomedClient read FClient write SetClient;
  End;

Const
  NS_ANNOTATION_COMMENT = 'libraries\publishing\wordprocessor\comment';

type
  TWPCommentDefinitionProvider = Class (TWPAnnotationLinkedDefinitionProvider)
    Public
      Function GetNamespace : String; Overload; Override;
      Function GetTitle : String; Overload; Override;
      Function GetIconIndex : Integer; Overload; Override;

      Function EditAnnotation(sText : String; var sAnnotation : String) : TEditAnnotationResult; Overload; Override;
      Function GetColour(sAnnotation : String) : TColour; Overload; Override;
  End;

Implementation

Uses
  FHIR.WP.Dialogs;

{ TWPAnnotationLinkedDefinitionProvider }

Constructor TWPAnnotationLinkedDefinitionProvider.Create(oWordProcessor: TWordProcessor);
Begin
  Create;
  FWordProcessor := oWordProcessor;
End;





{ TWPSnomedDefinitionProvider }


function TWPSnomedDefinitionProvider.GetColour(sAnnotation: String): TColour;
begin
  result := HTML_COLOUR_VALUES[hcGoldenrod];

end;

Function TWPSnomedDefinitionProvider.GetIconIndex: Integer;
Begin
  Result := WPIconModule.SNOMED;
End;


Function TWPSnomedDefinitionProvider.GetNamespace: String;
Begin
  Result := NS_ANNOTATION_SNOMED;
End;


Function TWPSnomedDefinitionProvider.GetTitle: String;
Begin
  Result := 'Snomed Code';
End;


Function TWPSnomedDefinitionProvider.EditAnnotation(sText : String; var sAnnotation : String) : TEditAnnotationResult;
var
  oDlg : TSnomedPicker;
Begin
  oDlg := TSnomedPicker.Create(WordProcessor);
  Try
    oDlg.Client := Client.Link;
    oDlg.Term := sAnnotation;
    oDlg.Text := sText;
    if oDlg.Execute then
      result := earChanged
    else
      result := earUnChanged;
    sAnnotation := oDlg.Term;
  Finally
    oDlg.Free;
  End;
end;

function TWPSnomedDefinitionProvider.GetDisplay(sAnnotation: String): String;
begin
  result := sAnnotation+': ';//FClient.GetSnomedDisplayName(sAnnotation);
end;


constructor TWPSnomedDefinitionProvider.Create;
begin
  inherited Create;
end;

destructor TWPSnomedDefinitionProvider.Destroy;
begin
  FClient.Free;
  inherited;
end;

function TWPSnomedDefinitionProvider.FontSize: Integer;
begin
  result := 7;
end;

procedure TWPSnomedDefinitionProvider.SetClient(const Value: TSnomedClient);
begin
  FClient.Free;
  FClient := Value;
end;

function TWPSnomedDefinitionProvider.IsInsertKey(iKey: Word; const aShift: TWPShiftStates; sText: String): Boolean;
begin
  result := iKey = VK_F4;
end;

{ TWPCommentDefinitionProvider }


function TWPCommentDefinitionProvider.GetColour(sAnnotation: String): TColour;
begin
  result := clNavy;

end;

Function TWPCommentDefinitionProvider.GetIconIndex: Integer;
Begin
  Result := WPIconModule.INSERT_COMMENT;
End;


Function TWPCommentDefinitionProvider.GetNamespace: String;
Begin
  Result := NS_ANNOTATION_COMMENT;
End;


Function TWPCommentDefinitionProvider.GetTitle: String;
Begin
  Result := 'Comment';
End;


Function TWPCommentDefinitionProvider.EditAnnotation(sText : String; var sAnnotation : String) : TEditAnnotationResult;
Var
  oDialog : TWPCommentDialog;
Begin
  oDialog := TWPCommentDialog.Create(WordProcessor);
  Try
    oDialog.Comment := sAnnotation;
    if oDialog.Execute then
    begin
      if oDialog.WantDelete then
        result := earDelete
      else
        result := earChanged;
      sAnnotation := oDialog.Comment;
    end
    else
      result := earUnchanged;
  Finally
    oDialog.Free;
  End;
end;


End.
